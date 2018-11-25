;; -*- lexical-binding: t; -*-

(require 'cl)

;; TODO parsing all those defun args has become very tedious. I almost wish I
;; didn't insist on optional keyword args so much. Bulk of the code here does this
;; nonsence. Should drop this and go with the flow, or does Elisp has some hidden
;; way of dealing with them? Or maybe my patterns are repetitive enough that I
;; could abstract them into a macro?

;; TODO Using list as a set is dumb and f-ing slow for membership lookup. Could I
;; just fake a set as a hash-table {:member t, ...}? That would speed up
;; membership lookup in methods, hierarchies, prefers.

;; TODO Instead of poluting symbol slots maybe I should have a multi struct. This
;; may make code quite a bit cleaner
;;
;;   (defstruct multi
;;     (methods (ht))
;;     default
;;     dispatch)
;;
;; I'll have to overload multi-methods accessor function so it can do what current
;; `multi-methods' can do. One interesting aspect is that the value of foo then
;; becomes a struct and predicate (multi-p foo) works as expected, this however
;; necessitates passing quoted foo where expected, but we could work around this
;; by adding an extra :name or :id or :symbol slot, what would carry 'foo.

;; TODO dispatch cache

;; TODO hierarchy cache

;; TODO Make `multi-test' available here to be used in comments and examples

;; TODO Create Makefile (stick to ANSI make): ert batch test, measure perf

;; TODO Consider storing hierarchies the way Clojure does it. IMO benefit is that
;; descendants are precalculated. Anything else?
;;
;;   {:parents     {:rect #{:shape}}
;;    :ancestors   {:rect #{shape}}
;;    :descendants {:shape #{rect}}}
;;
;; Ok, so isa? test becomes as trivial as (member VAL (multi-ancestors val))
;; unless you want to know the exact lineage between VAL and val. Performance wise
;; atm I pay the price of traversing hierarchy every time isa? is called, with
;; ancestors the price is paid upfront to calculate the ancestors! If you ever
;; need lineage, compute it from :parents :children as needed.
;;
;; Aha, this is very clever! Both multi-prefer :over and multi-isa :isa relations
;; would benefit from storing descendants and ancestors. Both have essentially the
;; above structure and can be thought of in terms of :parents :ancestors
;; :descendants. Access to :descendants or :ancestors makes checking for cycle
;; trivial e.g. for prefers:
;;
;; Essentially installing an a-prefer amounts to these relationships:
;; #{a's descendants} :over a :over #{a's ancestors}
;;
;; Registering b :over c would require a simple check:
;;
;;   not (c :over b)
;;
;; or equivalently
;;
;;   not (b is c's ancestor)
;;   not (c is b's descendant)
;;
;; or in our notation
;;
;;   (not (member b (multi-prefers fun hierarchy c :ancestors)))
;;   (not (member c (multi-prefers fun hierarchy b :descendants)))
;;
;; since ancestor and descendant relations imply one another, just keeping track
;; of :ancestors and having just one of the above checks should suffice. So with a
;; bit of redundancy our structs should be something like:
;;
;; Prefers:
;; (ht
;;  ;; relation
;;  (:over (set))
;;  ;; ancestors
;;  (:transitive-closure (set)))
;;
;; Hierarchy:
;; (ht
;;  ;; relation
;;  (:isa (set))
;;  ;; ancestors
;;  (:transitive-closure (set)))
;;
;; Not only are both structures very similar, but their supporting code is almost
;; the same:
;;
;; `multi-hierarchy' = `multi-prefers',
;; `multi-rel'       = `multi-prefer'
;;
;; So, perhaps its worth abstracting `multi-rel' to work with any relationship we
;; care to define?
;;
;; The right data-structure does make code simpler!
;;
;; Notice tha multimethod defines a relation, too. Observe:
;;
;; (multimethod foo :when [a b c] :then #'method)
;;
;; [a b c] :then  method
;; method  :when  [a b c]
;;
;; Not immediately clear if its useful, since VAL and METHOD aren't of the same
;; domain, there isn't an obvious transitive closure to compute. Note, that isa?
;; is a one-to-many relation, while :then is a many-to-one.
;;
;; but also potential optimization is to blow up [a b c] there int a hash-table
;; [(descendants a) x (descendants b) x (descendants c)]. While computing keys in
;; that hash-table if we ever arrive at the same key we need to use multi-prefers
;; to disambiguate (choose a single method). All this "at compile time" that is
;; before we ever run any code that makes use of defined multimethods. Basically,
;; we trade memory footprint for potentially zero-cost dispatch (one hash-table
;; lookup). If you think about it, its exactly like caching an isa? lookup but the
;; cost of the latter is amortized over the lifetime of the code that uses
;; multimethods. Caching is "being lazy", hash-table blow up is "aot compilation".
;;
;; Pre-computing :then and :when relations feels static, don't forget that we are
;; in a dynamic language, so new methods could be added any time, so can new isa?
;; relations. I'd rather have them as "partial" relations, that is only between
;; VALs and METHODs as defined by the user. Then figuring out what VAL the
;; dispatch value isa? at dispatch time and caching that.


;; Extras
;; --------

;; TODO Think about reasonable and practical global-hierarchy, e.g. one that works
;; for structs, isa relationship between predicates, maybe even eieio classes
;; although I have no experienc with those.

;; TODO Allow isa? with "_" patterns

;; TODO Allow predicates in patterns

;; TODO Hierarchy is orthogonal to `multi' dispatch function. However in Clojure
;; you may change it (only?) in `defmulti', but IMO it makes more sence to be able
;; to pass it to multimethod invocations (not even definitions). Need to think if
;; that'd be consistent and whether it has any practical value.

;; TODO Could we allow arbitrary relations? E.g. `parent-of'. Would that have any
;; practical benefit? When? How?

;; TODO Should I overload (multi-rel x relates-to? x) to be used as predicate:
;; (if (multi-rel y parent-of? x) do stuff) or define (multi-rel? ...)?

;; TODO How hard would it be to add body from (example foo body) forms to the
;; docstring of 'foo? Might be worth implementing something like:
;;
;;   (defun/example foo (&rest args)
;;     :doc "docstring"
;;     :example (example that runs whenever defun is evaled
;;                       and its stringified copy appended to docstring)
;;     body)


;;* Errors -------------------------------------------------------- *;;


(define-error 'multi-error "multi-error")


(defun multi-error (&rest args)
  "Signals errors specific to `multi' library. Can be caught with
'multi-error ERROR-SYMBOL in `condition-case', otherwise behaves
exactly like `error'

\(fn string &rest args)"
  (signal 'multi-error (list (apply #'format-message args))))


;;* State  -------------------------------------------------------- *;;


(defcustom multi-lexical-binding 'error
  "Control if multimethods can be defined when `lexical-binding'
 is disabled. Default to signaling an error if an attempt is made
 to define a new multi dispatch or method while in a dynamically
 scoped environment.")


(defun multi-lexical-binding ()
  "Signal an error depending on the setting of
`multi-lexical-binding' and `lexical-binding'."
  (when multi-lexical-binding
    (unless lexical-binding
      (multi-error
       (string-join
        (list
         "multimethods require `lexical-binding' to work properly."
         "If you know what you are doing you may disable this check"
         "by unsetting `multi-lexical-binding'.")
        " ")))))


(defstruct multi-hierarchy
  ;; TODO should this be an UUID or gensym is enough to avoid collisions?
  (id (gensym "multi-hierarchy"))
  (table (ht))
  (cache (ht)))


(defun multi-hierarchy (hierarchy &rest keys)
  "Returns the value in the HIERARCHY's nested table, where KEYS
is a sequence of keys. Returns nil if the key is not present.
Without KEYS returns the entire table. Calls are `setf'-able."
  (declare
   (gv-setter (lambda (val)
                `(setf (ht-get* (multi-hierarchy-table ,hierarchy) ,@keys) ,val))))
  (if keys
      (apply #'ht-get* (multi-hierarchy-table hierarchy) keys)
    (multi-hierarchy-table hierarchy)))


(defconst multi-global-hierarchy (make-multi-hierarchy)
  "Global table that holds global hierachy. Has the following
structure:

  (ht (VAL (ht (:parents (p ...)) (:children (c ...)))) ...)")


(defun multi-global-hierarchy (&rest keys)
  "Returns the value in the global hierarchy nested table, where
KEYS is a sequence of keys. Returns nil if the key is not
present. Without KEYS returns the entire table. Calls are
`setf'-able."
  (declare
   (gv-setter (lambda (val)
                `(setf (multi-hierarchy multi-global-hierarchy ,@keys) ,val))))
  (apply #'multi-hierarchy multi-global-hierarchy keys))


(defun multi--methods (fun-symbol &rest keys)
  "Returns the multi-methods hash-table of FUN-SYMBOL or the
value nested in that table if the sequence of KEYS is supplied.
This form is `setf'-able."
  (declare
   (gv-setter (lambda (val)
                (if keys
                    ;; with keys set corresponding value in :multi-methods table
                    `(setf (ht-get* (get ,fun-symbol :multi-methods) ,@keys) ,val)
                  ;; without keys set :multi-methods prop itself to a new table
                  `(setf (get ,fun-symbol :multi-methods) ,val)))))
  (if keys
      (apply #'ht-get* (get fun-symbol :multi-methods) keys)
    (get fun-symbol :multi-methods)))


;; TODO the (multi-methods 'fun &rest keys) interface suggests an interesting
;; feature. We could go a bit further than Clojure and allow :before, :after,
;; :arround methods, so the multi-methods table doesn't just maps an isa? pattern
;; to a method but potentionally to a map of methods:
;;
;; (ht (:before #'before-fun)
;;     (:after #'after-fun)
;;     (:main #'main-fun))
;;
;; Can I come up with interesting semantics?


(defun multi-methods (&rest args)
  "Returns a hash-table of FUN-SYM multimethods, where each entry
is a (value . method) pair.

When called with :for and :matching keywords limits the table to
pairs where (multi-isa? VAL value) relationship holds or
the (:default . method) if none match. If HIERARCHY not supplied
defaults to the global hierarchy.

When called without :for and :matching keywords returns the
entire table or the value nested in it if the sequence of KEYS is
supplied. This form is `setf'-able.

May be called according to one of the following signatures:

  (multi-methods :for fun-sym :matching val &optional :in hierarchy)
  (multi-methods fun-sym &rest keys)
  (setf (multi-methods fun-sym &rest keys) val)

\(fn :for fun-sym :matching val &optional :in hierarchy)"
  (declare
   (gv-setter (lambda (val)
                (pcase args

                  ;; (setf (multi-methods 'foo &rest keys) val)
                  (`((quote ,(multi fun-symbol :if symbolp)) . ,keys)
                   `(setf (multi--methods ',fun-symbol ,@keys) ,val))

                  ;; (setf (multi-methods foo &rest keys) val)
                  (`(,(multi fun-symbol :if symbolp) . ,keys)
                   `(setf (multi--methods ,fun-symbol ,@keys) ,val))

                  (otherwise
                   `(multi-error "in multi-methods malformed arglist at %s" ',args))))))
  ;; parse args
  (pcase args
    ;; (multi-methods :for 'fun :matching val)
    (`(:for ,fun :matching ,val)
     (multi-methods :for fun :matching val :in multi-global-hierarchy))

    ;; (multi-methods :for 'fun :matching val :in hierarchy)
    (`(:for ,fun :matching ,val :in ,hierarchy)
     (let* ((multi-methods
             (get fun :multi-methods))

            (methods
             (ht-select (fn (VAL _) (multi-isa? val VAL hierarchy)) multi-methods))

            (methods
             (unless (ht-empty? methods) methods))

            (default-method
              (unless methods
                (ht
                 ;; Use custom :default if installed, fallback to the
                 ;; pre-installed default method otherwise
                 (:default (or (ht-get* multi-methods :default)
                               (get fun :multi-default)))))))
       (or methods default-method)))

    ;; (multi-methods 'fun :rect)
    (`(,(multi fun-symbol :if symbolp) . ,keys)
     (apply #'multi--methods fun-symbol keys))

    (otherwise
     (multi-error "in multi-methods malformed arglist at %s" args))))


;; NOTE this naming `multi-methods-remove' becomes more consistent if or when we
;; allow :before :after :around methods
(defun multi-methods-remove (fun dispatch-value)
  "Removes the multimethod FUN associated with DISPATCH-VALUE."
  (ht-remove! (multi-methods fun) dispatch-value))


;; TODO make hierarchy optional defaulting to the global hierarchy
(defun multi-prefers (fun hierarchy &rest keys)
  "Returns a table of (preferred value :over set of other values)
in the hierarchy. If VAL supplied returns just tha set of other
values over which VAL is preferred. This form is `setf'-able.

\(multi-prefers fun hierarchy &optional val)"
  (declare
   (gv-setter (lambda (val)
                (if (cdr keys)
                    `(multi-error "in multi-prefers malformed arglist expected no more than one key, given %s" ',keys)
                  `(setf (ht-get* (get ,fun :multi-prefers) (multi-hierarchy-id ,hierarchy) ,@keys) ,val)))))
  (if (cdr keys)
      ;; expect no more than one argument (VAL to prefer over others)
      (multi-error "in multi-prefers malformed arglist expected no more than one key, given %s" keys)
    ;; return list of values over which (car keys) is preferred in hierarchy, or
    ;; the entire prefers table for hierarchy if no keys supplied
    (apply #'ht-get* (get fun :multi-prefers) (multi-hierarchy-id hierarchy) keys)))


;; TODO is this code correct?
(defun multi--preference-cycle? (item parent fun &optional hierarchy)
  "Checks if ITEM prefer over PARENT would form a cycle were the
relationship added the multi-prefers of FUN assuming HIERARCHY
and return that cycle. If HIERARCHY not supplied defaults to the
global hierarchy"
  (default hierarchy :to multi-global-hierarchy)
  (if (equal item parent)
      (list parent)
    (when-let ((cycle (some
                       (lambda (ancestor) (multi--preference-cycle? item ancestor fun hierarchy))
                       (multi-prefers fun hierarchy parent))))
      (cons parent cycle))))


(comment
 (multi-test (baz)
   (multi baz #'identity)
   (multi-prefer 'baz [:rect :shape] :over [:shape :rect])
   (multi-prefer 'baz [:shape :rect] :over [:parallelogram :rect])
   ;; (multi-prefer 'baz [:parallelogram :rect] :over [:rect :shape])
   (multi--preference-cycle? [:parallelogram :rect] [:rect :shape] 'baz multi-global-hierarchy))
 ;; comment
 )


(defun multi-prefer (fun &rest args)
  "Causes the multimethod FUN to prefer matches of dispatch VAL-X
over dispatch VAL-Y when there is a conflict.

May be called according to one of the following signatures:

  (multi-prefer foo val-x :to val-y &optional :in hierarchy)
  (multi-prefer foo val-x val-y &optional :in hierarchy)

\(multi-prefer foo val-x :over val-y &optional :in hierarchy)"
  (destructuring-bind
      (x y hierarchy)
      (pcase args

        ;; (multi-prefer foo x :over y :in hierarchy)
        (`(,x ,(or :over :to) ,y . ,(or `(:in ,hierarchy) '()))
         (list x y (or hierarchy multi-global-hierarchy)))

        ;; (multi-prefer foo x y :in hierarchy)
        (`(,x ,y . ,(or `(:in ,hierarchy) '()))
         (list x y (or hierarchy multi-global-hierarchy)))

        (otherwise
         (multi-error "in multi-prefer malformed arglist at %s" args)))

    ;; installing the preference mustn't create a cycle in multi-prefers
    (when-let ((cycle (multi--preference-cycle? x y fun hierarchy)))
      (multi-error "in multi-prefer cyclic preference %s over %s would form a cycle %s"
                   x y cycle))

    ;; install the preference x :over y
    (pushnew y (multi-prefers fun hierarchy x))))


;; TODO We don't really need to implement `multi-prefers-remove' because we've
;; already made `multi-prefers' `setf'-able, so the following should work:
;;
;; (setf (multi-prefers foo hierachy [:rect :shape]) '(some-values)
;; (ht-remove! (multi-prefers foo hierachy) [:rect :shape])
;;
;; Does providing `multi-prefers-remove' make things more consistent? I should
;; make a note about removing prefers in documentation regardless.


(defun multi-prefers-remove (fun &rest args)
  "Causes the multimethod FUN to not prefer matches of dispatch
VAL-X over dispatch VAL-Y when there is a conflict. If VAL-Y not
supplied removes all prefers for VAL-X. If HIERARCHY not supplied
defaults to the global hierarchy.

May be called according to one of the following signatures:

  (multi-prefers-remove foo val-x :to val-y &optional :in hierarchy)
  (multi-prefers-remove foo val-x val-y &optional :in hierarchy)
  (multi-prefers-remove foo val-x &optional :in hierarchy)
  (multi-prefers-remove foo &optional :in hierarchy)

\(multi-prefers-remove foo val-x :over val-y &optional :in hierarchy)"
  (pcase args

    ;; TODO fml matching order here matters! If we reorder earlier cases may fire.
    ;; This adds complexity that I don't like. Do I really want to offer this
    ;; calling freedom?

    ;; (multi-prefers-remove foo &optional :in hierarchy)
    ((or `(:in ,hierarchy) `())
     (setf (multi-prefers fun (or hierarchy multi-global-hierarchy)) (ht)))

    ;; (multi-prefers-remove foo x :over y &optional :in hierarchy)
    (`(,x ,(or :over :to) ,y . ,(or `(:in ,hierarchy) '()))
     ;; remove just the Y value from X's prefers
     (cl-callf2 remove y (multi-prefers fun (or hierarchy multi-global-hierarchy) x)))

    ;; (multi-prefers-remove foo x &optional :in hierarchy)
    (`(,x . ,(or `(:in ,hierarchy) '()))
     ;; remove all prefers for X
     (ht-remove! (multi-prefers fun (or hierarchy multi-global-hierarchy)) x))

    ;; (multi-prefers-remove foo x y &optional :in hierarchy)
    (`(,x ,y . ,(or `(:in ,hierarchy) '()))
     ;; remove just the Y value from X's prefers
     (cl-callf2 remove y (multi-prefers fun (or hierarchy multi-global-hierarchy) x)))

    (otherwise
     (multi-error "in multi-prefers-remove malformed arglist at %s" args))))


;;* Hierarchies --------------------------------------------------- *;;


;; TODO accumulate path so we can report the actual cycle
(defun multi--cycle? (item parent &optional hierarchy)
  "Checks if ITEM and would be PARENT would form a cycle were the
relationship added to the HIERARCHY. If HIERARCHY not supplied
defaults to the global hierarchy"
  (default hierarchy :to multi-global-hierarchy)
  (or (equal item parent)
      ;; TODO is ormap the same as the built-in `some'?
      (ormap
       (lambda (ancestor) (multi--cycle? item ancestor hierarchy))
       (multi-hierarchy hierarchy parent :parents))))


(defun multi--rel (child parent hierarchy)
  "Installs CHILD - PARENT relation in HIERARCHY, propagates any
necessary :descendant - :ancestor relations up and down the
HIERARCHY tree. Returns updated HIERARCHY."

  ;; don't allow cyclic relations
  (when (multi--cycle? child parent hierarchy)
    (multi-error "in multi-rel cycle relationship between %s and %s "
                 child parent))

  ;; Update child and its descendants
  (progn
    ;; add parent to child's :parents
    (pushnew parent (multi-hierarchy hierarchy child :parents))
    ;; add parent to child's :ancestors
    (pushnew parent (multi-hierarchy hierarchy child :ancestors))
    ;; extend child's :ancestors with parent's :ancestors
    (callf cl-union (multi-hierarchy hierarchy child :ancestors)
      (multi-hierarchy hierarchy parent :ancestors))
    ;; propagate now extended child's :ancestors down the family tree by
    ;; extending every child descendant's :ancestors with child's
    ;; :ancestors
    (dolist (descendant (multi-hierarchy hierarchy child :descendants))
      (callf cl-union (multi-hierarchy hierarchy descendant :ancestors)
        (multi-hierarchy hierarchy child :ancestors))))

  ;; Update parent and its ancestors
  (progn
    ;; add child to parent's :children
    (pushnew child (multi-hierarchy hierarchy parent :children))
    ;; add child to parent's :descendants
    (pushnew child (multi-hierarchy hierarchy parent :descendants))
    ;; extend parent's :descendants with child's :descendants
    (callf cl-union (multi-hierarchy hierarchy parent :descendants)
      (multi-hierarchy hierarchy child :descendants))
    ;; propagate now extended parent's :descendants up the family tree by
    ;; extending every parent ancestor's :descendants with parent's
    ;; :descendants
    (dolist (ancestor (multi-hierarchy hierarchy parent :ancestors))
      (callf cl-union (multi-hierarchy hierarchy ancestor :descendants)
        (multi-hierarchy hierarchy parent :descendants))))

  ;; return hierarchy
  hierarchy)


(defmacro multi-rel (&rest args)
  "Establishes an isa? (parent/child) relationship between PARENT
and CHILD. If HIERARCHY not supplied defaults to, and modifies,
the global hierarchy.

\(fn child :isa parent &optional :in hierarchy)"
  (declare (debug t))
  (destructuring-bind
      (err child parent hierarchy)
      (pcase args
        (`(,child ,(or 'isa :isa) ,parent) (list nil child parent nil))
        (`(,child ,(or 'isa :isa) ,parent ,(or 'in :in) ,hierarchy) (list nil child parent hierarchy))
        (otherwise
         (list `(multi-error "in multi-rel malformed arglist at %s" ',args) nil nil nil)))
    (or
     err
     (let ((hierarchy (or hierarchy 'multi-global-hierarchy)))
       `(multi--rel ,child ,parent ,hierarchy)))))


(defun ormap (pred lst)
  (when lst
    (or (funcall pred (car lst))
        (ormap pred (cdr lst)))))


(cl-defun multi--seq-isa? (seqx seqy hierarchy)
  (let ((rels (seq-mapn (lambda (x y) (multi-isa? x y hierarchy)) seqx seqy)))
    (and (cl-notany #'null rels)
         rels)))


(cl-defun multi-isa? (x y &optional (hierarchy) (generation 0))
  "Returns (generation 0) if (equal CHILD PARENT), or (generation
N) if CHILD is directly or indirectly derived from PARENT, where
N signifies how far down generations PARENT is from CHILD. If
HIERARCHY not supplied defaults to the global hierarchy

\(fn child parent &optional hierarchy)"
  (default hierarchy :to multi-global-hierarchy)
  (cond
   ((sequencep x)
    (and (sequencep y)
         (equal (length x) (length y))
         (multi--seq-isa? x y hierarchy)))

   ((sequencep y)
    ;; then x wasn't a seq or failed x isa? y test
    nil)

   ((equal x y)
    (cons :generation generation))

   ((member y (multi-hierarchy hierarchy x :parents))
    (cons :generation (1+ generation)))

   (:else
    (ormap
     (lambda (parent) (multi-isa? parent y hierarchy (1+ generation)))
     (multi-hierarchy hierarchy x :parents)))))


(defun multi--ancestors (x hierarchy)
  "Returns ancestors of X by walking the HIERARCHY tree. List may
have duplicates."
  (let ((parents (multi-hierarchy hierarchy x :parents)))
    (append parents
            (seq-mapcat
             (lambda (parent)
               (multi--ancestors parent hierarchy))
             parents))))

(defun multi-ancestors (x &optional hierarchy compute?)
  "Returns all ancestors of X such that (multi-isa? X ancestor).
If HIERARCHY not supplied defaults to the global hierarchy. If
COMPUTE? is t actually computes ancestors by walking the tree."
  (default hierarchy :to multi-global-hierarchy)
  (default compute? :to nil)
  (if compute?
      (cl-delete-duplicates (multi--ancestors x hierarchy) :test #'equal)
    (multi-hierarchy hierarchy x :ancestors)))


(defun multi--descendants (x hierarchy)
  "Returns descendants of X by walking the HIERARCHY tree. List
may have duplicates."
  (let ((children (multi-hierarchy hierarchy x :children)))
    (append children
            (seq-mapcat
             (lambda (child)
               (multi--descendants child hierarchy))
             children))))

(defun multi-descendants (x &optional hierarchy compute?)
  "Returns all descendants of X such that (multi-isa? descendant
X). If HIERARCHY not supplied defaults to the global hierarchy.
If COMPUTE? is t actually computes descendants by walking the
tree."
  (default hierarchy :to multi-global-hierarchy)
  (default compute? :to nil)
  (if compute?
      (cl-delete-duplicates (multi--descendants x hierarchy) :test #'equal)
    (multi-hierarchy hierarchy x :descendants)))


;;* Multi --------------------------------------------------------- *;;


;; NOTE on caching. There are two obvious things we can cache.
;;
;; One is the dispatch function with respect to its arguments. Since user can
;; perform all sorts of expensive calculations (even if shouldn't) memoizing
;; dispatch invocations can speed up calculating the value to be used later to
;; perform isa? search in a hierarchy. One possible gotcha here is that the
;; dispatch function must be pure! If the body relies on any external state, then
;; our cache can quite easily be stale, since it is obviously impossible to
;; determine when to invalidate it. Luckily, it should be fairly easy to make any
;; dispatch function pure simply by moving whatever stateful value you're looking
;; up to its arguments, that is look it up before you make a multi-call and call
;; with that extra argument. This may, potentially, lead to another gotcha with
;; respect to concurrency: you want the state lookup and multi-call performed in
;; transaction else you may end up with a race where relevant state gets updated
;; while the dispatch is in flight. This is a very generic comment and may not be
;; relevant to Emacs Lisp - I know nothing at all about its concurrency model -
;; does it ever have one? First, I need to make note about caching in
;; documentation; second, we probably want to let user turn this cache on and off
;; as needed; third, maybe I should allow another attribute in `multi' e.g. (:pure
;; t) so that user can declare that his dispatch is indeed pure and we can cache
;; as needed. Another potential issue with caching is that arguments maybe
;; something difficult to test for equality e.g. anonymous functions. Appears that
;; Emacs Lisp somehow manages to equate (lambda (x) x) with (lambda (x) x) but I
;; suspect it must be a hack that doesn't work in general. Since we are to use
;; args as keys in cache table this lead to said table to explode in size in tight
;; loops when keys are structurally equal, but Elisp can't really know that.
;;
;; The other is caching isa? hierarchy lookup. This implies that each hierarchy
;; needs to keep track of its cache and invalidate it every time a relationship is
;; added or removed. We'll want to change the signature of the `multi-isa?' or
;; introduce another function. New signature should be: (-> value hierarchy
;; result) that is it doesn't take a VALUE to check if (isa? val VALUE) but
;; instead tries every item in the hierarchy. This way we can easily memoize
;; hierarchy isa? checks with respect to just one val argument. Since this cache
;; is per hierarchy, every hierarchy should probably carry its isa? relationship
;; lookup with it. This probably means that hierarchies should be implemented as
;; structs or given a symbolic name so that they can keep such meta information in
;; the plist. IMO struct would be cleaner.


(pcase-defmacro multi (&rest patterns)
  (pcase patterns
    (`(,id :if ,predicate) `(and ,id (pred ,predicate)))
    (otherwise
     `(multi-error "malformed pcase multi pattern"))))


(example
 (pcase '([a b] "doc")
   (`(,(multi arglist :if vectorp) ,(multi doc :if stringp)) (list arglist doc))
   (otherwise
    (error "no match")))
 ;; example
 )


;; TODO Lexical vs dynamic scope. Something I ran into by chance. `multi-tests.el'
;; doesn't have lexical scope on and this has interesting implications for
;; multimethods. Say, this example won't work as expected in dynamic scope:
;;
;;   (let ((hierarchy (ht))
;;         (b 42))
;;     (multi-rel :rect isa :shape in hierarchy)
;;     (multi baz (lambda (x) (princ b) x) :in hierarchy)
;;     (multimethod baz (x) :when :shape :shape)
;;     (baz :rect)
;;     ;; prints  42 and returns :shape as expected
;;     )
;;
;;   ;; but outside of `let' neither variables hierarchy or b are bound due to dynamic
;;   ;; scope
;;
;;   (baz :rect)
;;   ;; => Error symbol value is void 'b
;;
;; Problem here is that although `multi' macro is defined in lexical scope, it
;; expansion may happen in dynamic scope (if the user so chooses or, like me,
;; forgets to enable lexical scope) and then much bafflement follows. E.g.
;; `hierarchy' parameter in the definition of `multi' is assumed to be bound in
;; the surrounding scope, so if that scope is dynamic the defined dispatch
;; function we intorduce doesn't close over `hirarchy', nor yet any other external
;; binding. So, if at any point those bindings go away dispatch goes ka-boom!
;;
;; If my stackoverflow question
;; https://emacs.stackexchange.com/questions/46088/forcing-lexical-scope-in-the-middle-of-dynamic-scope
;; ever gets answered I could check for `lexical-binding' at call site and
;; optionally hijack or force lexical scope for the body of my macro. For the rare
;; case where the user does actually mean dynamic scope, I could control whether
;; to force lexical or not with a variable they could set.
;;
;; At the very minimum I should make a note of this gotcha in documentation.


(defun multi--select-preferred (fun methods hierarchy dispatch-val)
  "Narrows METHODS matching DISPATCH-VAL down to a single method
based on preferences registered for multidispatch FUN and
HIERARCHY. Returns that method or signals an error."
  (if (= (ht-size methods) 1)

      ;; just one method, no ambiguity
      (car (ht-values methods))

    ;; multiple methods matching dispatch-val, use preferences to resolve
    (let* ((prefers (multi-prefers fun hierarchy))

           ;; for all keys in methods collect all values they prefer away
           (filter-set (seq-mapcat (lambda (val) (ht-get prefers val)) (ht-keys methods)))

           ;; trimp methods by removing all entries with keys in the filter-set
           (preferred (ht-reject (lambda (k v) (member k filter-set)) methods))

           ;; size is one of 0, 1, 2...
           (size (ht-size preferred)))
      (cond
       ;; one method wins, return it
       ((= size 1)
        (car (ht-values preferred)))

       ;; more than one method remains
       ((> size 1)
        (multi-error
         "multiple methods match dispatch value %s for dispatch %s:\n%s\n%s"
         dispatch-val fun
         (string-join
          (ht-map (fn (VAL _) (format "  %s :isa %s" dispatch-val VAL)) preferred)
          "\n")
         "and neither is preferred"))

       ;; no methods at all can only ever happen if the user managed to register
       ;; preferences that are mutually inconsitent i.e. create a cycle. If this
       ;; ever happens, our `multi--preference-cycle?' must be buggy.
       ((= size 0)
        (multi-error
         (string-join
          (list "inconsintency in registered multi-prefers unable to prefer any of" "  %s"
           "with registered preferences" "  %s") "\n")
         (ht-keys methods)
         prefers))))))


(defmacro multi (fun &rest args)
  "Creates a new multimethod dispatch function. The DOCSTRING and
HIERARCHY are optional. HIERARCHY if not supplied defaults to the
global hierarchy.

May be called according to one of the following signatures:

  (multi name argvector &optional docstring :in hierarchy body...)
  (multi name function &optional docstring :in hierarchy)

where ARGVECTOR is a vector of arguments that follows full Common
Lisp arglist convention, FUNCTION is any expression that returns
a function.

\(fn name argvector &optional docstring :in hierarchy body...)"
  (declare (indent 2))
  (destructuring-bind
      (err dispatch doc hierarchy)
      (pcase args
        ;; (multi foo [a b &rest args] "doc" :in hierarchy e1 e2)
        (`(,(multi arglist :if vectorp) ,(multi doc :if stringp) :in ,hierarchy . ,body)
         (list nil `(fn ,(seq-into arglist 'list) ,@body) doc hierarchy))

        ;; (multi foo [a b &rest args] :in hierarchy e1 e2)
        (`(,(multi arglist :if vectorp) :in ,hierarchy . ,body)
         (list nil `(fn ,(seq-into arglist 'list) ,@body) "" hierarchy))

        ;; (multi foo [a b &rest args] "doc" e1 e2)
        (`(,(multi arglist :if vectorp) ,(multi doc :if stringp) . ,body)
         (list nil `(fn ,(seq-into arglist 'list) ,@body) doc 'multi-global-hierarchy))

        ;; (multi foo [a b &rest args] e1 e2)
        (`(,(multi arglist :if vectorp) . ,body)
         (list nil `(fn ,(seq-into arglist 'list) ,@body) "" 'multi-global-hierarchy))

        ;; (multi foo fn-returning-expr "doc" :in hierarchy)
        (`(,f ,(multi doc :if stringp) :in ,hierarchy)
         (list nil f doc hierarchy))

        ;; (multi foo fn-returning-expr :in hierarchy)
        (`(,f :in ,hierarchy)
         (list nil f "" hierarchy))

        ;; (multi foo fn-returning-expr "doc")
        (`(,f ,(multi doc :if stringp))
         (list nil f doc 'multi-global-hierarchy))

        ;; (multi foo fn-returning-expr)
        (`(,f)
         (list nil f "" 'multi-global-hierarchy))

        (otherwise
         ;; TODO If we signal an an error immediately no relevant `condition-case'
         ;; would catch it, because IIUC it only traps runtime but we throw at
         ;; macro expansion, so instead I need to generate code that throws.
         ;; Wonder if there is a way to trap compile time errors?
         (list `(multi-error "in multi malformed arglist at %s" ',args) nil nil nil)))
    (or
     err
     `(progn

        ;; check if lexical binding is enabled
        (multi-lexical-binding)

        ;; create a dispatch function
        (defun ,fun (&rest args)
          ,doc
          (let* ((val     (apply (get ',fun :multi-dispatch) args))
                 (methods (multi-methods :for ',fun :matching val :in ,hierarchy))
                 (method  (multi--select-preferred ',fun methods ,hierarchy val)))
            (apply method args)))

        ;; set fun value slot to return its quoted form, this lets us pass fun
        ;; around as value and still not break functions that expect a symbol
        (setf ,fun ',fun)

        ;; reset dispatch prop to the dispatch function
        (setf (get ',fun :multi-dispatch) ,dispatch)

        ;; reset multi-methods prop to a fresh table with :default pre-installed
        (setf (get ',fun :multi-methods) (ht))

        ;; reset multi-prefers prop to a fresh table
        (setf (get ',fun :multi-prefers) (ht))
        (setf (multi-prefers ',fun ,hierarchy) (ht))

        ;; pre-install default method
        (setf (get ',fun :multi-default)
              (fn (&rest args)
                (multi-error
                 "no multimethods match dispatch value %s for dispatch %s "
                 (apply (get ',fun :multi-dispatch) args)
                 ',fun)))

        ;; TODO Invalidate multi-methods cache here. Need to do this to catch
        ;; cases where fun simply gets redefined and may hold cache for previous
        ;; dispatch function
        ))))


(comment
 ;; Syntax examples
 (multi foo (lambda (a b &rest args) e1 e2) "doc" :in hierarchy)
 (multi foo (lambda (a b &rest args) e1 e2) :in hierarchy)
 (multi foo (lambda (a b &rest args) e1 e2) "doc")
 (multi foo (lambda (a b &rest args) e1 e2))

 (multi foo 'foo-fun "doc" :in hierarchy)
 (multi foo 'foo-fun :in hierarchy)
 (multi foo 'foo-fun "doc")
 (multi foo 'foo-fun)

 (multi foo [a b &rest args] "doc" :in hierarchy e1 e2)
 (multi foo [a b &rest args] :in hierarchy e1 e2)
 (multi foo [a b &rest args] "doc" e1 e2)
 (multi foo [a b &rest args] e1 e2)

 ;; Same but pseudo-coded with &optional and &rest
 (multi foo (lambda (a &rest args) body) &optional "doc" :in hierarchy)
 (multi foo #'dispatch &optional "doc" :in hierarchy)
 (multi foo [a &rest args] &optional "doc" :in hierarchy &rest body)
 ;; example
 )


;; TODO Consider generalizing syntax for installed relationship not just `isa'
;; (multimethod foo (a b) :isa :b body)
;; (multimethod foo (a b) :parent-of :b body)
(cl-defmacro multimethod (fun arglist &rest args)
  "Creates a new multimethod associated with the dispatch
function FUN and dispatch value VAL. ARGLIST follows full Common
Lisp conventions.

\(fn fun arglist :when val body...)"
  (pcase args
    (`(:when ,val . ,body)
     (let ((method `(fn ,arglist ,@body)))
       `(progn

          ;; check if lexical binding is enabled
          (multi-lexical-binding)

          ;; add new method to the multi-methods table
          (setf (ht-get* (get ',fun :multi-methods) ,val) ,method)

          ;; TODO invalidate multi-methods cache
          )))
    (otherwise
     `(multi-error "in multimethod malformed arglist at %s" ',args))))


;;* Playground ---------------------------------------------------- *;;


(comment
 (multimethod foo (&rest args) :when [a b c] body)
 (multimethod foo (&rest args) :when [a b _] body)
 (multimethod foo (&rest args) :when [a (?  pred-p) c] body)

 ;; degenerate case where computed multi val maybe a seq, pred-p should still be
 ;; applied even though this here val isn't a seq
 (multimethod foo (&rest args) :when (?  pred-p) body)
 ;; comment
 )


;;* Provide ------------------------------------------------------- *;;


(provide 'multi)
