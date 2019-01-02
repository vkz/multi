;; -*- lexical-binding: t; -*-

;; TODO go over all code and try to avoid using let-over lambdas. Basically don't
;; assume we're lexically scoped and everything is a closure. I'd like everything
;; to work with lexical-binding nil at least for obvious cases.

(require 'multi-prelude)
(require 'multi-patterns)


;;* Prelude ------------------------------------------------------- *;;


(pcase-defmacro multi (&rest patterns)
  (pcase patterns
    (`(,id :if ,predicate) `(and ,id (pred ,predicate)))
    (otherwise
     `(mu-error "malformed pcase multi pattern"))))


(example
 (pcase '([a b] "doc")
   (`(,(multi arglist :if vectorp) ,(multi doc :if stringp)) (list arglist doc))
   (otherwise
    (error "no match")))
 ;; example
 )


;;* Settings  ----------------------------------------------------- *;;


(defcustom mu-lexical-binding 'error
  "multi-methods may not work correctly without
`lexical-binding'. By default check and signal an error if an
attempt is made to use multi-methods in dynamic scope.")


;; TODO (mu-lexical-binding) check is somehow subtly broken when you
;; byte-compile-file that defines multimethods and then load. With lexical-binding
;; set it remains on when you compile, but on load it appears nil. I don't know
;; what's going on. Either byte-compile is subtly broken, or by the time we
;; byte-compile every defun is already a closure and load happens in dynamic
;; environment. Until I figure this out, I am disabling this check. See:
;; https://emacs.stackexchange.com/questions/46812/byte-compile-and-lexical-binding
(setq mu-lexical-binding nil)


(defun mu-lexical-binding ()
  "Signal an error depending on the setting of
`mu-lexical-binding' and `lexical-binding'."
  (when mu-lexical-binding
    (unless lexical-binding
      (mu-error :lexical-binding))))


;;* Hierarchies --------------------------------------------------- *;;


(defstruct mu-hierarchy
  ;; TODO should this be an UUID or gensym is enough to avoid collisions?
  (id (gensym "mu-hierarchy"))
  ;; table: dispatch-val => (ht :parents :children :ancestors :descendants)
  (table (ht))
  (cache (ht)))


(defun mu-hierarchy (hierarchy &rest keys)
  "Lookup a value at KEYS in HIERARCHY or return the entire
table. Allow use as gv-variable."
  (declare
   (gv-setter (lambda (val)
                `(setf (ht-get* (mu-hierarchy-table ,hierarchy) ,@keys) ,val))))
  (if keys
      (apply #'ht-get* (mu-hierarchy-table hierarchy) keys)
    (mu-hierarchy-table hierarchy)))


(defconst mu-global-hierarchy (make-mu-hierarchy)
  "Global hierarchy")


(defvar mu--hierarchy-override nil
  "Hierarchy that overrides the global if set")


(defsubst mu--hierarchy (&optional symbol)
  "Return the hierarchy active in the current dynamic extent."
  (or
   ;; static hierarchy tramps all and cannot be overriden
   (when symbol (get symbol :static-hierarchy))
   ;; dynamic hierarchy override trams both custom and global
   mu--hierarchy-override
   ;; custom hierarchy tramps global
   (when symbol (get symbol :hierarchy))
   ;; global hierarchy
   mu-global-hierarchy))


(defmacro mu-with-hierarchy (hierarchy &rest body)
  "Prefer HIERARCHY during the dynamic extent of the body."
  (declare (indent 1))
  `(let ((mu--hierarchy-override ,hierarchy))
     ,@body))


(defun mu--cycle (child parent hierarchy)
  (if (equal child parent)
      (list parent)
    (when-let ((cycle (some
                       (lambda (ancestor) (mu--cycle child ancestor hierarchy))
                       (mu-hierarchy hierarchy parent :parents))))
      (cons parent cycle))))


(defun mu--cycle? (child parent &optional hierarchy compute?)
  "Check if CHILD and PARENT would create a cycle in the
currently active hierarchy and return it."
  (default hierarchy :to (mu--hierarchy))
  (when (or (equal child parent)
            (member child
                    (if compute?
                        (mu-ancestors parent hierarchy 'compute)
                      (mu-ancestors parent hierarchy))))
    ;; compute and report full cycle for debugging
    (mu--cycle child parent hierarchy)))


;; NOTE `mu--rel' may seem intimidating and very easy to get wrong. It ends up
;; what it is more out of curiousity than necessity. We pre-compute ancestors so
;; that `mu-isa?' requires a single `member' lookup. Children, parents,
;; descendants are redundant but we may as well pre-compute them so we do. This
;; implementation trades off time pre-computing relations whever a hierarchy gets
;; extended for the the time required to perform `mu-isa?' check, obviously
;; favoring the latter. We could, and the initial implementation did, re-compute
;; relations every time `mu-isa?' is called. Seemingly wasteful it could be made
;; asymptotically as performant as the pre-computed one by caching its calls,
;; invalidating the cache whenever a hierarchy gets extended.


(defun mu--rel (child parent hierarchy)
  "Extend HIERARCHY with a new CHILD - PARENT relation, propagate
any necessary :descendant - :ancestor relations up and down the
HIERARCHY tree. Return updated HIERARCHY."

  ;; there's no meaningful semantics to relate structured data
  (when (or (ht-p child)
            (ht-p parent)
            (and (seqp child) (not (null child)))
            (and (seqp parent) (not (null parent)))
            (cl-struct-p child)
            (cl-struct-p parent))
    (mu-error :rel-semantics child parent))

  ;; don't allow cyclic relations
  (when-let ((cycle (mu--cycle? child parent hierarchy)))
    (mu-error :rel-cycle child parent cycle))

  ;; Update child and its descendants
  (progn
    ;; add parent to child's :parents
    (pushnew parent (mu-hierarchy hierarchy child :parents))
    ;; add parent to child's :ancestors
    (pushnew parent (mu-hierarchy hierarchy child :ancestors))
    ;; extend child's :ancestors with parent's :ancestors
    (callf cl-union (mu-hierarchy hierarchy child :ancestors)
      (mu-hierarchy hierarchy parent :ancestors))
    ;; propagate now extended child's :ancestors down the family tree by
    ;; extending every child descendant's :ancestors with child's
    ;; :ancestors
    (dolist (descendant (mu-hierarchy hierarchy child :descendants))
      (callf cl-union (mu-hierarchy hierarchy descendant :ancestors)
        (mu-hierarchy hierarchy child :ancestors))))

  ;; Update parent and its ancestors
  (progn
    ;; add child to parent's :children
    (pushnew child (mu-hierarchy hierarchy parent :children))
    ;; add child to parent's :descendants
    (pushnew child (mu-hierarchy hierarchy parent :descendants))
    ;; extend parent's :descendants with child's :descendants
    (callf cl-union (mu-hierarchy hierarchy parent :descendants)
      (mu-hierarchy hierarchy child :descendants))
    ;; propagate now extended parent's :descendants up the family tree by
    ;; extending every parent ancestor's :descendants with parent's
    ;; :descendants
    (dolist (ancestor (mu-hierarchy hierarchy parent :ancestors))
      (callf cl-union (mu-hierarchy hierarchy ancestor :descendants)
        (mu-hierarchy hierarchy parent :descendants))))

  ;; return hierarchy
  hierarchy)


(defmacro mu-rel (child :isa parent &optional hierarchy)
  "Establish an isa relationship between CHILD and PARENT."
  `(mu--rel ,child ,parent (or ,hierarchy (mu--hierarchy))))


(defun mu-isa? (child parent &optional hierarchy)
  "Check if CHILD is isa? related to PARENT."
  (default hierarchy :to (mu--hierarchy))
  (or (equal child parent)
      (and (sequencep child)
           (sequencep parent)
           (equal (length child) (length parent))
           (every (lambda (child parent) (mu-isa? child parent hierarchy)) child parent))
      (and (not (sequencep parent))
           (member parent (mu-ancestors child hierarchy)))))


;; NOTE Since we pre-compute ancestors and descendants we can simply return them
;; by looking up in the hierarchy. We keep `mu--descendants' and `mu--ancestors'
;; around for testing and debugging in case our `mu--rel' implemantation turns out
;; to be buggy. Ditto `mu-isa/generations?'.


(defun mu--ancestors (x hierarchy)
  "Return ancestors of X by walking the HIERARCHY tree."
  (let ((parents (mu-hierarchy hierarchy x :parents)))
    ;; TODO instead of append I could cl-union to avoid cl-delete-duplicates use
    ;; later, but this is fine too?
    (append parents
            (seq-mapcat
             (lambda (parent)
               (mu--ancestors parent hierarchy))
             parents))))


(defun mu--descendants (x hierarchy)
  "Return descendants of X by walking the HIERARCHY tree."
  (let ((children (mu-hierarchy hierarchy x :children)))
    (append children
            (seq-mapcat
             (lambda (child)
               (mu--descendants child hierarchy))
             children))))


(defun mu-ancestors (x &optional hierarchy compute?)
  "Return all ancestors of X such that (mu-isa? X ancestor)."
  (default hierarchy :to (mu--hierarchy))
  (if compute?
      (cl-delete-duplicates (mu--ancestors x hierarchy) :test #'equal)
    (mu-hierarchy hierarchy x :ancestors)))


(defun mu-descendants (x &optional hierarchy compute?)
  "Return all descendants of X such that (mu-isa? descendant X)."
  (default hierarchy :to (mu--hierarchy))
  (if compute?
      (cl-delete-duplicates (mu--descendants x hierarchy) :test #'equal)
    (mu-hierarchy hierarchy x :descendants)))


(cl-defun mu--generations (seqx seqy hierarchy)
  (let ((rels (seq-mapn (lambda (x y) (mu-isa/generations? x y hierarchy)) seqx seqy)))
    (and (cl-notany #'null rels)
         rels)))


(cl-defun mu-isa/generations? (x y &optional (hierarchy) (generation 0))
  "Like `mu-isa?' but return the generation gap between CHILD and
PARENT."
  (default hierarchy :to (mu--hierarchy))
  (cond
   ((sequencep x)
    (and (sequencep y)
         (equal (length x) (length y))
         (mu--generations x y hierarchy)))

   ((sequencep y)
    ;; then x wasn't a seq or failed x isa? y test
    nil)

   ((equal x y)
    (cons :generation generation))

   ((member y (mu-hierarchy hierarchy x :parents))
    (cons :generation (1+ generation)))

   (:else
    (some
     (lambda (parent) (mu-isa/generations? parent y hierarchy (1+ generation)))
     (mu-hierarchy hierarchy x :parents)))))


;;* Methods ------------------------------------------------------- *;;


(defun mu--methods (fun-symbol &rest keys)
  "Returns the mu-methods hash-table of FUN-SYMBOL or the
value nested in that table if the sequence of KEYS is supplied.
This form is `setf'-able."
  (declare
   (gv-setter (lambda (val)
                (if keys
                    ;; with keys set corresponding value in :mu-methods table
                    `(setf (ht-get* (get ,fun-symbol :mu-methods) ,@keys) ,val)
                  ;; without keys set :mu-methods prop itself to a new table
                  `(setf (get ,fun-symbol :mu-methods) ,val)))))
  (if keys
      (apply #'ht-get* (get fun-symbol :mu-methods) keys)
    (get fun-symbol :mu-methods)))


;; TODO mu-methods is probably the most perf sensitivy function, so we shouldn't
;; overload it. IMO better extract into a separate mu-dispatch: fun val hier ->
;; methods, so it cane cache as needed. Also definitely don't need that argument
;; parsing overhead or recursion. May keep this implementation around to let users
;; reflect the dispatch as it happens, but use mu-dispatch for actual dispatch?


(defun mu-methods (&rest args)
  "Returns a hash-table of FUN-SYM mu-methods, where each entry
is a (value . method) pair.

When called with :for and :matching keywords limits the table to
pairs where (mu-isa? VAL value) relationship holds or
the (:default . method) if none match.

When called without :for and :matching keywords returns the
entire table or the value nested in it if the sequence of KEYS is
supplied. This form is `setf'-able.

May be called according to one of the following signatures:

  (mu-methods :for fun-sym :matching val)
  (mu-methods fun-sym &rest keys)
  (setf (mu-methods fun-sym &rest keys) val)

\(fn :for fun-sym :matching val)"
  (declare
   (gv-setter (lambda (val)
                (pcase args

                  ;; (setf (mu-methods 'foo &rest keys) val)
                  (`((quote ,(multi fun-symbol :if symbolp)) . ,keys)
                   `(setf (mu--methods ',fun-symbol ,@keys) ,val))

                  ;; (setf (mu-methods foo &rest keys) val)
                  (`(,(multi fun-symbol :if symbolp) . ,keys)
                   `(setf (mu--methods ,fun-symbol ,@keys) ,val))

                  (otherwise
                   `(mu-error :malformed-methods ',args))))))
  ;; parse args
  (pcase args

    ;; (mu-methods :for 'fun :matching val)
    (`(:for ,fun :matching ,val)
     (let* ((mu-methods
             (get fun :mu-methods))

            (methods
             (ht-select (fn (VAL _) (mu-isa? val VAL (mu--hierarchy fun))) mu-methods))

            (methods
             (unless (ht-empty? methods) methods))

            (default-method
              (unless methods
                (ht
                 ;; Use custom :default if installed, fallback to the
                 ;; pre-installed default method otherwise
                 (:default (or (ht-get* mu-methods :default)
                               (get fun :mu-default)))))))
       (or methods default-method)))

    ;; (mu-methods 'fun :rect)
    (`(,(multi fun-symbol :if symbolp) . ,keys)
     (apply #'mu--methods fun-symbol keys))

    (otherwise
     (mu-error :malformed-methods args))))


;; NOTE this naming `mu-methods-remove' becomes more consistent if or when we
;; allow :before :after :around methods
(defun mu-methods-remove (fun dispatch-value)
  "Removes the mu-method FUN associated with DISPATCH-VALUE."
  (ht-remove! (mu-methods fun) dispatch-value))


;;* Prefers ------------------------------------------------------- *;;


(defun mu-prefers (fun &rest keys)
  "Return a table of registered value preferences. When VAL is
supplied return just the set of all values over which VAL is
preferred. This form is `setf'-able.

\(mu-prefers fun &optional val)"
  (declare
   (gv-setter (lambda (val)
                `(setf (ht-get* (get ,fun :mu-prefers)
                                (mu-hierarchy-id (mu--hierarchy ,fun))
                                ,@keys)
                       ,val))))

  ;; return set of values VAL tramps or the entire prefers table
  (apply #'ht-get*
         (get fun :mu-prefers)
         (mu-hierarchy-id (mu--hierarchy fun))
         keys))


(defun mu--preference-cycle? (item parent fun)
  "Check if preferring ITEM over PARENT would form a cycle and
return it."
  (if (equal item parent)
      (list parent)
    (when-let ((cycle (some
                       (lambda (ancestor) (mu--preference-cycle? item ancestor fun))
                       (mu-prefers fun parent))))
      (cons parent cycle))))


(example
 (mu-test (baz)
   (mu-defmulti baz #'identity)
   (mu-prefer 'baz [:rect :shape] :over [:shape :rect])
   (mu-prefer 'baz [:shape :rect] :over [:parallelogram :rect])
   ;; (mu-prefer 'baz [:parallelogram :rect] :over [:rect :shape])
   (mu--preference-cycle? [:parallelogram :rect] [:rect :shape] 'baz))
 ;; example
 )


(mu-defun mu-prefer (fun &rest args)
  "Prefer dispatch X over Y when resolving a method. May be
called according to one of the following signatures:

  (mu-prefer foo x :to y)
  (mu-prefer foo x y)

\(mu-prefer foo x :over y)"
  ([_ x y] (if-let ((cycle (mu--preference-cycle? x y fun)))
               (mu-error :cyclic-prefer x y cycle)
             (pushnew y (mu-prefers fun x))))

  ([_ x (or :to :over 'to 'over) y] (mu-prefer fun x y))

  (otherwise
   (mu-error :malformed-prefer args)))


(mu-defun mu-unprefer (fun &rest args)
  "Remove registered preferences for FUN multi-dispatch function:

  (mu-unprefer foo x :to y) do not prefer X over Y
  (mu-unprefer foo x y)     do not prefer X over Y
  (mu-unprefer foo x)       remove all X preferences
  (mu-unprefer foo)         remove all preferences

\(fn foo x :over y)"
  ;; remove all prefers
  ([_] (setf (mu-prefers fun) (ht)))
  ;; remove prefers for value X
  ([_ x] (ht-remove! (mu-prefers fun) x))
  ;; remove value Y from X's prefers
  ([_ x y] (cl-callf2 remove y (mu-prefers fun x)))
  ;; remove value Y from X's prefers
  ([_ x (or :over :to 'over 'to) y] (cl-callf2 remove y (mu-prefers fun x)))
  ;; malformed call
  (otherwise (mu-error :malformed-unprefer args)))


;; TODO this ought to be part of the `mu-methods' call, so I don't have to cache
;; multiple functions
(defun mu--select-preferred (fun methods val)
  "Narrow METHODS matching dispatch value VAL down to a single
method based on preferences registered for multi-dispatch FUN or
signal an error."
  (if (= (ht-size methods) 1)

      ;; just one method, no ambiguity
      (car (ht-values methods))

    ;; multiple methods matching dispatch-val, use preferences to resolve
    (let* ((prefers (mu-prefers fun))

           ;; for all keys in methods collect all values they prefer away
           (filter-set (seq-mapcat (lambda (v) (ht-get prefers v)) (ht-keys methods)))

           ;; trim methods by removing all entries with keys in the filter-set
           (preferred (ht-reject (lambda (k v) (member k filter-set)) methods))

           ;; size is one of 0, 1, 2...
           (size (ht-size preferred)))
      (cond
       ;; one method wins, return it
       ((= size 1)
        (car (ht-values preferred)))

       ;; more than one method remains
       ((> size 1)
        (let ((fmt (lambda (v) (format "  %s :isa %s" val v))))
          (mu-error :ambiguous-methods fun val (mapconcat fmt (ht-keys preferred) "\n"))))

       ;; no methods at all can only ever happen if the user managed to register
       ;; preferences that are mutually inconsitent i.e. create a cycle. If this
       ;; ever happens, our `mu--preference-cycle?' must be buggy.
       ((= size 0)
        (mu-error :inconsistent-prefers fun val (mu--hierarchy fun) prefers))))))


;;* Multi --------------------------------------------------------- *;;


(defmacro mu-defmulti (name arglist &optional docstring &rest body)
  "Define a new multi-dispatch function NAME. If ARGLIST and BODY
follow an `mu-defun' single or multi-head calling convention use
mu-lambda to create a dispatch function, else assume CL-arglist.
If ARGLIST is itself a function, the BODY is ignored and that
function is used to dispatch.

The following attributes can appear before the BODY:

  :hierarchy         custom-hierarchy
  :static-hierarchy  static-hierarchy"
  (declare (indent defun))

  (unless (stringp docstring)
    (push docstring body)
    (setq docstring ""))

  (let* ((mu-multi-head? (mu--defun-multi-head-body body))
         (attrs (car (mu--prefix-map body)))
         (dispatch (cond
                    ;; TODO testing for function at compile time feels off. Can we
                    ;; really do it reliably or am I mixing the two phases? Do I
                    ;; push it to runtime?
                    ((mu-function? arglist) arglist)
                    ((vectorp arglist)      `(mu ,arglist ,@body))
                    (mu-multi-head?         `(mu ,arglist ,@body))
                    ((listp arglist)        `(fn ,arglist ,@body))
                    (:else
                     `(mu-error :malformed-defmulti ',name)))))

    (with-gensyms (args val methods method)
      `(progn

         ;; check if lexical binding is enabled
         (mu-lexical-binding)

         ;; create a dispatch function
         (defun ,name (&rest ,args)
           ,docstring
           (let* ((,val     (apply (get ',name :mu-dispatch) ,args))
                  (,methods (mu-methods :for ',name :matching ,val))
                  (,method  (mu--select-preferred ',name ,methods ,val)))
             (apply ,method ,args)))

         ;; set fun value slot to return its quoted form, this lets us pass fun
         ;; around as value and still not break functions that expect a symbol
         (setf ,name ',name)

         ;; reset dispatch prop to the dispatch function
         (setf (get ',name :mu-dispatch) ,dispatch)

         ;; reset mu-methods prop to a fresh table with :default pre-installed
         (setf (get ',name :mu-methods) (ht))

         ;; set hierarchy prop if passed, reset it to nil even if not passed, so
         ;; that if ever symbol gets redefined we don't get stale data
         (setf (get ',name :hierarchy) ,(ht-get attrs :hierarchy))

         ;; set or reset static-hierarchy prop
         (setf (get ',name :static-hierarchy) ,(ht-get attrs :static-hierarchy))

         ;; reset mu-prefers prop to a fresh table
         (setf (get ',name :mu-prefers) (ht))
         (setf (mu-prefers ',name) (ht))

         ;; pre-install default method
         (setf (get ',name :mu-default)
               (lambda (&rest ,args)
                 (mu-error :no-methods (apply (get ',name :mu-dispatch) ,args) ',name)))

         ;; TODO Invalidate mu-methods cache here. Need to do this to catch
         ;; cases where fun simply gets redefined and may hold cache for previous
         ;; dispatch function
         ))))


;; NOTE Far as I can tell if I were to define some methods with mu-defmethod then
;; compile that file and load it mu-methods table will have compiled lambdas, so
;; that's good. But the (byte-compile (mu-defmethod ...)) doesn't seem to install
;; compiled lambdas, not sure why.


(defmacro mu-defmethod (name arglist :when val &rest body)
  "Add a new method to multi-dispatch function NAME for dispatch
value VAL. If ARGLIST and BODY follow an `mu-defun' single or
multi-head calling convention use mu-lambda to create a method,
else assume CL-arglist."
  (declare (indent defun))
  (let* ((mu-multi-head? (mu--defun-multi-head-body body))
         (method (cond
                  ((mu-function? arglist) arglist)
                  ((vectorp arglist)      `(mu ,arglist ,@body))
                  (mu-multi-head?         `(mu ,arglist ,@body))
                  ((listp arglist)        `(fn ,arglist ,@body))
                  (:else
                   `(mu-error :malformed-defmethod ',name)))))
    `(progn
       ;; check if lexical binding is enabled
       (mu-lexical-binding)
       ;; add new method to the mu-methods table
       (setf (ht-get* (get ',name :mu-methods) ,val) ,method)
       ;; TODO invalidate mu-methods cache
       )))


;;* Provide ------------------------------------------------------- *;;


(provide 'multi-methods)


;; TODO Elisp specific idea is to allow supplying setters in mu-methods, so that
;; mu-defmethod invocation can be used with gv setters like `setf', `push', `callf'
;; etc. That makes perfect sence if your dispatch is for looking up some location
;; based on arguments. It may on occasion be quite natural to use the same syntax
;; to set new value to that location.

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
;; I'll have to overload mu-methods accessor function so it can do what current
;; `mu-methods' can do. One interesting aspect is that the value of foo then
;; becomes a struct and predicate (mu-p foo) works as expected, this however
;; necessitates passing quoted foo where expected, but we could work around this
;; by adding an extra :name or :id or :symbol slot, what would carry 'foo.

;; TODO dispatch cache

;; TODO hierarchy cache

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
;; up to its arguments, that is look it up before you make a mu-call and call
;; with that extra argument. This may, potentially, lead to another gotcha with
;; respect to concurrency: you want the state lookup and mu-call performed in
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
;; added or removed. We'll want to change the signature of the `mu-isa?' or
;; introduce another function. New signature should be: (-> value hierarchy
;; result) that is it doesn't take a VALUE to check if (isa? val VALUE) but
;; instead tries every item in the hierarchy. This way we can easily memoize
;; hierarchy isa? checks with respect to just one val argument. Since this cache
;; is per hierarchy, every hierarchy should probably carry its isa? relationship
;; lookup with it. This probably means that hierarchies should be implemented as
;; structs or given a symbolic name so that they can keep such meta information in
;; the plist. IMO struct would be cleaner.
;;
;; Actually, the above paragraph isn't quite right. In my experience hierarchies
;; are rarely used unless you have a quality global one that captures type
;; hierarchies in the language and you often dispatch on type-of. Most cache
;; benefit comes from not having to choose the method by sequentially doing isa
;; with every registered dispatch value. So, as the number of registered methods
;; grow you want to cache the choice you make based on the incomming value. In my
;; case caching should be done for: (mu-methods :for val :in hierarchy)

;; TODO Lexical vs dynamic scope. Something I ran into by chance. `mu-tests.el'
;; doesn't have lexical scope on and this has interesting implications for
;; mu-methods. Say, this example won't work as expected in dynamic scope:
;;
;;   (let ((hierarchy (ht))
;;         (b 42))
;;     (mu-rel :rect isa :shape in hierarchy)
;;     (mu-defmulti baz (lambda (x) (princ b) x) :in hierarchy)
;;     (mu-defmethod baz (x) :when :shape :shape)
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


;; Extras
;; --------

;; TODO the (mu-methods 'fun &rest keys) interface suggests an interesting
;; feature. We could go a bit further than Clojure and allow :before, :after,
;; :arround methods, so the mu-methods table doesn't just maps an isa? pattern
;; to a method but potentionally to a map of methods:
;;
;; (ht (:before #'before-fun)
;;     (:after #'after-fun)
;;     (:main #'main-fun))
;;
;; Can I come up with interesting semantics?

;; TODO Think about reasonable and practical global-hierarchy, e.g. one that works
;; for structs, isa relationship between predicates, maybe even eieio classes
;; although I have no experienc with those.

;; TODO Allow isa? with "_" patterns
;; (mu-defmethod foo (&rest args) :when [a b _] body)

;; TODO Allow predicates in patterns
;; degenerate case where computed multi val maybe a seq, pred-p should still be
;; applied even though this here val isn't a seq
;; (mu-defmethod foo (&rest args) :when (?  pred-p) body)

;; TODO Hierarchy is orthogonal to `multi' dispatch function. However in Clojure
;; you may change it (only?) in `defmulti', but IMO it makes more sence to be able
;; to pass it to mu-defmethod invocations (not even definitions). Need to think if
;; that'd be consistent and whether it has any practical value.

;; TODO Could we allow arbitrary relations? E.g. `parent-of'. Would that have any
;; practical benefit? When? How?
;; (mu-defmethod foo (a b) :isa :b body)
;; (mu-defmethod foo (a b) :parent-of :b body)
