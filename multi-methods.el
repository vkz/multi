;; -*- lexical-binding: t; -*-

;; Copyright (C) 2018, 2019 by Vlad Kozin

(require 'multi-prelude)
(eval-and-compile
  (require 'multi-patterns))


;;* edebug-specs -------------------------------------------------- *;;


;; TODO spec all macros in here espcially `mu-defmulti' and `mu-defmethod'


;; TODO first attempt at debug decl. Missing ARGLIST is a `mu-function?' case
(def-edebug-spec mu-defmethod
  (&define name mu-defun-arglist
           [&rest [keywordp sexp]]
           mu-defun-body))


;;* Multi --------------------------------------------------------- *;;


(cl-defstruct multi
  name
  dispatch
  (methods (ht))
  (prefers (ht))
  default
  hierarchy
  static-hierarchy)


(defmacro multi (struct slot &rest keys)
  "Convenience macro to get and set slots in `multi' struct
instances. For any slot that's a hash-table pass KEYS to get or
set corresponding value."
  (declare
   (gv-setter
    (lambda (val)
      (if keys
          ;; get table at SLOT and set the value at KEYS
          `(setf (ht-get* (cl-struct-slot-value 'multi ',slot ,struct) ,@keys) ,val)
        ;; set SLOT value
        `(setf (cl-struct-slot-value 'multi ',slot ,struct) ,val)))))
  (if keys
      ;; get the table at SLOT and lookup value at KEYS
      `(ht-get* (cl-struct-slot-value 'multi ',slot ,struct) ,@keys)
    ;; get the value at SLOT
    `(cl-struct-slot-value 'multi ',slot ,struct)))


;;* Hierarchies --------------------------------------------------- *;;


(defstruct mu-hierarchy
  ;; TODO should this be an UUID or gensym is enough to avoid collisions?
  (id (gensym "mu-hierarchy"))
  ;; table: dispatch-val => (ht :parents :children :ancestors :descendants)
  (table (ht))
  (cache (ht)))


(defmacro mu-hierarchy (struct slot &rest keys)
  "Convenience macro to get and set slots in `mu-hierarchy'
struct instances. For any slot that's a hash-table pass KEYS to
get or set corresponding value."
  (declare
   (gv-setter
    (lambda (val)
      (if keys
          ;; get table at SLOT and set the value at KEYS
          `(setf (ht-get* (cl-struct-slot-value 'mu-hierarchy ',slot ,struct) ,@keys) ,val)
        ;; set SLOT value
        `(setf (cl-struct-slot-value 'mu-hierarchy ',slot ,struct) ,val)))))
  (if keys
      ;; get the table at SLOT and lookup value at KEYS
      `(ht-get* (cl-struct-slot-value 'mu-hierarchy ',slot ,struct) ,@keys)
    ;; get the value at SLOT
    `(cl-struct-slot-value 'mu-hierarchy ',slot ,struct)))


(defconst mu-global-hierarchy (make-mu-hierarchy)
  "Global hierarchy")


(defvar mu--hierarchy-override nil
  "Hierarchy that overrides the global if set")


(defsubst mu-active-hierarchy (&optional fun id?)
  "Return the hierarchy active in the current dynamic extent."
  (let ((hierarchy (or
                    ;; static hierarchy tramps all and cannot be overriden
                    (when fun (multi fun static-hierarchy))
                    ;; dynamic hierarchy override trams both custom and global
                    mu--hierarchy-override
                    ;; custom hierarchy tramps global
                    (when fun (multi fun hierarchy))
                    ;; global hierarchy
                    mu-global-hierarchy)))
    (if id? (mu-hierarchy-id hierarchy) hierarchy)))


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
                       (mu-hierarchy hierarchy table parent :parents))))
      (cons parent cycle))))


(defun mu--cycle? (child parent &optional hierarchy compute?)
  "Check if CHILD and PARENT would create a cycle in the
currently active hierarchy and return it."
  (default hierarchy :to (mu-active-hierarchy))
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
;; implementation trades off time pre-computing relations whenever a hierarchy
;; gets extended for the the time required to perform `mu-isa?' check, obviously
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
    (pushnew parent (mu-hierarchy hierarchy table child :parents))
    ;; add parent to child's :ancestors
    (pushnew parent (mu-hierarchy hierarchy table child :ancestors))
    ;; extend child's :ancestors with parent's :ancestors
    (callf cl-union (mu-hierarchy hierarchy table child :ancestors)
      (mu-hierarchy hierarchy table parent :ancestors))
    ;; propagate now extended child's :ancestors down the family tree by
    ;; extending every child descendant's :ancestors with child's
    ;; :ancestors
    (dolist (descendant (mu-hierarchy hierarchy table child :descendants))
      (callf cl-union (mu-hierarchy hierarchy table descendant :ancestors)
        (mu-hierarchy hierarchy table child :ancestors))))

  ;; Update parent and its ancestors
  (progn
    ;; add child to parent's :children
    (pushnew child (mu-hierarchy hierarchy table parent :children))
    ;; add child to parent's :descendants
    (pushnew child (mu-hierarchy hierarchy table parent :descendants))
    ;; extend parent's :descendants with child's :descendants
    (callf cl-union (mu-hierarchy hierarchy table parent :descendants)
      (mu-hierarchy hierarchy table child :descendants))
    ;; propagate now extended parent's :descendants up the family tree by
    ;; extending every parent ancestor's :descendants with parent's
    ;; :descendants
    (dolist (ancestor (mu-hierarchy hierarchy table parent :ancestors))
      (callf cl-union (mu-hierarchy hierarchy table ancestor :descendants)
        (mu-hierarchy hierarchy table parent :descendants))))

  ;; return hierarchy
  hierarchy)


(defmacro mu-rel (child isa parent &optional hierarchy)
  "Establish an isa relationship between CHILD and PARENT in the
currently active hierarchy or HIERARCHY.

(mu-rel CHILD REL PARENT [HIERARCHY])
-------------------------------------
    CHILD = val
      REL = :isa | isa | any
   PARENT = val
HIERARCHY = mu-hierarchy-p
-------------------------------------

REL argument is provided to help readability but is otherwise
ignored."
  `(mu--rel ,child ,parent (or ,hierarchy (mu-active-hierarchy))))


(defun mu-isa? (child parent &optional hierarchy)
  "Check if CHILD is isa? related to PARENT in the currently
active hierarchy or HIERARCHY."
  (default hierarchy :to (mu-active-hierarchy))
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
  (let ((parents (mu-hierarchy hierarchy table x :parents)))
    ;; TODO instead of append I could cl-union to avoid cl-delete-duplicates use
    ;; later, but this is fine too?
    (append parents
            (seq-mapcat
             (lambda (parent)
               (mu--ancestors parent hierarchy))
             parents))))


(defun mu--descendants (x hierarchy)
  "Return descendants of X by walking the HIERARCHY tree."
  (let ((children (mu-hierarchy hierarchy table x :children)))
    (append children
            (seq-mapcat
             (lambda (child)
               (mu--descendants child hierarchy))
             children))))


(defun mu-ancestors (x &optional hierarchy compute?)
  "Return all ancestors of X such that (mu-isa? X ancestor) in
the currently active hierarchy or HIERARCHY."
  (default hierarchy :to (mu-active-hierarchy))
  (if compute?
      (cl-delete-duplicates (mu--ancestors x hierarchy) :test #'equal)
    (mu-hierarchy hierarchy table x :ancestors)))


(defun mu-descendants (x &optional hierarchy compute?)
  "Return all descendants of X such that (mu-isa? descendant X)
in the currently active hierarchy or HIERARCHY."
  (default hierarchy :to (mu-active-hierarchy))
  (if compute?
      (cl-delete-duplicates (mu--descendants x hierarchy) :test #'equal)
    (mu-hierarchy hierarchy table x :descendants)))


(cl-defun mu--generations (seqx seqy hierarchy)
  (let ((rels (seq-mapn (lambda (x y) (mu-isa/generations? x y hierarchy)) seqx seqy)))
    (and (cl-notany #'null rels)
         rels)))


(cl-defun mu-isa/generations? (x y &optional (hierarchy) (generation 0))
  "Like `mu-isa?' but return the generation gap between CHILD and
PARENT."
  (default hierarchy :to (mu-active-hierarchy))
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

   ((member y (mu-hierarchy hierarchy table x :parents))
    (cons :generation (1+ generation)))

   (:else
    (some
     (lambda (parent) (mu-isa/generations? parent y hierarchy (1+ generation)))
     (mu-hierarchy hierarchy table x :parents)))))


;;* Prefers ------------------------------------------------------- *;;


(defun mu-prefers (fun &rest keys)
  "Return a table of registered value preferences. When VAL is
supplied return just the set of all values over which VAL is
preferred. This form is `setf'-able.

\(mu-prefers fun &optional val)"
  (declare
   (gv-setter
    (lambda (val)
      `(setf (multi ,fun prefers (mu-active-hierarchy ,fun :id) ,@keys) ,val))))

  ;; return set of values VAL tramps or the entire prefers table
  (let* ((id (mu-active-hierarchy fun :id))
         (prefers (or (multi fun prefers id)
                      (setf (multi fun prefers id) (ht)))))
    (if keys (apply #'ht-get* prefers keys) prefers)))


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
  "Prefer dispatch value X over Y ..."
  ([_ x y] (if-let ((cycle (mu--preference-cycle? x y fun)))
               (mu-error :cyclic-prefer x y cycle)
             (pushnew y (mu-prefers fun x))))

  ([_ x (or :to :over 'to 'over) y] (mu-prefer fun x y))

  (otherwise
   (mu-error :malformed-prefer args)))


(mu-defun mu-unprefer (fun &rest args)
  "Remove registered preferences for FUN ..."
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
        (mu-error :inconsistent-prefers fun val (mu-active-hierarchy fun) prefers))))))


;;* Methods ------------------------------------------------------- *;;


;; TODO `mu--select-preferred' feels redundant now, should just move its code into
;; `mu-select-method'.
(defun mu-select-method (fun val)
  "Select a multi-method whose value (isa? VAL value)."
  (let* ((hierarchy (mu-active-hierarchy fun))
         (methods   (ht-select (lambda (VAL method) (mu-isa? val VAL hierarchy))
                               (multi fun methods))))
    (if (ht-empty? methods)
        ;; user-installed or pre-installed default
        (or (multi fun methods :default)
            (multi fun default))
      ;; narrow to just one method or throw
      (mu--select-preferred fun methods val))))


;; TODO defmulti should allow :interactive declaration, maybe others


(defmacro mu-defmulti (name arglist &optional docstring &rest body)
  "Define a new multi-dispatch function NAME ..."
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
                     `(mu-error :malformed-defmulti ',name))))
         (args (gensym "args"))
         (val (gensym "val"))
         (default `(lambda (&rest ,args)
                     (mu-error :no-methods (apply (multi ,name dispatch) ,args) ',name))))
    `(progn

       ;; check if lexical binding is enabled
       (mu-lexical-binding)

       ;; create a dispatch function for NAME
       (defun ,name (&rest ,args)
         ,docstring
         (let* ((,val (apply (multi ,name dispatch) ,args)))
           (apply (mu-select-method ,name ,val) ,args)))

       ;; create a multi struct for NAME and set NAME value to that struct
       (setf ,name (make-multi
                    :name            ',name
                    :dispatch         ,dispatch
                    :methods          (ht)
                    :prefers          (ht)
                    :default          ,default
                    :hierarchy        ,(ht-get attrs :hierarchy)
                    :static-hierarchy ,(ht-get attrs :static-hierarchy)))

       ;; TODO Invalidate mu-methods cache here. Need to do this to catch
       ;; cases where fun simply gets redefined and may hold cache for previous
       ;; dispatch function
       )))


;; NOTE Far as I can tell if I were to define some methods with mu-defmethod then
;; compile that file and load it mu-methods table will have compiled lambdas, so
;; that's good. But the (byte-compile (mu-defmethod ...)) doesn't seem to install
;; compiled lambdas, not sure why.


(defmacro mu-defmethod (name arglist _when val &rest body)
  "Add a new method to multi-dispatch function NAME ..."
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
       ;; add new method
       (setf (multi ,name methods ,val) ,method)
       ;; TODO invalidate mu-methods cache
       )))


(defun mu-undefmethod (fun val)
  "Remove multi-method for FUN and dispatch value VAL"
  (ht-remove! (multi fun methods) val))


;;* Docs --------------------------------------------------------- *;;


(mu-docfun mu-prefer
  "Prefer dispatch value X over Y when resolving method FUN.

(mu-prefer FUN ARGS ...)
------------------------
     FUN = id

ARGS ... = val :to val
         | val :over val
         | val val
------------------------

\(fn fun x :over y)")


(mu-docfun mu-unprefer
  "Remove registered preferences for FUN multi-dispatch function:

(mu-unprefer FUN ARGS ...)
--------------------------
     FUN = id

ARGS ... = val :to val
         | val :over val
         | val val
         | val
         |
--------------------------

Called with a single VAL argument removes all preferences defined
for the dispatch VAL; called with just FUN removes all known
preferences for FUN.

\(fn foo x :over y)")


(mu-docfun mu-defmulti
  "Define a new multi-dispatch function NAME.

--------------------------------------------------
        ARGLIST = cl-arglist
                | seq-pattern
                | mu-function?

           BODY = [metadata] clause ...

         clause = body
                | mu-defun-clause ...

       metadata = :hierarchy mu-hierarchy-p
                | :static-hierarchy mu-hierarchy-p

mu-defun-clause = (seq-pattern body ...)

    seq-pattern = `['mu-pattern ...`]'
--------------------------------------------------

ARGLIST maybe a CL-ARGLIST, a function (#'function, `lambda',
`mu' lambda) or a sequence []-pattern. When ARGLIST is itself a
function, BODY is ignored and that function is used to dispatch.
ARGLIST and BODY combined may follow single-head or multi-head
syntax to define a `mu-defun' for dispatch and destructuring.

BODY must return a value to be used for `mu-isa?' dispatch.")


(mu-docfun mu-defmethod
  "Add a new method to multi-dispatch function NAME for dispatch
value VAL.

----------------------------------------
        ARGLIST = cl-arglist
                | seq-pattern
                | mu-function?

           BODY = clause ...

         clause = body
                | mu-defun-clause ...

mu-defun-clause = (seq-pattern body ...)

    seq-pattern = `['mu-pattern ...`]'
----------------------------------------

ARGLIST maybe a `cl-arglist', a function (#'function, `lambda',
`mu' lambda) or a sequence []-pattern. ARGLIST and BODY combined
may follow single-head or multi-head syntax to define a
`mu-defun' for dispatch and destructuring.")


;;* Provide ------------------------------------------------------ *;;


(provide 'multi-methods)

;; TODO Using list as a set is dumb and f-ing slow for membership lookup. Could I
;; just fake a set as a hash-table {:member t, ...}? That would speed up
;; membership lookup in methods, hierarchies, prefers.

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
;;
;; First mu-defmulti should support :before :after that fire no matter what method
;; except perhaps :default. Every method should also allow :before that runs after
;; defmulti's :before and :after that runs before defmulti's :after. In simplest
;; case :before and :after aren't separate methods but attributes on the multi and
;; methods that simply specify a lisp-form to execute with args in scope. We can
;; also define full-fledged before and after methods like so:
(comment
 (mu-defmulti  foo #'vector
   :before (do before)
   :after (do after))
 (mu-defmethod foo (arg) :when [:foo]
   :after (do after))
 (mu-defmethod foo (arg) :before [:bla] body)
 (mu-defmethod foo (arg) :when [:bla] body)
 (mu-defmethod foo (arg) :after [:bla] body)
 ;; comment
 )

;; TODO maybe nice to have `mu-call-next-method' a-la `cl-call-next-method'.
;; Typical pattern would be to delegate to :default. This calls for semantics to
;; order methods by priority and specificity, I guess.

;; TODO Think about reasonable and practical global-hierarchy, e.g. one that works
;; for structs, isa relationship between predicates, maybe even eieio classes
;; although I have no experienc with those.

;; TODO Allow isa? with "_" patterns
;; (mu-defmethod foo (&rest args) :when [a b _] body)
;;
;; Here's a possible semantics:
;;
;; Any value V isa _. Then suppose we have two methods:
;;
;;   (mu-defmulti foo #'vector)
;;   (mu-defmethod foo _ :when [a b])
;;   (mu-defmethod foo _ :when [a '_])
;;
;; (foo a b) isa [a b] and
;; (foo a b) isa [a '_] because b isa '_.
;;
;; We can resolve this ambiguity by always choosing the most precise match. To
;; that effect we must flag any dispatch values with '_ we store in mu-methods so
;; we can tell them from other values. Then the can resolve above ambiguity:
;; 1. drop all flagged dispatch values with '_
;; 2-a. if a single method left ([a b] here) use that,
;; 2-b. if more than one methods left, resolve with preferred methods
;; 2-c. if no methods left then go-to 3
;; 3-a. if only one method with '_ match, use that
;; 3-b. if more than one method with '_, resolve with preferred methods,
;; 3-c. no method is preferred - raise error

;; TODO Allow predicates in patterns
;; degenerate case where computed multi val maybe a seq, pred-p should still be
;; applied even though this here val isn't a seq
;; (mu-defmethod foo (&rest args) :when (?  pred-p) body)

;; TODO Could we allow arbitrary relations? E.g. `parent-of'. Would that have any
;; practical benefit? When? How?
;; (mu-defmethod foo (a b) :isa :b body)
;; (mu-defmethod foo (a b) :parent-of :b body)
