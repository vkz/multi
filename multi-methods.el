;; -*- lexical-binding: t; -*-


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
  "Control if mu-methods can be defined when `lexical-binding'
 is disabled. Default to signaling an error if an attempt is made
 to define a new multi dispatch or method while in a dynamically
 scoped environment.")


(defun mu-lexical-binding ()
  "Signal an error depending on the setting of
`mu-lexical-binding' and `lexical-binding'."
  (when mu-lexical-binding
    (unless lexical-binding
      (mu-error
       (string-join
        (list
         "mu-methods require `lexical-binding' to work properly."
         "If you know what you are doing you may disable this check"
         "by unsetting `mu-lexical-binding'.")
        " ")))))


;;* Hierarchies --------------------------------------------------- *;;


(defstruct mu-hierarchy
  ;; TODO should this be an UUID or gensym is enough to avoid collisions?
  (id (gensym "mu-hierarchy"))
  (table (ht))
  (cache (ht)))


(defun mu-hierarchy (hierarchy &rest keys)
  "Returns the value in the HIERARCHY's nested table, where KEYS
is a sequence of keys. Returns nil if the key is not present.
Without KEYS returns the entire table. Calls are `setf'-able."
  (declare
   (gv-setter (lambda (val)
                `(setf (ht-get* (mu-hierarchy-table ,hierarchy) ,@keys) ,val))))
  (if keys
      (apply #'ht-get* (mu-hierarchy-table hierarchy) keys)
    (mu-hierarchy-table hierarchy)))


(defconst mu-global-hierarchy (make-mu-hierarchy)
  "Global table that holds global hierachy. Has the following
structure:

  (ht (VAL (ht (:parents (p ...)) (:children (c ...)))) ...)")


(defun mu-global-hierarchy (&rest keys)
  "Returns the value in the global hierarchy nested table, where
KEYS is a sequence of keys. Returns nil if the key is not
present. Without KEYS returns the entire table. Calls are
`setf'-able."
  (declare
   (gv-setter (lambda (val)
                `(setf (mu-hierarchy mu-global-hierarchy ,@keys) ,val))))
  (apply #'mu-hierarchy mu-global-hierarchy keys))


;; TODO is this code correct?
(defun mu--cycle (child parent hierarchy)
  "Reports the cycle that the CHILD - PARENT relation would've
created in HIERARCHY if installed, nil if no cycle."
  (if (equal child parent)
      (list parent)
    (when-let ((cycle (some
                       (lambda (ancestor) (mu--cycle child ancestor hierarchy))
                       (mu-hierarchy hierarchy parent :parents))))
      (cons parent cycle))))


(defun mu--cycle? (child parent &optional hierarchy compute?)
  "Checks if CHILD and would be PARENT would form a cycle were
the relationship added to the HIERARCHY. If HIERARCHY not
supplied defaults to the global hierarchy. If COMPUTE? is t
actually computes PARENT's ancestors for the check instead of
using already stored in the HIERARCHY."
  (default hierarchy :to mu-global-hierarchy)
  (default compute? :to nil)
  (when (or (equal child parent)
            (member child
                    (if compute?
                        (mu-ancestors parent hierarchy 'compute)
                      (mu-ancestors parent hierarchy))))
    ;; compute and report full cycle for debugging
    (mu--cycle child parent hierarchy)))


(defun mu--rel (child parent hierarchy)
  "Installs CHILD - PARENT relation in HIERARCHY, propagates any
necessary :descendant - :ancestor relations up and down the
HIERARCHY tree. Returns updated HIERARCHY."

  ;; there's no meaningful semantics to relate structured data
  (when (or (ht-p child)
            (ht-p parent)
            (and (seqp child) (not (null child)))
            (and (seqp parent) (not (null parent)))
            (cl-struct-p child)
            (cl-struct-p parent))
    (mu-error "in mu-rel no meaningful semantics relate structured data\n  %s\n  %s"
                 child parent))

  ;; don't allow cyclic relations
  (when-let ((cycle (mu--cycle? child parent hierarchy)))
    (mu-error "in mu-rel cyclic relationship between %s and %s: %s"
                 child parent cycle))

  ;; TODO fixing :ancestors and :descendants is too easy to get wrong, as should
  ;; be obvious from the code below. Another thing we could do is to recompute
  ;; :ancestors and :descendants for every item in hierarchy by invoking e.g.
  ;; (mu-ancestors item hierarchy 'compute). That'd result in a ton of
  ;; redundant computation, but we could simply memoize `mu-ancestors' and
  ;; `mu-descendants'. Their cache would have to be wiped every new relation
  ;; installed with mu-rel, naturally. IMO it'd be performant enough and much
  ;; more readable than the code below.

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


(defmacro mu-rel (&rest args)
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
         (list `(mu-error "in mu-rel malformed arglist at %s" ',args) nil nil nil)))
    (or
     err
     (let ((hierarchy (or hierarchy 'mu-global-hierarchy)))
       `(mu--rel ,child ,parent ,hierarchy)))))

(comment
 (mu-defmacro mu-rel (&rest args)
   :doc "docstring"
   :sig "(fn child :isa parent &optional :in hierarchy)"
   ([child (or 'isa :isa) parent]
    (mu-rel child parent 'mu-global-hierarchy))

   ([child (or 'isa :isa) parent (or 'in :in) hierarchy]
    (mu-rel child parent hierarchy))

   ([child parent hierarchy]
    `(mu--rel ,child ,parent ,hierarchy))

   (otherwise
    `(mu-error "in mu-rel malformed arglist at %s" ',args)))
 ;; comment
 )


(defun mu-isa? (x y &optional hierarchy)
  "Checks if CHILD is isa? related to PARENT in HIERARCHY. If
HIERARCHY not supplied defaults to the global hierarchy

\(fn child parent &optional hierarchy)"
  (default hierarchy :to mu-global-hierarchy)
  (or (equal x y)
      (and (sequencep x)
           (sequencep y)
           (equal (length x) (length y))
           (every (lambda (x y) (mu-isa? x y hierarchy)) x y))
      (and (not (sequencep y))
           (member y (mu-ancestors x hierarchy)))))


(cl-defun mu--generations (seqx seqy hierarchy)
  (let ((rels (seq-mapn (lambda (x y) (mu-isa/generations? x y hierarchy)) seqx seqy)))
    (and (cl-notany #'null rels)
         rels)))


(cl-defun mu-isa/generations? (x y &optional (hierarchy) (generation 0))
  "Returns (generation 0) if (equal CHILD PARENT), or (generation
N) if CHILD is directly or indirectly derived from PARENT, where
N signifies how far down generations PARENT is from CHILD. If
HIERARCHY not supplied defaults to the global hierarchy

\(fn child parent &optional hierarchy)"
  (default hierarchy :to mu-global-hierarchy)
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


(defun mu--ancestors (x hierarchy)
  "Returns ancestors of X by walking the HIERARCHY tree. List may
have duplicates."
  (let ((parents (mu-hierarchy hierarchy x :parents)))
    ;; TODO instead of append I could cl-union to avoid cl-delete-duplicates use
    ;; later, but this is fine too?
    (append parents
            (seq-mapcat
             (lambda (parent)
               (mu--ancestors parent hierarchy))
             parents))))


(defun mu-ancestors (x &optional hierarchy compute?)
  "Returns all ancestors of X such that (mu-isa? X ancestor).
If HIERARCHY not supplied defaults to the global hierarchy. If
COMPUTE? is t actually computes ancestors by walking the tree."
  (default hierarchy :to mu-global-hierarchy)
  (default compute? :to nil)
  (if compute?
      (cl-delete-duplicates (mu--ancestors x hierarchy) :test #'equal)
    (mu-hierarchy hierarchy x :ancestors)))


(defun mu--descendants (x hierarchy)
  "Returns descendants of X by walking the HIERARCHY tree. List
may have duplicates."
  (let ((children (mu-hierarchy hierarchy x :children)))
    (append children
            (seq-mapcat
             (lambda (child)
               (mu--descendants child hierarchy))
             children))))


(defun mu-descendants (x &optional hierarchy compute?)
  "Returns all descendants of X such that (mu-isa? descendant
X). If HIERARCHY not supplied defaults to the global hierarchy.
If COMPUTE? is t actually computes descendants by walking the
tree."
  (default hierarchy :to mu-global-hierarchy)
  (default compute? :to nil)
  (if compute?
      (cl-delete-duplicates (mu--descendants x hierarchy) :test #'equal)
    (mu-hierarchy hierarchy x :descendants)))


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
the (:default . method) if none match. If HIERARCHY not supplied
defaults to the global hierarchy.

When called without :for and :matching keywords returns the
entire table or the value nested in it if the sequence of KEYS is
supplied. This form is `setf'-able.

May be called according to one of the following signatures:

  (mu-methods :for fun-sym :matching val &optional :in hierarchy)
  (mu-methods fun-sym &rest keys)
  (setf (mu-methods fun-sym &rest keys) val)

\(fn :for fun-sym :matching val &optional :in hierarchy)"
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
                   `(mu-error "in mu-methods malformed arglist at %s" ',args))))))
  ;; parse args
  (pcase args
    ;; (mu-methods :for 'fun :matching val)
    (`(:for ,fun :matching ,val)
     (mu-methods :for fun :matching val :in mu-global-hierarchy))

    ;; (mu-methods :for 'fun :matching val :in hierarchy)
    (`(:for ,fun :matching ,val :in ,hierarchy)
     (let* ((mu-methods
             (get fun :mu-methods))

            (methods
             (ht-select (fn (VAL _) (mu-isa? val VAL hierarchy)) mu-methods))

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
     (mu-error "in mu-methods malformed arglist at %s" args))))


(comment
 (mu-defun mu-methods (&rest args)
   :doc "docstring"
   :sig "(fn :for fun-sym :matching val &optional :in hierarchy)"
   ([:for fun :matching val]
    (mu-methods :for fun :matching val :in mu-global-hierarchy))

   ([:for fun :matching val :in hierarchy]
    (let* ((mu-methods
            (get fun :mu-methods))

           (methods
            (ht-select (fn (VAL _) (mu-isa? val VAL hierarchy)) mu-methods))

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

   ([(multi fun-symbol :if symbolp) &rest keys]
    (apply #'mu--methods fun-symbol keys))

   (otherwise
    (mu-error "in mu-methods malformed arglist at %s" args))

   :gv
   ([(multi fun :if mu-rel?) &rest keys]
    `(setf (mu--methods ',(sym fun) ,@keys) ,gv))

   (otherwise
    `(mu-error "in mu-methods malformed arglist at %s" ',args)))
 ;; comment
 )


;; NOTE this naming `mu-methods-remove' becomes more consistent if or when we
;; allow :before :after :around methods
(defun mu-methods-remove (fun dispatch-value)
  "Removes the mu-method FUN associated with DISPATCH-VALUE."
  (ht-remove! (mu-methods fun) dispatch-value))


;;* Prefers ------------------------------------------------------- *;;


;; TODO make hierarchy optional defaulting to the global hierarchy
(defun mu-prefers (fun hierarchy &rest keys)
  "Returns a table of (preferred value :over set of other values)
in the hierarchy. If VAL supplied returns just tha set of other
values over which VAL is preferred. This form is `setf'-able.

\(mu-prefers fun hierarchy &optional val)"
  (declare
   (gv-setter (lambda (val)
                (if (cdr keys)
                    `(mu-error "in mu-prefers malformed arglist expected no more than one key, given %s" ',keys)
                  `(setf (ht-get* (get ,fun :mu-prefers) (mu-hierarchy-id ,hierarchy) ,@keys) ,val)))))
  (if (cdr keys)
      ;; expect no more than one argument (VAL to prefer over others)
      (mu-error "in mu-prefers malformed arglist expected no more than one key, given %s" keys)
    ;; return list of values over which (car keys) is preferred in hierarchy, or
    ;; the entire prefers table for hierarchy if no keys supplied
    (apply #'ht-get* (get fun :mu-prefers) (mu-hierarchy-id hierarchy) keys)))


(comment
 (mu-defun mu-prefers (fun hierarchy &rest keys)
   :doc "docstring"
   :sig "(mu-prefers fun hierarchy &optional val)"
   ([val]
    (apply #'ht-get* (get fun :mu-prefers) (mu-hierarchy-id hierarchy) keys))

   (otherwise
    (mu-error
     "in mu-prefers malformed arglist expected no more than one key, given %s"
     keys))

   :gv
   ([val]
    `(setf (ht-get* (get ,fun :mu-prefers) (mu-hierarchy-id ,hierarchy) ,@keys) ,gv))

   (otherwise
    (mu-error
     "in mu-prefers malformed arglist expected no more than one key, given %s"
     ',keys)))
 ;; comment
 )


(defun mu--preference-cycle? (item parent fun &optional hierarchy)
  "Checks if ITEM prefer over PARENT would form a cycle were the
relationship added the mu-prefers of FUN assuming HIERARCHY
and return that cycle. If HIERARCHY not supplied defaults to the
global hierarchy"
  (default hierarchy :to mu-global-hierarchy)
  (if (equal item parent)
      (list parent)
    (when-let ((cycle (some
                       (lambda (ancestor) (mu--preference-cycle? item ancestor fun hierarchy))
                       (mu-prefers fun hierarchy parent))))
      (cons parent cycle))))


(comment
 (mu-test (baz)
   (multi baz #'identity)
   (mu-prefer 'baz [:rect :shape] :over [:shape :rect])
   (mu-prefer 'baz [:shape :rect] :over [:parallelogram :rect])
   ;; (mu-prefer 'baz [:parallelogram :rect] :over [:rect :shape])
   (mu--preference-cycle? [:parallelogram :rect] [:rect :shape] 'baz mu-global-hierarchy))
 ;; comment
 )


(defun mu-prefer (fun &rest args)
  "Causes the mu-method FUN to prefer matches of dispatch VAL-X
over dispatch VAL-Y when there is a conflict.

May be called according to one of the following signatures:

  (mu-prefer foo val-x :to val-y &optional :in hierarchy)
  (mu-prefer foo val-x val-y &optional :in hierarchy)

\(mu-prefer foo val-x :over val-y &optional :in hierarchy)"
  (destructuring-bind
      (x y hierarchy)
      (pcase args

        ;; (mu-prefer foo x :over y :in hierarchy)
        (`(,x ,(or :over :to) ,y . ,(or `(:in ,hierarchy) '()))
         (list x y (or hierarchy mu-global-hierarchy)))

        ;; (mu-prefer foo x y :in hierarchy)
        (`(,x ,y . ,(or `(:in ,hierarchy) '()))
         (list x y (or hierarchy mu-global-hierarchy)))

        (otherwise
         (mu-error "in mu-prefer malformed arglist at %s" args)))

    ;; installing the preference mustn't create a cycle in mu-prefers
    (when-let ((cycle (mu--preference-cycle? x y fun hierarchy)))
      (mu-error "in mu-prefer cyclic preference %s over %s would form a cycle %s"
                   x y cycle))

    ;; install the preference x :over y
    (pushnew y (mu-prefers fun hierarchy x))))


(comment
 (mu-defun mu-prefer (fun &rest args)
   :doc "string"
   :sig "(mu-prefer foo val-x :over val-y &optional :in hierarchy)"
   ([x (or :over :to) y :in hierarchy] (mu-prefer x y hierarchy))
   ([x (or :over :to) y] (mu-prefer x y mu-global-hierarchy))

   ([x y :in hierarchy] (mu-prefer x y hierarchy))
   ([x y] (mu-prefer x y mu-global-hierarchy))
   ([x y hierarchy]
    (when-let ((cycle (mu--preference-cycle? x y fun hierarchy)))
      (mu-error "in mu-prefer cyclic preference %s over %s would form a cycle %s"
                x y cycle))

    (pushnew y (mu-prefers fun hierarchy x)))

   (otherwise
    (mu-error "in mu-prefer malformed arglist at %s" args)))
 ;; comment
 )


;; TODO We don't really need to implement `mu-prefers-remove' because we've
;; already made `mu-prefers' `setf'-able, so the following should work:
;;
;; (setf (mu-prefers foo hierachy [:rect :shape]) '(some-values)
;; (ht-remove! (mu-prefers foo hierachy) [:rect :shape])
;;
;; Does providing `mu-prefers-remove' make things more consistent? I should
;; make a note about removing prefers in documentation regardless.


(defun mu-prefers-remove (fun &rest args)
  "Causes the mu-method FUN to not prefer matches of dispatch
VAL-X over dispatch VAL-Y when there is a conflict. If VAL-Y not
supplied removes all prefers for VAL-X. If HIERARCHY not supplied
defaults to the global hierarchy.

May be called according to one of the following signatures:

  (mu-prefers-remove foo val-x :to val-y &optional :in hierarchy)
  (mu-prefers-remove foo val-x val-y &optional :in hierarchy)
  (mu-prefers-remove foo val-x &optional :in hierarchy)
  (mu-prefers-remove foo &optional :in hierarchy)

\(mu-prefers-remove foo val-x :over val-y &optional :in hierarchy)"
  (pcase args

    ;; TODO fml matching order here matters! If we reorder earlier cases may fire.
    ;; This adds complexity that I don't like. Do I really want to offer this
    ;; calling freedom?

    ;; (mu-prefers-remove foo &optional :in hierarchy)
    ((or `(:in ,hierarchy) `())
     (setf (mu-prefers fun (or hierarchy mu-global-hierarchy)) (ht)))

    ;; (mu-prefers-remove foo x :over y &optional :in hierarchy)
    (`(,x ,(or :over :to) ,y . ,(or `(:in ,hierarchy) '()))
     ;; remove just the Y value from X's prefers
     (cl-callf2 remove y (mu-prefers fun (or hierarchy mu-global-hierarchy) x)))

    ;; (mu-prefers-remove foo x &optional :in hierarchy)
    (`(,x . ,(or `(:in ,hierarchy) '()))
     ;; remove all prefers for X
     (ht-remove! (mu-prefers fun (or hierarchy mu-global-hierarchy)) x))

    ;; (mu-prefers-remove foo x y &optional :in hierarchy)
    (`(,x ,y . ,(or `(:in ,hierarchy) '()))
     ;; remove just the Y value from X's prefers
     (cl-callf2 remove y (mu-prefers fun (or hierarchy mu-global-hierarchy) x)))

    (otherwise
     (mu-error "in mu-prefers-remove malformed arglist at %s" args))))


(comment
 (mu-defun mu-prefers-remove (fun &rest args)
   :doc "string"
   :sig "(mu-prefers-remove foo val-x :over val-y &optional :in hierarchy)"
   ([]
    (setf (mu-prefers fun (or hierarchy mu-global-hierarchy)) (ht)))

   ([x (or :over :to) y :in hierarchy]
    (mu-prefers-remove fun x y hierarchy))

   ([x (or :over :to) y]
    (mu-prefers-remove fun x y mu-global-hierarchy))

   ([x :in hierarchy]
    (ht-remove! (mu-prefers fun hierarchy) x))

   ([x]
    (ht-remove! (mu-prefers fun mu-global-hierarchy) x))

   ([x y :in hierarchy]
    (mu-prefers-remove x y hierarchy))

   ([x y]
    (mu-prefers-remove x y mu-global-hierarchy))

   ([x y hierarchy]
    (cl-callf2 remove y (mu-prefers fun hierarchy x)))

   (otherwise
    (mu-error "in mu-prefers-remove malformed arglist at %s" args)))
 ;; comment
 )


(comment
 (mu-defun mu-prefers-remove (fun &rest args)
   :doc "string"
   :sig "(mu-prefers-remove foo val-x :over val-y &optional :in hierarchy)"
   ((or [:in hierarchy] [])
    (setf (mu-prefers fun (or hierarchy mu-global-hierarchy)) (ht)))

   ([x (or :over :to) y &rest (or [:in hierarchy] [])]
    (mu-prefers-remove fun x y (or hierarchy mu-global-hierarchy)))

   ([(x &rest (or [:in hierarchy] []))]
    ;; remove all prefers for X
    (ht-remove! (mu-prefers fun (or hierarchy mu-global-hierarchy)) x))

   ;; (mu-prefers-remove foo x y &optional :in hierarchy)
   ([x y &rest (or [:in hierarchy] [])]
    ;; remove just the Y value from X's prefers
    (mu-prefers-remove fun x y (or hierarchy mu-global-hierarchy)))

   ([x y hierarchy]
    (cl-callf2 remove y (mu-prefers fun (or hierarchy mu-global-hierarchy) x)))

   (otherwise
    (mu-error "in mu-prefers-remove malformed arglist at %s" args)))
 ;; comment
 )


(defun mu--select-preferred (fun methods hierarchy dispatch-val)
  "Narrows METHODS matching DISPATCH-VAL down to a single method
based on preferences registered for multidispatch FUN and
HIERARCHY. Returns that method or signals an error."
  (if (= (ht-size methods) 1)

      ;; just one method, no ambiguity
      (car (ht-values methods))

    ;; multiple methods matching dispatch-val, use preferences to resolve
    (let* ((prefers (mu-prefers fun hierarchy))

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
        (mu-error
         "multiple methods match dispatch value %s for dispatch %s:\n%s\n%s"
         dispatch-val fun
         (string-join
          (ht-map (fn (VAL _) (format "  %s :isa %s" dispatch-val VAL)) preferred)
          "\n")
         "and neither is preferred"))

       ;; no methods at all can only ever happen if the user managed to register
       ;; preferences that are mutually inconsitent i.e. create a cycle. If this
       ;; ever happens, our `mu--preference-cycle?' must be buggy.
       ((= size 0)
        (mu-error
         (string-join
          (list "inconsintency in registered mu-prefers unable to prefer any of" "  %s"
                "with registered preferences" "  %s") "\n")
         (ht-keys methods)
         prefers))))))


;;* Multi --------------------------------------------------------- *;;


(defmacro mu-defmulti (fun &rest args)
  "Creates a new mu-method dispatch function. The DOCSTRING and
HIERARCHY are optional. HIERARCHY if not supplied defaults to the
global hierarchy.

May be called according to one of the following signatures:

  (mu-defmulti name argvector &optional docstring :in hierarchy body...)
  (mu-defmulti name function &optional docstring :in hierarchy)

where ARGVECTOR is a vector of arguments that follows full Common
Lisp arglist convention, FUNCTION is any expression that returns
a function.

\(fn name argvector &optional docstring :in hierarchy body...)"
  (declare (indent 2))
  (destructuring-bind
      (err dispatch doc hierarchy)
      (pcase args
        ;; (mu-defmulti foo [a b &rest args] "doc" :in hierarchy e1 e2)
        (`(,(multi arglist :if vectorp) ,(multi doc :if stringp) :in ,hierarchy . ,body)
         (list nil `(fn ,(seq-into arglist 'list) ,@body) doc hierarchy))

        ;; (mu-defmulti foo [a b &rest args] :in hierarchy e1 e2)
        (`(,(multi arglist :if vectorp) :in ,hierarchy . ,body)
         (list nil `(fn ,(seq-into arglist 'list) ,@body) "" hierarchy))

        ;; (mu-defmulti foo [a b &rest args] "doc" e1 e2)
        (`(,(multi arglist :if vectorp) ,(multi doc :if stringp) . ,body)
         (list nil `(fn ,(seq-into arglist 'list) ,@body) doc 'mu-global-hierarchy))

        ;; (mu-defmulti foo [a b &rest args] e1 e2)
        (`(,(multi arglist :if vectorp) . ,body)
         (list nil `(fn ,(seq-into arglist 'list) ,@body) "" 'mu-global-hierarchy))

        ;; (mu-defmulti foo fn-returning-expr "doc" :in hierarchy)
        (`(,f ,(multi doc :if stringp) :in ,hierarchy)
         (list nil f doc hierarchy))

        ;; (mu-defmulti foo fn-returning-expr :in hierarchy)
        (`(,f :in ,hierarchy)
         (list nil f "" hierarchy))

        ;; (mu-defmulti foo fn-returning-expr "doc")
        (`(,f ,(multi doc :if stringp))
         (list nil f doc 'mu-global-hierarchy))

        ;; (mu-defmulti foo fn-returning-expr)
        (`(,f)
         (list nil f "" 'mu-global-hierarchy))

        (otherwise
         ;; TODO If we signal an an error immediately no relevant `condition-case'
         ;; would catch it, because IIUC it only traps runtime but we throw at
         ;; macro expansion, so instead I need to generate code that throws.
         ;; Wonder if there is a way to trap compile time errors?
         (list `(mu-error "in mu-defmulti malformed arglist at %s" ',args) nil nil nil)))
    (or
     err
     `(progn

        ;; check if lexical binding is enabled
        (mu-lexical-binding)

        ;; create a dispatch function
        (defun ,fun (&rest args)
          ,doc
          (let* ((val     (apply (get ',fun :mu-dispatch) args))
                 (methods (mu-methods :for ',fun :matching val :in ,hierarchy))
                 (method  (mu--select-preferred ',fun methods ,hierarchy val)))
            (apply method args)))

        ;; set fun value slot to return its quoted form, this lets us pass fun
        ;; around as value and still not break functions that expect a symbol
        (setf ,fun ',fun)

        ;; reset dispatch prop to the dispatch function
        (setf (get ',fun :mu-dispatch) ,dispatch)

        ;; reset mu-methods prop to a fresh table with :default pre-installed
        (setf (get ',fun :mu-methods) (ht))

        ;; reset mu-prefers prop to a fresh table
        (setf (get ',fun :mu-prefers) (ht))
        (setf (mu-prefers ',fun ,hierarchy) (ht))

        ;; pre-install default method
        (setf (get ',fun :mu-default)
              (fn (&rest args)
                (mu-error
                 "no mu-methods match dispatch value %s for dispatch %s "
                 (apply (get ',fun :mu-dispatch) args)
                 ',fun)))

        ;; TODO Invalidate mu-methods cache here. Need to do this to catch
        ;; cases where fun simply gets redefined and may hold cache for previous
        ;; dispatch function
        ))))


(defun mu--dispatch-defun (fun dispatch doc hierarchy)
  `(progn

     ;; check if lexical binding is enabled
     (mu-lexical-binding)

     ;; create a dispatch function
     (defun ,fun (&rest args)
       ,doc
       (let* ((val (apply (get ',fun :mu-dispatch) args))
              (methods (mu-methods :for ',fun :matching val :in ,hierarchy))
              (method (mu--select-preferred ',fun methods ,hierarchy val)))
         (apply method args)))

     ;; set fun value slot to return its quoted form, this lets us pass fun
     ;; around as value and still not break functions that expect a symbol
     (setf ,fun ',fun)

     ;; reset dispatch prop to the dispatch function
     (setf (get ',fun :mu-dispatch) ,dispatch)

     ;; reset mu-methods prop to a fresh table with :default pre-installed
     (setf (get ',fun :mu-methods) (ht))

     ;; reset mu-prefers prop to a fresh table
     (setf (get ',fun :mu-prefers) (ht))
     (setf (mu-prefers ',fun ,hierarchy) (ht))

     ;; pre-install default method
     (setf (get ',fun :mu-default)
           (fn (&rest args)
             (mu-error
              "no mu-methods match dispatch value %s for dispatch %s "
              (apply (get ',fun :mu-dispatch) args)
              ',fun)))))


(comment
 (mu-defmacro mu-defmulti (fun &rest args)
   :doc "string"
   :sig ""
   :declare ((indent 2))
   ([(multi arglist :if vectorp) (multi doc :if stringp) :in hierarchy &rest body]
    (mu--dispatch-defun fun `(fn (seq-into ,arglist 'list) ,@body) doc hierarchy))

   ([(multi arglist :if vectorp) :in hierarchy &rest body]
    `(multi ,fun ,arglist "" :in ,hierarchy ,@body))

   ([(multi arglist :if vectorp) (multi doc :if stringp) &rest body]
    `(multi ,fun ,arglist ,doc :in 'mu-global-hierarchy ,@body))

   ([(multi arglist :if vectorp) &rest body]
    `(multi ,fun ,arglist "" :in 'mu-global-hierarchy ,@body))

   ([f (multi doc :if stringp) :in hierarchy]
    (mu--dispatch-defun fun f doc hierarchy))

   ([f :in hierarchy]
    (mu--dispatch-defun fun f "" hierarchy))

   ([f (multi doc :if stringp)]
    (mu--dispatch-defun fun f "" 'mu-global-hierarchy))

   ([f]
    (list nil f "" 'mu-global-hierarchy))

   (otherwise
    `(mu-error "in mu-defmulti malformed arglist at %s" 'args)))
 ;; comment
 )


;; NOTE Far as I can tell if I were to define some methods with mu-defmethod then
;; compile that file and load it mu-methods table will have compiled lambdas, so
;; that's good. But the (byte-compile (mu-defmethod ...)) doesn't seem to install
;; compiled lambdas, not sure why.


(cl-defmacro mu-defmethod (fun arglist &rest args)
  "Creates a new mu-defmethod associated with the dispatch
function FUN and dispatch value VAL. ARGLIST follows full Common
Lisp conventions.

\(fn fun arglist :when val body...)"
  (pcase args
    (`(:when ,val . ,body)
     (let ((method `(fn ,arglist ,@body)))
       `(progn

          ;; check if lexical binding is enabled
          (mu-lexical-binding)

          ;; add new method to the mu-methods table
          (setf (ht-get* (get ',fun :mu-methods) ,val) ,method)

          ;; TODO invalidate mu-methods cache
          )))
    (otherwise
     `(mu-error "in mu-defmethod malformed arglist at %s" ',args))))


;;* Playground ---------------------------------------------------- *;;


(comment
 ;; Syntax examples
 (mu-defmulti foo (lambda (a b &rest args) e1 e2) "doc" :in hierarchy)
 (mu-defmulti foo (lambda (a b &rest args) e1 e2) :in hierarchy)
 (mu-defmulti foo (lambda (a b &rest args) e1 e2) "doc")
 (mu-defmulti foo (lambda (a b &rest args) e1 e2))

 (mu-defmulti foo 'foo-fun "doc" :in hierarchy)
 (mu-defmulti foo 'foo-fun :in hierarchy)
 (mu-defmulti foo 'foo-fun "doc")
 (mu-defmulti foo 'foo-fun)

 (mu-defmulti foo [a b &rest args] "doc" :in hierarchy e1 e2)
 (mu-defmulti foo [a b &rest args] :in hierarchy e1 e2)
 (mu-defmulti foo [a b &rest args] "doc" e1 e2)
 (mu-defmulti foo [a b &rest args] e1 e2)

 ;; Same but pseudo-coded with &optional and &rest
 (mu-defmulti foo (lambda (a &rest args) body) &optional "doc" :in hierarchy)
 (mu-defmulti foo #'dispatch &optional "doc" :in hierarchy)
 (mu-defmulti foo [a &rest args] &optional "doc" :in hierarchy &rest body)
 ;; example
 )


;;* Provide ------------------------------------------------------- *;;


(provide 'multi-methods)


;; TODO hoist all error messages to Error section

;; TODO maybe `curry' and `rcurry'?

;; TODO Elisp specific idea is to allow supplying setters in mu-methods, so that
;; mu-defmethod invocation can be used with gv setters like `setf', `push', `callf'
;; etc. That makes perfect sence if your dispatch is for looking up some location
;; based on arguments. It may on occasion be quite natural to use the same syntax
;; to set new value to that location.

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
;; I'll have to overload mu-methods accessor function so it can do what current
;; `mu-methods' can do. One interesting aspect is that the value of foo then
;; becomes a struct and predicate (mu-p foo) works as expected, this however
;; necessitates passing quoted foo where expected, but we could work around this
;; by adding an extra :name or :id or :symbol slot, what would carry 'foo.

;; TODO dispatch cache

;; TODO hierarchy cache

;; TODO Make `mu-test' available here to be used in comments and examples

;; TODO Create Makefile (stick to ANSI make): ert batch test, measure perf

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

;; TODO Should I overload (mu-rel x relates-to? x) to be used as predicate:
;; (if (mu-rel y parent-of? x) do stuff) or define (mu-rel? ...)?

;; TODO How hard would it be to add body from (example foo body) forms to the
;; docstring of 'foo? Might be worth implementing something like:
;;
;;   (defun/example foo (&rest args)
;;     :doc "docstring"
;;     :example (example that runs whenever defun is evaled
;;                       and its stringified copy appended to docstring)
;;     body)
