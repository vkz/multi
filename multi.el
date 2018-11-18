;; -*- lexical-binding: t; -*-

(require 'cl)

;; TODO Consider struct to hold hierarchies
;; (defstruct multi-hierarchy
;;   (table (ht))
;;   (cache (ht)))

;; TODO dispatch cache

;; TODO hierarchy cache

;; TODO Make `multi-test' available here to be used in comments and examples

;; TODO Create Makefile (stick to ANSI make): ert batch test, measure perf

;; TODO Implement `prefer-method' for disambiguation

;; TODO Implement `remove-method'

;; TODO Consider storing hierarchies the way Clojure does it. IMO benefit is that
;; descendants are precalculated. Anything else?
;;   {:parents     {:rect #{:shape}}
;;    :ancestors   {:rect #{shape}}
;;    :descendants {:shape #{rect}}}

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
;; docstring of 'foo?


;;* Errors -------------------------------------------------------- *;;


(define-error 'multi-error "multi-error")


(defun multi-error (&rest args)
  "Signals errors specific to `multi' library. Can be caught with
'multi-error ERROR-SYMBOL in `condition-case', otherwise behaves
exactly like `error'

\(fn string &rest args)"
  (signal 'multi-error (list (apply #'format-message args))))


;;* State  -------------------------------------------------------- *;;


(defconst multi-global-hierarchy (ht)
  "Global table that holds global hierachy. Has the following
structure:

  (ht (VAL (ht (:parents (p ...)) (:children (c ...)))) ...)")


(defun multi-global-hierarchy (&rest keys)
  "Returns the value in the global hierarchy nested table, where
KEYS is a sequence of keys. Returns nil if the key is not
present. Without KEYS returns the entire table."
  (if keys
      (apply #'ht-get* multi-global-hierarchy keys)
    multi-global-hierarchy))


(cl-defun multi-methods (&rest args)
  "Returns an alist of (VALUE . method) pairs where (multi-isa?
VAL VALUE) relationship holds. If no such pairs exist returns an
alist of one (:default . default-method), where default-method is
either a custom user-installed or the pre-installed one. If
HIERARCHY not supplied defaults to the global hierarchy. If
called with a single FUN argument returns its full table of
installed multi-methods. FUN is a symbol.

\(fn :for fun &optional :matching val :in hierarchy)"
  ;; parse args
  (pcase args
    ;; (multi-methods :for 'fun)
    (`(:for ,fun)
     (get fun :multi-methods))

    ;; (multi-methods :for 'fun :matching val)
    (`(:for ,fun :matching ,val)
     (multi-methods :for fun :matching val :in multi-global-hierarchy))

    ;; (multi-methods :for 'fun :matching val :in hierarchy)
    (`(:for ,fun :matching ,val :in ,hierarchy)
     (let* ((multi-methods (get fun :multi-methods))
            (methods
             (-non-nil
              (ht-map
               (fn (VAL method)
                 (and (multi-isa? val VAL hierarchy)
                      (cons VAL method)))
               multi-methods)))
            (default-method
              (unless methods
                (list
                 ;; Use custom :default if installed, fallback to the
                 ;; pre-installed default method otherwise
                 (cons :default (or (ht-get* multi-methods :default)
                                    (get fun :multi-default)))))))
       (or methods default-method)))

    (otherwise
     (multi-error "malformed arglist at %s" args))))


;;* Hierarchies --------------------------------------------------- *;;


(defun multi--cycle? (item parent &optional hierarchy)
  "Checks if ITEM and would be PARENT would form a cycle were the
relationship added to the HIERARCHY. If HIERARCHY not supplied
defaults to the global hierarchy"
  (default hierarchy :to multi-global-hierarchy)
  (or (equal item parent)
      (ormap
       (lambda (ancestor) (multi--cycle? item ancestor hierarchy))
       (ht-get* hierarchy parent :parents))))


(defmacro multi-rel (&rest args)
  "Establishes an isa? (parent/child) relationship between PARENT
and CHILD. If HIERARCHY not supplied defaults to, and modifies,
the global hierarchy.

\(fn child :isa parent &optional :in hierarchy)"
  (destructuring-bind
      (child parent hierarchy)
      ;; TODO report malformed arglist the last pcase
      (pcase args
        (`(,child ,(or 'isa :isa) ,parent) (list child parent nil))
        (`(,child ,(or 'isa :isa) ,parent ,(or 'in :in) ,hierarchy) (list child parent hierarchy)))
    (let ((hierarchy (or hierarchy 'multi-global-hierarchy)))
      `(progn
         (let ((child ,child)
               (parent ,parent))
           (when (multi--cycle? ,child ,parent ,hierarchy)
             (multi-error "cycle relationship between %s and %s "
                          ,child ,parent))
           (pushnew ,parent (ht-get* ,hierarchy ,child :parents))
           (pushnew ,child (ht-get* ,hierarchy ,parent :children))
           ,hierarchy)))))


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

   ((member y (ht-get* hierarchy x :parents))
    (cons :generation (1+ generation)))

   (:else
    (ormap
     (lambda (parent) (multi-isa? parent y hierarchy (1+ generation)))
     (ht-get* hierarchy x :parents)))))


(defun multi-parents (x &optional hierarchy)
  "Returns immediate parents (multi-isa? X PARENT) of X. If
HIERARCHY not supplied defaults to the global hierarchy"
  (default hierarchy :to multi-global-hierarchy)
  (ht-get* hierarchy x :parents))


(defun multi-ancestors (x &optional hierarchy)
  "Returns an immediate and indirect ancestors (multi-isa? X
ANCESTOR) of X. If HIERARCHY not supplied defaults to the global
hierarchy"
  (default hierarchy :to multi-global-hierarchy)
  (let ((parents (ht-get* hierarchy x :parents)))
    (append parents
            (seq-mapcat
             (lambda (parent)
               (multi-ancestors parent hierarchy))
             parents))))


(defun multi-descendants (x &optional hierarchy)
  "Returns an immediate and indirect descendants (multi-isa?
DESCENDANT X) of X. If HIERARCHY not supplied defaults to the
global hierarchy"
  (default hierarchy :to multi-global-hierarchy)
  (let ((children (ht-get* hierarchy x :children)))
    (append children
            (seq-mapcat
             (lambda (child)
               (multi-descendants child hierarchy))
             children))))


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
;; as needed.
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
     (multi-error "malformed pcase multi pattern"))))


(example
 (pcase '([a b] "doc")
   (`(,(multi arglist :if vectorp) ,(multi doc :if stringp)) (list arglist doc))
   (otherwise
    (error "no match")))
 ;; example
 )


(defmacro multi (fun &rest args)
  "Creates a new multimethod dispatch function. The DOCSTRING and
HIERARCHY are optional. HIERARCHY if not supplied defaults to the
global hierarchy.

May be called according to one the following signatures:

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
         (list `(multi-error "malformed arglist at %s" ',args) nil nil nil)))
    (or
     err
     `(progn

        (defun ,fun (&rest args)
          ,doc
          (let* ((val     (apply (get ',fun :multi-dispatch) args))
                 (methods (multi-methods :for ',fun :matching val :in ,hierarchy))
                 ;; => ((VAL . method) ...)
                 ;; TODO Choose method with prefer method instead of cdar here
                 (method  (cdar methods)))
            (unless (null (cdr methods))
              (multi-error
               "multiple methods match dispatch value %s for dispatch %s"
               val ',fun))
            (apply method args)))

        ;; reset dispatch prop to the dispatch function
        (setf (get ',fun :multi-dispatch) ,dispatch)

        ;; reset multi-methods prop to a fresh table with :default pre-installed
        (setf (get ',fun :multi-methods) (ht))

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
          ;; add new method to the multi-methods table
          (setf (ht-get* (get ',fun :multi-methods) ,val) ,method)

          ;; TODO invalidate multi-methods cache
          )))
    (otherwise
     `(multi-error "malformed arglist at %s" ',args))))


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
