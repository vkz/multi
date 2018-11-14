;; -*- lexical-binding: t; -*-

(require 'cl)

;; TODO Switch to - and -- function naming convention

;; TODO Consider storing hierarchies the way Clojure does it. IMO benefit is that
;; descendants are precalculated. Anything else?
;;   {:parents     {:rect #{:shape}}
;;    :ancestors   {:rect #{shape}}
;;    :descendants {:shape #{rect}}}

;; TODO cache

;; TODO Implement `prefer-method' for disambiguation

;; TODO Allow isa? with "_" patterns

;; TODO Allow predicates in patterns

;; TODO Hierarchy is orthogonal to `multi' dispatch function. However in Clojure
;; you may change it (only?) in `defmulti', but IMO it makes more sence to be able
;; to pass it to multimethod invocations (not even definitions). Need to think if
;; that'd be consistent and whether it has any practical value.

;; TODO Could we allow arbitrary relations? E.g. `parent-of'. Would that have any
;; practical benefit? When? How?

;; TODO Should I overload (rel x relates-to? x) to be used as predicate:
;; (if (rel y parent-of? x) do stuff) or define (rel? ...)?

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


(example
 (list
  (get 'multi-error 'error-message)
  (condition-case e
      (multi-error "foo %s" 'bar)
    (multi-error (cdr e))))
 ;; =>
 ("multi-error" ("foo bar"))
 ;; example
 )


;;* State  -------------------------------------------------------- *;;


(defconst multi/base-hierarchy (ht)
  "Global table that holds global hierachy. Has the following
structure:

  (ht (VAL (ht (:parents (p ...)) (:children (c ...)))) ...)")


(defconst multi/methods (ht)
  "Global table that holds all multimethods. Has the following
structure:

  (ht (dispatch-fun-sym (ht (dispatch-val method) ...)) ...)")


(defun multi/base-hierarchy (&rest keys)
  "Returns the value in the global hierarchy nested table, where
KEYS is a sequence of keys. Returns nil if the key is not
present. Without KEYS returns the entire table."
  (if keys
      (apply #'ht-get* multi/base-hierarchy keys)
    multi/base-hierarchy))


(cl-defun multi/methods (&key ((:for fun))
                              ((:matching val))
                              ((:in hierarchy) multi/base-hierarchy))
  "Returns an alist of (VALUE . method) pairs where (isa? VAL
VALUE) relationship holds. If HIERARCHY not supplied defaults to
the global hierarchy.

\(fn :for fun :matching val &optional :in hierarchy)"
  (default hierarchy :to multi/base-hierarchy)
  (let ((methods
         (-non-nil
          (ht-map
           (fn (VAL method)
               (and (isa? val VAL hierarchy)
                    (cons VAL method)))
           (ht-get multi/methods fun)))))
    (or methods (ht-get* multi/methods fun :default))))


(defmacro let/hierarchy (rels &rest body)
  "Installs isa? relationships supplied in RELS in the global
hierarchy only for the extent of BODY execution. Each
relationship in RELS takes the form (:foo isa :bar).

\(fn ((val isa VAL)...) body...)"
  (declare (indent defun))
  (let ((rels (mapcar
               (fn ((val _ VAL)) `(rel ,val isa ,VAL))
               rels)))
    `(let ((multi/base-hierarchy (ht)))
       ,@rels
       ,@body)))


(example
 (let/hierarchy ((:square isa :rect)
                 (:rect isa :shape))
   (list
    (isa? :square :shape)
    multi/base-hierarchy))
 ;; example
 )


(comment
 (let/hierarchy ((:rect isa :shape)
                 (:square isa :rect)
                 (:square isa :parallelogram))
   (multi foo [a] a)
   (multimethod foo (a) :when :square (list a 'isa :square))
   (multimethod foo (a) :when :shape (list a 'isa :shape))
   (list
    (foo :rect)
    ;; => (:rect isa :shape)
    (foo :shape)
    ;; => (:shape isa :shape)
    (condition-case err
        (foo :square)
      (multi-error
       (multi/methods :for 'foo :matching :square)))
    ;; => ambiguous, so return all matching methods
    ))
 ;; comment
 )


;;* Hierarchies --------------------------------------------------- *;;


(defun multi/cycle? (item parent &optional hierarchy)
  "Checks if ITEM and would be PARENT would form a cycle were the
relationship added to the HIERARCHY. If HIERARCHY not supplied
defaults to the global hierarchy"
  (default hierarchy :to multi/base-hierarchy)
  (or (equal item parent)
      (ormap
       (lambda (ancestor) (multi/cycle? item ancestor hierarchy))
       (ht-get* hierarchy parent :parents))))


(comment
 ;; TODO make it a runnable example
 (let ((hierarchy (ht)))
   (rel :square isa :rect in hierarchy)
   (rel :rect isa :shape in hierarchy)
   ;; introduce cycle
   (rel :shape isa :square in hierarchy)
   (multi/cycle? :rect :shape hierarchy)
   ;; t
   (multi/cycle? :square :shape hierarchy)
   (assert (not (multi/cycle? :shape :square hierarchy)) nil "Cyclic `isa?' rel between %s and %s detected." :shape :square)
   ;; error
   )
 ;; comment
 )


(defmacro rel (&rest args)
  "Establishes an isa? (parent/child) relationship between PARENT
and CHILD. If HIERARCHY not supplied defaults to, and modifies,
the global hierarchy.

\(fn child :isa parent &optional :in hierarchy)"
  (destructuring-bind
      (child parent hierarchy)
      (pcase args
        (`(,child ,(or 'isa :isa) ,parent) (list child parent nil))
        (`(,child ,(or 'isa :isa) ,parent ,(or 'in :in) ,hierarchy) (list child parent hierarchy)))
    (let ((hierarchy (or hierarchy 'multi/base-hierarchy)))
      `(progn
         (let ((child ,child)
               (parent ,parent))
           ;; assert child parent do not form cyclic relation
           (assert (not (multi/cycle? ,child ,parent ,hierarchy)) nil "Cyclic `isa?' rel between %s and %s detected." ,child ,parent)
           (pushnew ,parent (ht-get* ,hierarchy ,child :parents))
           (pushnew ,child (ht-get* ,hierarchy ,parent :children))
           ,hierarchy)))))


(defun ormap (pred lst)
  (when lst
    (or (funcall pred (car lst))
        (ormap pred (cdr lst)))))


(cl-defun seq-isa? (seqx seqy hierarchy)
  (let ((rels (seq-mapn (lambda (x y) (isa? x y hierarchy)) seqx seqy)))
    (and (cl-notany #'null rels)
         rels)))


(cl-defun isa? (x y &optional (hierarchy) (generation 0))
  "Returns (generation 0) if (equal CHILD PARENT), or (generation
N) if CHILD is directly or indirectly derived from PARENT, where
N signifies how far down generations PARENT is from CHILD. If
HIERARCHY not supplied defaults to the global hierarchy

\(fn child parent &optional hierarchy)"
  (default hierarchy :to multi/base-hierarchy)
  (cond
   ((sequencep x)
    ;; then
    (and (sequencep y) (equal (length x) (length y))
         (seq-isa? x y hierarchy)))

   ((sequencep y)
    ;; then x wasn't a sequence but should've been
    nil)

   ((equal x y)
    ;; then
    (cons :generation generation))

   ((member y (ht-get* hierarchy x :parents))
    ;; then
    (cons :generation (1+ generation)))

   (:else
    (ormap
     (lambda (parent) (isa? parent y hierarchy (1+ generation)))
     (ht-get* hierarchy x :parents)))))


(example
 (let/hierarchy ((:rect isa :shape)
                 (:square isa :rect))
   (list
    (isa? 42 42)
    ;; 0
    (isa? :rect :shape)
    ;; 1
    (isa? :square :shape)
    ;; 2
    (isa? [:square :rect] [:rect :shape])
    ;; (1 1)
    (isa? [:square :shape] [:rect :shape])
    ;; (1 0)
    (isa? [:square :rect] [:shape :square])
    ;; nil
    (isa? [:square] :rect)
    ;; nil
    (isa? [:square] [])
    ;; nil
    ))
 ;; example
 )


(defun multi/parents (x &optional hierarchy)
  "Returns immediate parents (isa? X PARENT) of X. If HIERARCHY
not supplied defaults to the global hierarchy"
  (default hierarchy :to multi/base-hierarchy)
  (ht-get* hierarchy x :parents))


(defun multi/ancestors (x &optional hierarchy)
  "Returns an immediate and indirect ancestors (isa? X ANCESTOR) of X.
If HIERARCHY not supplied defaults to the global hierarchy"
  (default hierarchy :to multi/base-hierarchy)
  (let ((parents (ht-get* hierarchy x :parents)))
    (append parents
            (seq-mapcat
             (lambda (parent)
               (multi/ancestors parent hierarchy))
             parents))))


(defun multi/descendants (x &optional hierarchy)
  "Returns an immediate and indirect descendants (isa? DESCENDANT
X) of X. If HIERARCHY not supplied defaults to the global
hierarchy"
  (default hierarchy :to multi/base-hierarchy)
  (let ((children (ht-get* hierarchy x :children)))
    (append children
            (seq-mapcat
             (lambda (child)
               (multi/descendants child hierarchy))
             children))))


(example
 (let/hierarchy ((:rect isa :shape)
                 (:square isa :rect)
                 (:square isa :parallelogram))
   (list
    (list '(parents :rect) (multi/parents :rect))
    (list '(parents :square) (multi/parents :square))
    (list '(ancestors :square) (multi/ancestors :square))
    (list '(descendants :shape) (multi/descendants :shape))))
 ;; example
 )


;;* Multi --------------------------------------------------------- *;;


(pcase-defmacro multi (&rest patterns)
  (pcase patterns
    (`(,id :if ,predicate) `(and ,id (pred ,predicate)))
    (otherwise
     (multi-error "Malformed `pcase' multi pattern"))))


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
      (dispatch doc hierarchy)
      (pcase args
        ;; (multi foo [a b &rest args] "doc" :in hierarchy e1 e2)
        ;; (multi foo [a b &rest args] :in hierarchy e1 e2)
        ;; (multi foo [a b &rest args] "doc" e1 e2)
        ;; (multi foo [a b &rest args] e1 e2)
        (`(,(multi arglist :if vectorp) ,(multi doc :if stringp) :in ,hierarchy . ,body)
         (list `(fn ,(seq-into arglist 'list) ,@body) doc hierarchy))

        (`(,(multi arglist :if vectorp) :in ,hierarchy . ,body)
         (list `(fn ,(seq-into arglist 'list) ,@body) "" hierarchy))

        (`(,(multi arglist :if vectorp) ,(multi doc :if stringp) . ,body)
         (list `(fn ,(seq-into arglist 'list) ,@body) doc 'multi/base-hierarchy))

        (`(,(multi arglist :if vectorp) . ,body)
         (list `(fn ,(seq-into arglist 'list) ,@body) "" 'multi/base-hierarchy))

        ;; (multi foo fn-returning-expr "doc" :in hierarchy)
        ;; (multi foo fn-returning-expr :in hierarchy)
        ;; (multi foo fn-returning-expr "doc")
        ;; (multi foo fn-returning-expr)
        (`(,f ,(multi doc :if stringp) :in ,hierarchy)
         (list f doc hierarchy))

        (`(,f :in ,hierarchy)
         (list f "" hierarchy))

        (`(,f ,(multi doc :if stringp))
         (list f doc 'multi/base-hierarchy))

        (`(,f)
         (list f "" 'multi/base-hierarchy))

        (otherwise
         (multi-error "Malformed arglist at %s" args)))

    `(progn
       (defun ,fun (&rest args)
         ,doc
         (let* ((val     (apply ,dispatch args))
                (methods (multi/methods :for ',fun :matching val :in ,hierarchy))
                ;; => ((VAL . method) ...)
                ;; TODO Choose method with prefer method instead of cdar here
                (method  (cdar methods)))
           (assert (null (cdr methods)) nil "multi: expected at most one method %s to match %s" ',fun val)
           (apply method args)))

       ;; Initialize 'fun key to empty table to store 'fun methods. Every
       ;; `multimethod' defined will (assoc val method) in that table.
       (setf (ht-get multi/methods ',fun) (ht)))))


(example
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
 ;; example
 )


(cl-defmacro multimethod (fun arglist &rest args)
  "Creates a new multimethod associated with the dispatch
function FUN and dispatch value VAL. ARGLIST follows full Common
Lisp conventions.

\(fn fun arglist :when val body...)"
  (pcase args
    (`(:when ,val . ,body)
     (let ((method `(fn ,arglist ,@body)))
       `(progn
          (setf (ht-get* multi/methods ',fun ,val) ,method))))

    (otherwise
     (multi-error "Malformed arglist at %s" args))))


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
