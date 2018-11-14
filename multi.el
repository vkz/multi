;; -*- lexical-binding: t; -*-

(require 'cl)

;; TODO cache

;;* Errors

(define-error 'multi-error "multi-error")

(defun multi-error (&rest args)
  "Exactly like `error' except its ERROR-SYMBOL is
`multi-error'."
  (declare (advertised-calling-convention (string &rest args) ""))
  (signal 'multi-error (list (apply #'format-message args))))

(example
 (list
  (get 'multi-error 'error-message)
  (condition-case e
      (multi-error "bal bla %s" 'foo)
    (multi-error (cdr e))))
 ;; =>
 ("multi-error" ("bal bla foo"))
 ;; example
 )

;;* Globals


(defconst multi/base-hierarchy (ht))
(defconst multi/methods (ht))
;; =>
;; (ht
;;  ('multifun (ht
;;              (val1 method1)
;;              (val2 method2)
;;              etc)))


(defun multi/base-hierarchy (&rest keys)
  (if keys
      (apply #'ht-get* multi/base-hierarchy keys)
    multi/base-hierarchy))


(cl-defun multi/methods (&key ((:for fun))
                              ((:matching val))
                              ((:in hierarchy) multi/base-hierarchy))
  (default hierarchy :to multi/base-hierarchy)
  (let ((methods (-non-nil
                  (mapcar
                   (fn ((VAL . method)) (and (isa? val VAL hierarchy) method))
                   (ht->alist
                    (ht-get multi/methods fun))))))
    (or methods (ht-get* multi/methods fun :default))))


;; TODO multi/methods should return hash-table of matched val-fn pairs, to which
;; we may apply `prefer-method'

;; TODO Hierarchy is orthogonal to `multi' definition. Indeed in Clojure you may
;; change it (only?) in `defmulti', but IMO it makes more sence to be able to pass
;; it to multimethod invocations (not even definitions). Need to think if that'd
;; be consistent and whether it has any practical value.
(comment
 (let ((hierarchy (ht)))
   (rel :rect isa :shape in hierarchy)
   (rel :square isa :rect in hierarchy)
   (rel :square isa :parallelogram in hierarchy)

   (multi foo [a] :in hierarchy a)
   (multimethod foo (a) :when :square (list a 'isa :square))
   (multimethod foo (a) :when :shape (list a 'isa :shape))

   (list
    (foo :rect)
    (foo :shape)
    ;; => (:shape :rect)
    (condition-case err
        (foo :square)
      ;; => ambiguous, so return all matching methods
      (multi-error
       (multi/methods :for 'foo :matching :square :in hierarchy)))))
 ;; comment
 )


;;* Hierarchies


(defun multi/cycle? (item parent &optional hierarchy)
  "Find if ITEM and would be PARENT would form a cycle were the
relation added to HIERARCHY."
  (default hierarchy :to multi/base-hierarchy)
  (or (equal item parent)
      (ormap
       (lambda (ancestor) (multi/cycle? item ancestor hierarchy))
       (ht-get* hierarchy parent :parents))))


(example
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
 ;; example
 )


;; TODO Could we allow arbitrary relations? E.g. `parent-of'. Would that have any
;; practical benefit? When? How?
(defmacro rel (&rest args)
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


;; TODO Always isa? with "_" pattern
;; TODO Allow predicates in patterns
(cl-defun isa? (x y &optional (hierarchy) (generation 0))
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

   ;; TODO define (relp x foo-rel? y) so we can write
   ;; ((relp y parent-of? x)
   ;;  ;; then
   ;;  (cons :generation (1+ generation)))

   ((member y (ht-get* hierarchy x :parents))
    ;; then
    (cons :generation (1+ generation)))

   (:else
    (ormap
     (lambda (parent) (isa? parent y hierarchy (1+ generation)))
     (ht-get* hierarchy x :parents)))))


(example
 (let ((hierarchy (ht)))
   (rel :rect isa :shape in hierarchy)
   (rel :square isa :rect in hierarchy)
   (list
    (isa? 42 42 hierarchy)
    ;; 0
    (isa? :rect :shape hierarchy)
    ;; 1
    (isa? :square :shape hierarchy)
    ;; 2
    (isa? [:square :rect] [:rect :shape] hierarchy)
    ;; (1 1)
    (isa? [:square :shape] [:rect :shape] hierarchy)
    ;; (1 0)
    (isa? [:square :rect] [:shape :square] hierarchy)
    ;; nil
    (isa? [:square] :rect hierarchy)
    ;; nil
    (isa? [:square] [])
    ;; nil
    ))

 ;; TODO case can be made that this should be true, but I don't like it
 (isa? [:square :rect] [:rect])
 ;; example
 )


(defun multi/parents (x &optional hierarchy)
  (default hierarchy :to multi/base-hierarchy)
  (ht-get* hierarchy x :parents))


(defun multi/ancestors (x &optional hierarchy)
  (default hierarchy :to multi/base-hierarchy)
  (let ((parents (ht-get* hierarchy x :parents)))
    (append parents
            (seq-mapcat
             (lambda (parent)
               (multi/ancestors parent hierarchy))
             parents))))


(defun multi/descendants (x &optional hierarchy)
  (default hierarchy :to multi/base-hierarchy)
  (let ((children (ht-get* hierarchy x :children)))
    (append children
            (seq-mapcat
             (lambda (child)
               (multi/descendants child hierarchy))
             children))))


(example
 (let ((hierarchy (ht)))
   (rel :rect isa :shape in hierarchy)
   (rel :square isa :rect in hierarchy)
   (rel :square isa :parallelogram in hierarchy)
   (list
    (list '(parents :rect) (multi/parents :rect hierarchy))
    (list '(parents :square) (multi/parents :square hierarchy))
    (list '(ancestors :square) (multi/ancestors :square hierarchy))
    (list '(descendants :shape) (multi/descendants :shape hierarchy))))
 ;; example
 )


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
                (method  (car methods)))
           ;; TODO Implement `prefer-method' for disambiguation
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
  (pcase args
    (`(:when ,val . ,body)
     (let ((method `(lambda ,arglist ,@body)))
       `(progn
          (setf (ht-get* multi/methods ',fun ,val) ,method))))

    (otherwise
     (multi-error "Malformed arglist at %s" args))))


(comment
 (multimethod foo (&rest args) :when [a b c] body)
 ;; TODO _ always isa
 ;; TODO (? predicate) isa when true
 (multimethod foo (&rest args) :when [a b _] body)
 (multimethod foo (&rest args) :when [a (?  pred-p) c] body)
 ;; degenerate case where computed multi val maybe a seq, pred-p should still be
 ;; applied even though this here val isn't a seq
 (multimethod foo (&rest args) :when (?  pred-p) body)
 ;; comment
 )


;;* Provide

(provide 'multi)
