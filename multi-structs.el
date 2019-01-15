;; -*- lexical-binding: t; -*-


(require 'multi-prelude)


;;* mu-protocols ------------------------------------------------- *;;


;; NOTE every METHOD declared in protocol NAME must be unique just like a function
;; declaration would since we are generating cl-defgenerics from each


(defconst mu-protocols (ht (:protocols (ht))
                           (:types (ht))))

(example
 (ht (:protocols (ht ('protocol-name (ht ('method-name '(arglist...))))))
     (:types (ht ('type (ht ('protocol-name t))))))
 ;; example
 )


(defmacro mu-defprotocol (name &rest methods)
  (declare (indent 1))
  (let ((by-length (lambda (a b) (> (length a) (length b)))))
    `(progn
       ,@(loop for (_ sym . rest) in methods
               with arglists
               with docstring
               do (setf arglists (seq-take-while #'listp rest)) and
               do (setf docstring (seq-drop-while #'listp rest)) and
               collect
               `(progn
                  (setf (ht-get* mu-protocols :protocols ',name ',sym)
                        ',(sort arglists by-length))
                  (cl-defgeneric ,sym (self &rest args) ,@docstring))))))


(cl-defun mu--method-to-pcase-clause ((arglist . body))
  (let ((pattern (list '\` (mapcar (lambda (s) (list '\, s)) arglist))))
    `(,pattern ,@body)))


(defun mu--cl-defmethod (sym obj type rest methods)
  `(cl-defmethod ,sym ((,obj ,type) &rest ,rest)
     (pcase (cons ,obj ,rest)
       ,@(mapcar #'mu--method-to-pcase-clause methods)
       (otherwise
        (mu-error "in protocol method %s arity error" ',sym)))))


(defun mu--cl-defmethods (type methods)
  (loop for (sym . methods) in (seq-group-by #'second methods)
        with obj = (gensym "obj")
        with rest = (gensym "rest")
        collect
        (mu--cl-defmethod
         sym obj type rest (mapcar #'cddr methods))))


(defmacro mu-extend (protocol &rest body)
  (declare (indent 1))
  ;; TODO check for malformed body and that protocol/methods match existing.
  ;; There's a ton of error checking that should happen here tbh
  (let* ((decl? (lambda (item) (eq :to item)))
         (decls (seq-remove #'null (mu--split-when decl? body))))
    `(progn
       ,@(loop for (type . methods) in decls
               collect
               `((setf (ht-get* mu-protocols :types ',type ',protocol) t)
                 ,@(mu--cl-defmethods type methods))
               into defmethods
               finally return (apply #'append defmethods)))))


(defun mu-implements (obj &optional protocol)
  (when-let ((protocols (ht-get* mu-protocols :types (type-of obj))))
    (if protocol
        (ht-get protocols protocol)
      (ht-keys protocols))))


(example

 (mu-defprotocol foo-protocol
   (defmethod foo-stringify (self) (self format) "docstring")
   (defmethod foo-symbolify (self) "docstring"))


 (mu-extend foo-protocol

   :to foo-struct

   (defmethod foo-stringify (foo fmt)
     (format fmt (foo-struct-name foo) (foo-struct-value foo)))

   (defmethod foo-stringify (foo)
     (foo-stringify foo "%S => %S"))

   (defmethod foo-symbolify (foo)
     (sym (foo-struct-name foo)))

   :to bar-struct

   (defmethod foo-stringify (bar fmt)
     (format fmt (bar-struct-nm bar) (bar-struct-val bar)))

   (defmethod foo-stringify (bar)
     (foo-stringify bar "%S => %S"))

   (defmethod foo-symbolify (bar)
     (sym (bar-struct-nm bar))))

 (cl-defstruct foo-struct (type :foo) name (value "always foo"))
 (cl-defstruct bar-struct (type :bar) nm (val "always bar"))


 (mu-implements (make-foo-struct :name 'my-foo) 'foo-protocol)
 (mu-implements (make-foo-struct :name 'my-foo))


 (foo-stringify (make-foo-struct :name 'my-foo) "string of %s value %s")
 (foo-symbolify (make-foo-struct :name 'my-foo))

 (foo-stringify (make-bar-struct :nm 'my-bar) "string of %s value %s")
 (foo-symbolify (make-bar-struct :nm 'my-bar))

 ;; example
 )


;;* mu-structs --------------------------------------------------- *;;


(cl-defstruct mu-struct (-missing-slots (ht)))
(defalias 'mu-struct? 'mu-struct-p)


(defmacro mu-defstruct (name-props &rest slots)
  (declare (indent defun))
  (let (struct-name
        struct-props)

    (pcase name-props
      (`(,name . ,props) (setf struct-name name
                               struct-props props))
      (name              (setf struct-name name
                               struct-popns nil)))

    (setf struct-props `((:include mu-struct) ,@struct-props (:copier nil)))

    `(progn
       (cl-defstruct (,struct-name ,@struct-props) ,@slots)

       ;; TODO Do I want this for every new mu-struct, or should I defer to the
       ;; default `mu--get' above?
       ;; (cl-defmethod mu--get ((obj ,struct-name) key)
       ;;   (condition-case err
       ;;       (cl-struct-slot-value ',struct-name (sym key) obj)
       ;;     (cl-struct-unknown-slot
       ;;      (if (mu-struct? obj)
       ;;          (ht-get (mu-struct--missing-slots obj) key)
       ;;        (mu-error "not a mu-struct %S has no slot %S" obj key)))))

       ;; TODO implement mu-struct patterns
       ;; (mu-defpattern ,struct-name (&rest patterns)
       ;;   (mu-error "implement mu-struct patterns"))
       )))


;;* mu-getters --------------------------------------------------- *;;


;; generic table lookup
(cl-defgeneric mu--get (table key))


;; method when nil
(cl-defmethod mu--get ((obj (eql nil)) key)
  nil)


;; method for hash-table
(cl-defmethod mu--get ((obj hash-table) key)
  (ht-get obj key))


;; default method
(cl-defmethod mu--get ((obj t) key)
  (condition-case err
      (cl-struct-slot-value (type-of obj) (sym key) obj)
    ;; lookup in --missing-slots if mu-struct instance
    (cl-struct-unknown-slot (if (mu-struct? obj)
                                (ht-get (mu-struct--missing-slots obj) key)
                              (mu-error "not a mu-struct %S has no slot %S" obj key)))))


(defun mu. (table key &rest keys)
  "Look up KEY in TABLE. Return nil if no such KEY. Works for any
TABLE that implements generic `mu--get'."
  (when-let ((table (mu--get table key)))
    (if keys
        (apply #'mu. table keys)
      table)))


(example
 ;; getters
 (mu-defstruct foo-struct props)

 (mu. (ht (:a 1)) :a)
 (mu. (ht (:a (ht (:b 1)))) :a :b)
 (mu. (make-foo-struct :props (ht (:a (ht (:b 1))))) :props :a :b)

 ;; default mu--get should work
 (cl-defstruct bar-struct props)
 (mu. (make-bar-struct :props (ht (:a (ht (:b 1))))) :props :a :b)

 ;; nested structs should work
 (mu. (make-bar-struct :props (make-foo-struct :props 1)) :props :props)

 ;; mix of nested ht and structs should work
 (let* ((foo (make-foo-struct :props (ht (:b 1))))
        (bar (make-bar-struct :props (ht (:a foo)))))
   (list
    (mu. bar :props :a :props :b)
    (mu. bar :props :a :props :c)))
 ;; example
 )


;;* mu-setters --------------------------------------------------- *;;


(cl-defgeneric mu--set (obj key val))


;; hash-table setter
(cl-defmethod mu--set ((obj hash-table) key val)
  (setf (ht-get obj key) val))


;; TODO nil setter. Do we assume its an empty list?
;;
;; (cl-defmethod mu--set ((obj (eql nil)) key val)
;;   (mu-error "unimplemented"))


;; default setter
(cl-defmethod mu--set ((obj t) key val)
  (if (recordp obj)
      (condition-case err
          (setf (cl-struct-slot-value (type-of obj) (sym key) obj) val)
        (cl-struct-unknown-slot (if (mu-struct? obj)
                                    (setf (ht-get (mu-struct--missing-slots obj) key) val)
                                  (mu-error "not a mu-struct %S has no slot %S" obj key))))
    (mu-error "no mu-setter defined for object %S of type %s" obj (type-of obj))))


(defun mu--set* (val obj key keys)
   (if keys
       (let ((table (or (mu--get obj key) (mu--set obj key (ht)))))
         (mu--set* val table (car keys) (cdr keys)))
     (mu--set obj key val)))


;; NOTE this one is exactly like my `ht-get*' setter, except that `mu--set*'
;; dispatches to generic `mu--set' to do correct `setf'
(gv-define-setter mu. (val table key &rest keys)
  `(mu--set* ,val ,table ,key (list ,@keys)))


(example

 (mu-defstruct bazzer props)

 (mu. (make-bazzer :props '(a b)) :props)
 ;; =>
 '(a b)

 (mu. (make-bazzer :props '(a b)) :missing)
 ;; =>
 nil

 (let ((baz (make-bazzer :props (ht (:a (ht (:b 1)))))))
   (setf (mu. baz :props :a :b) 2)
   (mu. baz :props :a :b))
 ;; =>
 2

 (let ((baz (make-bazzer :props (ht))))
   (setf (mu. baz :props :a) 2)
   (mu. baz :props :a))
 ;; =>
 2

 (let ((baz (make-bazzer)))
   (setf (mu. baz :props) 2)
   (mu. baz :props))
 ;; =>
 2

 (let ((baz (make-bazzer)))
   (setf (mu. baz :missing :a :b) 2)
   (mu. baz :missing :a :b))
 ;; =>
 2

 (let ((baz (make-bazzer :props (ht))))
   (setf (mu. baz :props :a :b) 2)
   (mu. baz :props :a :b))
 ;; =>
 2

 (let ((baz (make-bazzer)))
   (setf (mu. baz :props :a) 2)
   (mu. baz :props :a))
 ;; =>
 2

 ;; example
 )


(comment

 ;; TODO non-generic setters - remove this once I test generic `mu--set'

 (defun mu--setter (val obj key)
   `(cond
     ;; hash-table
     ((ht-p ,obj)
      (setf (ht-get ,obj ,key) ,val))
     ;; struct
     ((recordp ,obj)
      (condition-case err
          (setf (cl-struct-slot-value (type-of ,obj) (sym ,key) ,obj) ,val)
        (cl-struct-unknown-slot
         (if (mu-struct? ,obj)
             (setf (ht-get (mu-struct--missing-slots ,obj) ,key) ,val)
           (mu-error "not a mu-struct %S has no slot %S" ,obj ,key)))))
     ;; TODO vectors and lists?
     (:else
      (mu-error "no mu. setter defined for object %S of type %s" ,obj ,(type-of ,obj)))))


 (gv-define-setter mu--get (val obj key)
   (mu--setter val obj key))


 (defun mu--set* (val obj key keys)
   (if keys
       (let ((table (or (mu--get obj key) (setf (mu--get obj key) (ht)))))
         (mu--set* val table (car keys) (cdr keys)))
     (setf (mu--get obj key) val)))

 ;; comment
 )


;;* provide ------------------------------------------------------ *;;


(provide 'multi-structs)


;;* todo --------------------------------------------------------- *;;


;; TODO arity dispatch in protocol methods doesn't require `pcase'. Suppose we
;; store method to arity map for every method in `mu-protocols':
;;
(ht ('mu--get (ht (2 (lambda (obj key) body))
                  (3 (lambda (obj k1 k2) body)))))
;;
;; then dispatch becomes trivial, also simplifies arity checks etc
(cl-defmethod mu--get ((obj type) &rest args)
  (let ((args (cons obj args )))
    ;; apply stored relevant method implementation explicitly
    (apply (ht-get mu-protocols 'mu--get (length args)) args)))


;; TODO each protocol should either be a `mu-protocol' struct or maybe a function
;; that returns relevant entries from `mu-protocols'. Idea is to have meaningful
;; documentation for each protocol


;; TODO protocol methods should mention their protocol in docs. See how
;; `cl--generic-describe' does it.


;; TODO steal debug declaration from `cl-defmethod'


;; TODO should `mu--get' be an protocol method?
(mu-defprotocol mu-associative-protocol
  (defmethod mu--get (obj keys)))
(mu-extend mu-get-protocol
  :to hash-table
  (defmethod mu--get (obj keys) (apply #'ht-get* obj keys)))


;; TODO allow to define and store setters alongside each method, where setter is
;; just a function of val and getter-args that must generate a working `setf' code
(mu-extend foo-proto
  :to foo-struct

  ;; note how setters have access to the getter's arg-ids

  (defmethod foo-stringify (obj key)
    ;; arity = 2 (mu--get obj key)
    :setter (lambda (val) `(setf (ht-get ,obj ,key) ,val))
    ;; => extend arglist with (obj key) and store:
    ;;
    ;; (lambda (val obj key) `(setf (ht-get ,obj ,key) ,val))
    body)

  (defmethod foo-stringify (obj key1 key2)
    ;; arity = 3 (mu--get obj key1 key2)
    :setter (lambda (val) `(setf (ht-get ,obj ,key1 ,key2) ,val))
    ;; => extend arglist with (obj key1 key2) and store:
    ;;
    ;; (lambda (val obj key1 key2) `(setf (ht-get ,obj ,key1 ,key2) ,val))
    body))
;;
;; every time the protocol gets extended to a type we re-generate:
;;
(gv-define-setter mu--get (val obj &rest args)
  (mu--dispatch-to-setters 'mu--get val obj args))
;;
;;
;; Where `mu--dispatch-to-setters' generates case-code dispatching on type of obj.
;; This assumes we keep a mapping from type to arity to method somewhere - so,
;; `mu-protocols' would need to be richer:
;;
(defun mu--dispatch-to-setters (method-name val obj rest)
  ;; note that we know arity at compile time!
  (let* ((args (cons obj rest))
         (setters (loop for type in (keys (ht-get mu-protocols :types))
                        ;; get setter for current arity
                        when (ht-get mu-protocols :types type method-name (length args))
                        collect
                        ;; collect (type setter)
                        (list type
                              (ht-get mu-protocols :types type method-name (length args))))))
    ;; for every registered `mu--get' setter of arity (length args) insert:
    `(case (type-of ,obj)
       ,(loop for (type setter) in setters
              collect `(,type ,(apply setter val args))))

    ;; => runtime dispatch for every type

    ;; (case (type-of obj)
    ;;   ('foo-struct (setf (pre-existing setter) val))
    ;;   ('bar-struct (setf (pre-existing setter) val)))

    ;; Actually this may as well be a hash-table of 'type => code, where we simply
    ;; lookup 'type and return code, no need for case dispatch
    ))


;; TODO `mu-defstruct' should generate a getter named `struct-name' which works
;; for slots and keys that aren't slots:
(foo-struct foo 'methods :a :b)
(foo-struct foo 'sub 'table :a :b)


;; TODO `mu-defstruct' should generate defsetter for `struct-name' which works for
;; slots and keys that aren't slots:
(setf (foo-struct foo methods :a :b) 'val)
(setf (foo-struct foo sub table :a :b) 'val)


;; TODO register a mu-pattern for `struct-name' that works just like ht-pattern
;; but also tests with `struct-name-p'
(mu-defstruct multi name)
(setq m (make-multi :name 'foo))
;;
(mu-case m
  ((multi name) name))
;; => 'foo


;; TODO corollary to that extend ht-pattern so that it works on any associative
;; e.g. should work for any mu-struct?


;; TODO mu-callable protocol that effectively makes structs callable
(mu-defprotocol mu-callable-protocol
  (mu-call (f &rest args))
  (mu-apply (f &rest args)))
;; where
(cl-defgeneric mu-call (obj &rest args))
(cl-defgeneric mu-apply (obj &rest args))
;; lets make struct callable (assume it defines some lambda in :call slot)
(mu-extend mu-callable-protocol
  :to foo-struct
  (mu-call  (foo &rest args) (apply (mu. foo :mu-call) args))
  (mu-apply (foo &rest args) (apply #'apply (mu. obj :call) args)))
;;
;; IMO `mu-apply' is redundant and can be derived from `m-call':
(mu-apply foo arg (list a b))
;; =>
(apply #'apply #'mu-call foo args)
;; ==
(apply #'apply #'mu-call foo (list arg (list a b)))
;; =>
(apply #'mu-call foo arg (list a b))
;; =>
(mu-call foo arg a b)
;;
;; In fact I can extend mu-callable-protocol to every mu-struct simply by adding a
;; :mu-call slot to `mu--proto-struct', so that every mu-struct inherits it, then
;; unless user overrides the default `mu-call' should work:
(cl-defmethod mu-call ((obj t) &rest args)
  (if (mu-struct? obj)
      (apply (mu. obj :mu-call) args)
    (apply obj args)))
;;
(cl-defmethod mu-apply ((obj t) &rest args)
  (if (mu-struct? obj)
      (apply #'apply (mu. obj :mu-call) args)
    (apply #'apply obj args)))


;; TODO nothing stopping protocols from sharing methods IMO. With defprotocol
;; dumbly generating defgenerics they might step on each-other. Wonder how to
;; handle it properly. One idea is to check against existing protocols and methods
;; to raise unless introduced arities match that for already existing method and
;; bypass defgeneric if they match. Another idea is to always raise when method is
;; redefined and instead assume that protocols are defined in small enough units
;; and then groups of them extended to types as needed. That is no two protocols
;; intersect. Instead types have to implement multiple protocols as needed.
;; Partitioning protocols well may end up just as hard as building a class
;; hierarchy in OOP, or is it?


;; TODO how about extending protocol to another protocol?


;; NOTE interesting pattern is dispatching to a multi-method inside a generic
;; method when dispatching on type is too coarse:
(cl-defmethod some-method ((obj t) arg)
  (cond
   ;; ... earlier branches ...
   ((pred? obj) (mu--multi-get obj arg))
   ;; ... later   branches ...
   (:else       (some default action))))


;; TODO If we go the generic route with mu-struct and mu-protocols, then it is
;; worth thinking about visually telling generic functions from all others. Maybe
;; a `mu.' prefix or more generally `namespace.generic-function':
(mu. a b)
(mu.get a b c)
(mu.keys a)
;; private
(mu.-val-at)
;; or
(mu..val-at)


;; TODO Equivalent of ISeqable but for objects that can be queried like
;; associative structures, so like (mu.seq obj) but for maps (mu.ht obj). Above
;; mentioned `mu--slots' being the first meaningful slot marks the beginning of
;; slots, so we could implement the equivalent of `destruct' without macros:
;;
;;   (seq-drop (cl-struct-slot-info 'baz-struct)
;;             (1+ (cl-struct-slot-offset 'baz-struct 'mu--slots)))
;;
;; Of course pre-generating `destruct' function at mu-defstruct expansion time may
;; have some perf benefit at the cost of heavier macro-code.


;; TODO I could simplify some setters and getters if I were to implement a
;; threading macro `as->' so that it doesn't compute intermediate steps but simply
;; rewrites into nested calls. Then we could thread `setf' and friends
;;
;; macro getter
`(mu-> ,struct it
       (cl-struct-slot-value 'foo-struct ',slot it)
       (bar-struct it ,@keys))
;; ==
`(bar-struct (cl-struct-slot-value 'foo-struct ',slot ,struct) ,@keys)
;;
;; macro setter
`(mu-> ,struct it
       (cl-struct-slot-value 'foo-struct ',slot it)
       (bar-struct it ,@keys)
       (setf it 'val))
;; ==
`(setf (bar-struct (cl-struct-slot-value 'foo-struct ',slot ,struct) ,@keys) ,val)


;; TODO annotate slots with possible getters. NOTE this is subsumed by generic
;; `mu--get' or `mu--val-at' alike, no immediate need for annotations. At least
;; not as long as we can dispatch on the type of slot value.
(mu-defstruct foo-struct
              (methods (ht) :type 'ht)
              (prefers (ht) :type 'ht)
              (hierarchy    :type 'ht)
              (sub          :type 'bar-struct))
;;
(mu-defstruct bar-struct
              name
              (table (ht) :type 'ht))
;;
(defmacro foo-struct (struct slot &rest keys)
  (declare
   (gv-setter
    (lambda (val)
      (if keys
          ;; setf nested value
          `(case ',slot
             ((mul)
              ;; 'bar-struct sub-struct
              (setf (bar-struct (cl-struct-slot-value 'foo-struct ',slot ,struct) ,@keys) ,val))

             ((methods prefers hierarchy static-hierarchy)
              ;; hash-table
              (setf (ht-get* (cl-struct-slot-value 'foo-struct ',slot ,struct) ,@keys) ,val)))
        ;; set the slot
        (setf (cl-struct-slot-value 'foo-struct ',slot ,struct) ,val)))))
  (if keys
      ;; nested lookup
      `(case ',slot
         ((sub)
          ;; 'bar-struct sub-struct
          (bar-struct (cl-struct-slot-value 'foo-struct ',slot ,struct) ,@keys))

         ((methods prefers hierarchy)
          ;; hash-table
          (ht-get* (cl-struct-slot-value 'foo-struct ',slot ,struct) ,@keys)))
    ;; slot only lookup
    `(cl-struct-slot-value 'foo-struct ',slot ,struct)))


