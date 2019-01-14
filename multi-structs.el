;; -*- lexical-binding: t; -*-


(require 'multi-prelude)


;;* protocols ---------------------------------------------------- *;;


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
  ;; TODO check for malformed body and that protocol/methods match existing
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


;;* todo --------------------------------------------------------- *;;


;; TODO each protocol should either be a `mu-protocol' struct or maybe a function
;; that returns relevant entries from `mu-protocols'. Idea is to have meaningful
;; documentation for each protocol

;; TODO protocol methods should mention their protocol in docs. See how
;; `cl--generic-describe' does it.

;; TODO steal debug declaration from `cl-defmethod'

;; TODO Possible protocols:
;;
;;   mu-mappable - associative data
;;   mu-seqable - sequential data (e.g. vectors, lists etc)
;;   mu-callable - invoke structs as functions
;;
;; then we could implement generic functions e.g.:
;;
;; (mu. map :key1 :key2)
;; (mu.keys map)
;; (mu.vals map)
;; (mu.map    function map)
;; (mu.reduce function map)
;; (setf (mu. map :key1 :key2) val)
;; (mu. seq 42)
;; (mu.conj seq val)
;; (mu.cons val seq)


;; TODO Every mu-defstruct must define a generic getter `mu--get' (or `mu--val-at'
;; or whatever)
(cl-defmethod mu--get ((obj foo-struct) key)
  ;; (sym key) will turn any of 'key, :key, "key" into 'key, which IMO is a
  ;; reasonable convention for structs if not for hash-tables!
  (cl-struct-slot-value 'foo-struct (sym key) obj))
;;
;; NOTE actually, this maybe a IGet (or something) protocol from which we generate
;; the `mu-get' or whatever:
(mu-defprotocol mu-get-protocol
  (mu--get (obj key)))
(mu-extend mu-get-protocol
  :to hash-table
  (mu--get #'ht-get))


;; TODO Implement a generic `mu.' getter that works for any associative value.
;; Motivation: local variable names typically signal the type of thing they hold,
;; so e.g. var holding foo-struct would be called foo, then these become verbose:
;;
;;   (dr/session session scope files)
;;   (dr/drill drill meta props)
;;
;; may as well make them shorter and work even we don't know the type of struct:
;;
;;   (mu. session :scope :files)
;;   (mu. drill :meta :props)
;;
;; When local var holding struct doesn't convey what it is, we should still prefer
;; (foo-struct var ...) getter.
;;
;; Here's we could implement a generic getter `mu.' or call it `mu--val-at'
(cl-defgeneric mu--get (obj key))
;;
;; default method
(cl-defmethod mu--get ((obj t) key)
  (condition-case err
      (cl-struct-slot-value (type-of obj) (sym key) obj)
    (cl-struct-unknown-slot nil)))
;;
;; method when nil
(cl-defmethod mu--get ((obj (eql nil)) key)
  nil)
;;
;; method for hash-table
(cl-defmethod mu--get ((obj hash-table) key)
  (ht-get obj key))
;;
;; Generic `mu.' that should work for type that implements `mu--get':
(defun mu. (table key &rest keys)
  (when-let ((table (mu--get table key)))
    (if keys
        (apply #'mu. table keys)
      table)))
;;
(example
 (cl-defstruct foo-struct props)
 (cl-defmethod mu--get ((obj foo-struct) key)
   (cl-struct-slot-value 'foo-struct (sym key) obj))

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


(defun mu--set (val obj key &rest keys)
  (if keys
      (apply #'mu--set val `(mu--get ,obj ,key) keys)
    `(setf (mu--get ,obj ,key) ,val)))


(example
 (mu--set 42 'foo :props :a :b)
 ;; =>
 (setf (mu--get (mu--get (mu--get foo :props) :a) :b) 42)
 ;; example
 )


(gv-define-setter mu. (val table key &rest keys)
  (apply #'mu--set val table key keys))


;; NOTE setter isn't generic like `mu--get' proper. This asymmetry is unfortunate
;; but can't be helped (at least I can't think of a way) since we only discover
;; obj type at runtime but setter needs to rewrite at compile time. This may
;; actually be ok, since it would work for structs out of the box, any missing
;; case would likely be some native type, which user shouldn't really touch. It'd
;; be like overriding some core Clojure protocol on a native type with your own
;; implementation. It may work but may also break other code.
(gv-define-setter mu--get (val obj key)
  (let ((table (gensym "table")))
    `(let ((,table ,obj))
       (cond
        ;; hash-table
        ((ht-p ,table)
         (setf (ht-get ,table ,key) ,val))
        ;; struct
        ((recordp ,table)
         (setf (cl-struct-slot-value (type-of ,table) (sym ,key)) ,val))))))
;;
;;  cond in mu--get setter above suggests one possible strategy that's more
;; dynamic: alongside `mu--get' cl-defmethod user can register a pair
;;
;;   (type      => setf-expr)
;; or
;;   (predicate => setf-expr)
;;
;; Then we can simply generate as many branches as there're registered pairs. We
;; should probably pre-register a bunch of obvious ones: lists, vectors, records,
;; hash-tables, etc? GOOD: we avoid hard-coding the setter, BAD: anyone can
;; clobber and break other people's code by overriding say a setter for lists.
;; This could also be a feature not a bug though if every such defsetter specified
;; a dynamic defvar and the above main setter would use whatever setter is bound
;; by that var at any given time: unpredictable but works.
(mu-deftypesetter (val obj key) :type 'cons :body `(setf (nth ,key ,obj) ,val))
;; or more generic
(mu-defpredsetter (val obj key) :pred `(listp ,obj) :body `(setf (nth ,key ,obj) ,val))


(example
 ;; setter
 (let ((foo (make-foo-struct :props (ht (:a (ht (:b 1)))))))
   (setf (mu. foo :props :a :b) 2)
   (mu. foo :props :a :b))
 ;; example
 )


;; TODO generate getter named `struct-name' rewriting into `mu.' call
(foo-struct foo methods :a :b)
(foo-struct foo sub table :a :b)


;; TODO generate defsetter for `struct-name' by re-writing into `mu.'
(setf (foo-struct foo methods :a :b) 'val)
(setf (foo-struct foo sub table :a :b) 'val)


;; TODO Handle missing slots gracefully. Actually lets differentiate :key from a
;; struct :slot. (mu. struct :slot) should work, but also (mu. struct :key)
;; shouln't raise. (setf (mu. struct :key) 42) should assoc this :key with 42 as
;; expected, so that then (mu. struct :key) => 42.
;;
;; Here's how we could go about it: have every `mu-defstruct' inherit from a
;; `mu-prototype-struct':
;;
(cl-defstruct baz-struct a b)
(cl-struct-slot-value 'baz-struct 'foo (make-baz-struct :a 1))
;;   => error (cl-struct-unknown-slot foo-struct :foo)
;;
;; But I'd like an associative (Clojure like) behavior where we can set a missing
;; property on the struct as key.
;;
(cl-defstruct mu--proto-struct (mu--slots (ht)))
;; or maybe
;;   (cl-defstruct mu-struct (-slots (ht)))
;;   (mu-struct--slots (make-mu-struct))
(defun mu-struct? (obj)
  (mu--proto-struct-p obj))
;;
(cl-defstruct (baz-struct (:include mu--proto-struct)) a b)
(cl-defmethod mu--get ((obj baz-struct) key)
  (condition-case err
      (cl-struct-slot-value 'baz-struct (sym key) obj)
    ;; else lookup in mu--slots
    (cl-struct-unknown-slot (mu--get
                             (cl-struct-slot-value 'baz-struct 'mu--slots obj)
                             key))))
(setq bazzer (make-baz-struct :a 1))
(mu-struct? bazzer)
(mu--get bazzer :foo)
;; => nil
(setf (ht-get (cl-struct-slot-value 'baz-struct 'mu--slots bazzer) :foo) 42)
(mu--get bazzer :foo)
;; => 42
;;
;; So now our `mu--set' implementation could simply add slots that aren't struct
;; props to mu--slots table of every mu-struct.
;;
;; Only annoyance is that mu--slots slot makes printed representation of every
;; mu-struct terribly busy. But then they are already ugly.


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


;; TODO leverage my old custom defstruct implementation to guide mu-defstruct
(defgeneric destruct (struct &rest args)
  "Generic method to turn a STRUCT into an alist of (:slot .
  value).")


(defmacro dr/defstruct (name+props &rest slots)
  "Like cl-defstruct but
- avoids generating a :copier,
- avoids generating a default :constructor `make-foostruct',
- generates default :constructor `foostruct-create' instread, unless
- the caller provides an explicit (:constructor customfoobuild),
- generates `pcase-defmacro' to pcase match your foostruct,
- generates generic method `destruct' that turns struct into an alist.

pcase example:
 (dr/defstruct foo f1 f2)
 (pcase (foo-create :f1 1 :f2 2)
   ((foo f1 f2) etc))

destruct example:
 (destruct (foo-create :bar 1 :baz 2))
 ;; =>
 ((:bar . 1) (:baz . 2))
"
  (declare (indent defun))
  (let* (struct-name
         struct-props
         struct-slots
         (default-constructor
           (lambda (struct-name)
             (unless (assoc :constructor struct-props)
               (list
                `(:constructor ,(sym struct-name "-" "create"))))))
         (slot+accessor
          (lambda (slot)
            (pcase slot
              (`(,field ,_) (list (sym ":" field) (sym struct-name "-" field)))
              (field       (list (sym ":" field) (sym struct-name "-" field)))))))
    (pcase name+props
      (`(,name . ,props) (setf struct-name name
                               struct-props props))
      (name              (setf struct-name name
                               struct-popns nil)))
    (setf struct-props `((:constructor nil)
                         ,@(funcall default-constructor struct-name)
                         ,@struct-props
                         (:copier nil)))
    (setf struct-slots (-map slot+accessor slots))
    `(progn
       (cl-defstruct (,struct-name ,@struct-props) ,@slots)

       (pcase-defmacro ,struct-name (&rest patterns)
         (let ((name ',struct-name))
           `(cl-struct ,name ,@patterns)))

       (defmethod destruct ((struct ,struct-name) &rest args)
         (list
          ,@(-map
             (lambda (slot+accessor)
               (pcase slot+accessor
                 (`(,slot ,accessor) (list 'cons slot `(,accessor struct)))))
             struct-slots))))))
