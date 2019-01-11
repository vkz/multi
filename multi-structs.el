;; -*- lexical-binding: t; -*-

(require 'multi-prelude)

;; TODO What do I want from `mu-defstruct'

;; generic `mu-destruct' that converts struct into hash-table

;; register a mu-pattern for `struct-name' that works just like ht-pattern but
;; also tests with `struct-name-p'

(mu-defstruct multi name )
(setq m (make-multi :name 'foo))

(mu-case m
  ((multi name) name))
;; => 'foo

(mu-destruct m)
;; =>
(ht (:name 'foo))

;; generates getter/setter macro `struct-name' that can customize getter and
;; setter for slots based on annotations provided.

(mu-defstruct foo-struct
              (methods (ht) :type 'ht)
              (prefers (ht) :type 'ht)
              (hierarchy    :type 'ht)
              (sub          :type 'bar-struct))

(mu-defstruct bar-struct
              name
              (table (ht) :type 'ht))

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

;; I could simplify the above setters and getters if I were to implement a
;; threading macro `as->' so that it doesn't compute intermediate steps but simply
;; rewrites into nested calls. Then we could thread `setf' and friends

;; macro getter
`(mu-> ,struct it
       (cl-struct-slot-value 'foo-struct ',slot it)
       (bar-struct it ,@keys))
;; ==
`(bar-struct (cl-struct-slot-value 'foo-struct ',slot ,struct) ,@keys)

;; macro setter
`(mu-> ,struct it
       (cl-struct-slot-value 'foo-struct ',slot it)
       (bar-struct it ,@keys)
       (setf it 'val))
;; ==
`(setf (bar-struct (cl-struct-slot-value 'foo-struct ',slot ,struct) ,@keys) ,val)


;; With our getter/setter `foo-struct' macro above, we can:

;; get stuff
(foo-struct foo methods :a :b)
(foo-struct foo sub table :a :b)

;; set stuff
(setf (foo-struct foo methods :a :b) 'val)
(setf (foo-struct foo sub table :a :b) 'val)


;; TODO In the most typical case local variables of type foo-struct would be
;; called foo-struct. It is only naturaly and I also do it, after all names should
;; be meaningful. Examples: drill of type dr/drill, session of type dr/session,
;; hierarchy of type mu-hierarchy. This introduces unnecessary repetition. IMO
;; there's value in being able to linearly read what's being extracted, so perhaps
;; instead of:
;;
;;   (dr/session session scope files)
;;   (dr/drill drill meta props)
;;
;; we should implement >> or mu. or mu: (latter better conveys associativity?)
;;
;;   (>> session scope files)
;;   (>> drill meta props)
;;
;; No reason this wouldn't work for hash-tables. All it takes is looking up the
;; type with `type-of'. So it could either be a generic or a multi method that can
;; be further extended: (defgeneric mu. (struct &rest keys)). Still worth having a
;; getter macro for every struct IMO for cases when there're multiple objects of
;; the same type or you want short names that don't necessarily convey the type of
;; the object.
;;
;; Here's we could implement a generic getter `mu.':
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
;;
;; But we also need to handle missing slots somehow that is I'd like an
;; associative (Clojure like) behavior we should be able to set a missing property
;; on the struct as key.
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
;; mu-struct terribly busy. But then again (pp struct) isn't much better.

;; TODO Generic `mu--get' above points at a cool generalization: `mu-protocols'.
;; Here's a brief outline of how it could be implemented. Every protocol is just a
;; set of methods that need to be implemented, where each method is a defgeneric
;; that dispatches on one the type of one of its arguments:
(defmacro mu-defprotocol (name &rest methods)
  (declare (indent 1)))
(defmacro mu-extend (protocol &rest body)
  (declare (indent 1)))
;;
;; define new protocol
(mu-defprotocol map-proto
  ;; self is special - its the arg whose type we dispatch on
  (mu.meth-1 (_ self _))
  (mu.meth-2 (self _)))
;; Simply registers a new protocol
;;
;;     (ht-set mu-protocols 'map-proto (ht ('mu.meth-1 '(_ self _))
;;                                         ('mu.meth-2 '(self _))))
;;
;; Nowe we can extend map-proto to a foo-struct type
(mu-extend map-proto
  :to foo-struct
  (mu.meth-1 (a foo b) body)
  (mu.meth-2 (foo arg) body))
;; Does two things:
;; 1. add 'foo-struct type to mu-proto-registry
;;
;;     (ht-set mu-proto-registry 'foo-struct 'map-proto)
;;
;; 2. define generic methods for each protocol method
;;
;;     (cl-defmethod mu.meth-1 (a (foo foo-struct) b) body)
;;     (cl-defmethod mu.meth-2 ((foo foo-struct) arg) body)
;;
;; Suppose now we have protocols:
;;
;;   map-proto - associative data
;;   seq-proto - sequential data (e.g. vectors, lists etc)
;;   gv-proto - place like data (rewrites into existing gv)
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

;; IDEA if we are going the route of this generic associative mu: getter, maybe we
;; should go all the way and define all typical associative functions in a generic
;; way so that e.g. mu:keys mu:select mu:map etc would work for structs,
;; hash-tables and whatever.
;;
;; Since `type-of' is built-in C function that we can't extend, at least not
;; easily. Wonder if there's a way to implement an extensible `mu-type' somehow.

;; We could of course just use threading macro, but the `dash' ones won't work
;; with `setf' and friends:

(-> foo
    (foo-struct methods)
    (ht-get* :a :b))

(-> foo
    (foo-struct sub)
    ;; notice we need to remember sub is 'bar-struct here
    (bar-struct table)
    (ht-get* :a :b))


;; TODO generic associative getter

(mu-get foo key1 key2)

(mu-get (ht (key1 (ht (key2 'val)))) key1 key2)
;; => 'val

;; not sure about alists - alists are stupid - hate em
(mu-get (ht (key1 ((key2 . 'val)))) key1 key2)
;; => 'val

(mu-get (ht (key1 (foo-struct :key2 'val))) key1 key2)
;; => 'val

(mu-get (ht (key1 (list 'val))) key1 0)
;; => 'val

(mu-get (ht (key1 [val])) key1 0)
;; => 'val

;; then setter could just be
(setf (mu-get foo key1 key2) 'val)

;; Getter could be implemented in terms of `mu->' I think

(mu-get foo key1 key2)
(mu-> foo
      (mu--get it key1)
      (mu--get it key2))
;; where `mu--get' performs a single key lookup. Maybe we can even make it
;; `defgeneric' or a multi-method so that users can extend it?


;; TODO my old custom defstruct that I might turn into mu-defstruct

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
