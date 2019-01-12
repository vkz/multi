;; -*- lexical-binding: t; -*-


(require 'multi-prelude)


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


;; TODO generates getter/setter function `struct-name' by e.g. rewriting into
;; `mu.' call
;;
;; get stuff
(foo-struct foo methods :a :b)
(foo-struct foo sub table :a :b)
;;
;; set stuff
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


;; TODO Generic `mu--get' above points at a cool generalization: `mu-protocols'.
;; Conceptually protocol is just mete-data - set of methods to implement, each a
;; defgeneric that dispatches on a pre-defined arg type (not just first arg!):
(defmacro mu-defprotocol (name &rest methods) (declare (indent 1)))
(defmacro mu-extend (protocol &rest body) (declare (indent 1)))
;;
;; define new protocol
(mu-defprotocol map-proto
  ;; self is special - its the arg whose type we dispatch on. It absolutely has to
  ;; be one of required args. Instead of allowing multiple definitions of the same
  ;; method for different arrities like Clojure, I'd rather allow multi-head defun
  ;; body in method implementation. That way we can reuse `cl-defmethod' machinery
  ;; (that doesn't allow multiple arrities for the same generic) and already
  ;; implemented `mu-defun':
  (mu.meth-1 (_ self &rest args))
  (mu.meth-2 (self _)))
;; Simply registers a new protocol
;;
;;     (ht-set mu-protocols 'map-proto (ht ('mu.meth-1 '(_ self _))
;;                                         ('mu.meth-2 '(self _))))
;;
;; Nowe we can extend map-proto to a foo-struct type
(mu-extend map-proto
  :to foo-struct
  (mu.meth-1 (_ self &rest args)
             ([a self]   (do body))
             ([a self b] (do body)))
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
