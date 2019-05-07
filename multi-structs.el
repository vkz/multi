;; -*- lexical-binding: t; -*-

;; Copyright (C) 2018, 2019 by Vlad Kozin

(require 'multi-prelude)
;; we use `mu-rel' to enrich multi hierarchy with protocols
(require 'multi-methods)


;;* mu-protocols ------------------------------------------------- *;;


;; NOTE every METHOD declared in protocol NAME must be unique just like a function
;; declaration would since we are generating cl-defgenerics from each


(cl-defstruct mu-protocol name (methods (ht)) (types (ht)))


(example
 (make-mu-protocol
  :name    'my-silly-protocol
  :methods (ht (method-symbol list-of-arglists)
               ('method '((a) (a b) (a b c))))
  :types   (ht (type-symbol t)
               ('foo-struct t)))
 ;; example
 )


;; NOTE Protocols as structs stored in a defvar may have a downside. If someone
;; re-evals protocol creation site it'll be the same protocol conceptually, but
;; any types the original protocol extend will've been lost, so `mu-extends?' will
;; start failing for this new protocol value. Example: package defines a protocol
;; that types in other packages implement. If user ever evals the protocol
;; package, its dependants would have to be reloaded, too. Is this eventuality
;; something to be concerned about? TBH protocol is nothing but metadata - a tag
;; with a bunch of other tags (methods) attached. Making it a struct - an actual
;; compound value stored on a var is a matter of convenience, not required
;; complexity. Revisit if becomes a problem. Either go back to just symbolic tags
;; and keeping stuff in global tables, or cache everything in an external table
;; and have it sync with known protocols on request.


(defun mu-protocol-error (protocol obj &optional prefix)
  (mu-error :no-protocol (mu-protocol-name protocol) obj (type-of obj) (or prefix "")))


(defmacro mu-defprotocol (name &optional docstring &rest methods)
  "Create a new protocol NAME with a set of generic METHODS ..."
  (declare (indent 1) (debug t))

  (unless (stringp docstring)
    (push docstring methods)
    (setq docstring (format "`mu-protocol' instance for `%s'" name)))

  (let ((by-length     (lambda (a b) (> (length a) (length b))))
        (protocollable (sym (string-remove-suffix "-protocol" (symbol-name name)))))
    `(progn
       (defconst ,name (make-mu-protocol :name ',protocollable) ,docstring)
       ,@(loop for (_ sym . rest) in methods
               with arglists
               with docstring
               do (setf arglists (seq-take-while #'listp rest)) and
               do (setf docstring (seq-drop-while #'listp rest)) and
               collect
               `(progn
                  (setf (ht-get (mu-protocol-methods ,name) ',sym)
                        ',(sort arglists by-length))
                  (cl-defgeneric ,sym (self &rest args) ,@docstring)
                  (put ',sym :mu-protocol ',name))))))



(cl-defun mu--method-to-pcase-clause ((arglist . body))
  (let ((pattern (list '\` (mapcar (lambda (s) (list '\, s)) arglist))))
    `(,pattern ,@body)))


(defun mu--cl-defmethod (sym obj type rest methods &optional slots)
  `(cl-defmethod ,sym ((,obj ,type) &rest ,rest)
     (let (,@slots)
       (pcase (cons ,obj ,rest)
         ,@(mapcar #'mu--method-to-pcase-clause methods)
         (otherwise
          (mu-error "in protocol method %s arity error" ',sym))))))


(defun mu--cl-defmethods (type methods &optional slots)
  (loop for (sym . methods) in (seq-group-by #'second methods)
        with obj = (gensym "obj")
        with rest = (gensym "rest")
        collect
        (mu--cl-defmethod
         sym obj type rest (mapcar #'cddr methods)
         ;; bind slot symbols to respective slot values in obj
         (loop for slot in slots
               collect `(,slot (cl-struct-slot-value ',type ',slot ,obj))))))


(defmacro mu-extend (protocol &rest body)
  "Extend PROTOCOL to one or more existing types ..."
  (declare (indent 1) (debug t))
  (let* ((decl? (lambda (item) (eq :to item)))
         (decls (seq-remove #'null (mu--split-when decl? body))))
    `(progn
       ,@(loop for (type . methods) in decls
               collect
               `(progn

                  ;; cache type
                  (setf (ht-get (mu-protocol-types ,protocol) ',type) t)

                  ;; add an `isa?' relation to `mu-global-hierarchy'
                  (mu-rel ',type :isa (mu-protocol-name ,protocol))

                  ;; TODO need to `mu-rel' every child of that TYPE to PROTOCOL

                  ;; generate cl-defmethods
                  ,@(mu--cl-defmethods type methods))))))


(cl-defun mu-extends? (&key type protocol)
  "Check if PROTOCOL has been extended to TYPE"
  (or
   ;; try cache
   (ht-get (mu-protocol-types protocol) type)
   ;; cache miss, check ancestors
   (when-let ((struct-class (cl-find-class type)))
     (and (cl-some
           (lambda (parent-type) (mu-extends? :type parent-type :protocol protocol))
           (mapcar (lambda (parent)
                     (cl-struct-slot-value 'cl-structure-class 'name parent))
                   (cl--struct-class-parents struct-class)))
          ;; cache type if some ancestor implements protocol
          (setf (ht-get (mu-protocol-types protocol) type) t)))))


(defun mu-implements? (object protocol)
  "Check if OBJECT implements PROTOCOL"
  (mu-extends? :type (type-of object) :protocol protocol))


;;* mu-structs --------------------------------------------------- *;;


(defconst mu--struct-children nil
  "List of all struct types that inherit from `mu-struct'")


(cl-defstruct (mu-struct (:constructor mu-struct-create))
  (-keys (ht)))
(put 'mu-struct :mu-constructor #'mu-struct-create)


(defun mu-type? (type)
  "Check if symbol TYPE is tagged as a mu-type (inherits from
`mu-struct')"
  (and type
       (symbolp type)
       (get type :mu-type?)))


(defalias 'mu-struct? 'mu-struct-p)


(defmacro mu.new (type &rest args)
  "Uniform constructor that works for every mu-struct"
  `(funcall (get ',type :mu-constructor) ,@args))


(defmacro mu-defstruct (struct-name &rest slots)
  "Like `cl-defstruct' but with mu-struct extensions ..."
  (declare (indent defun) (debug t))
  (let (struct-props
        constructors
        new
        include
        include-type
        include-rest
        implements
        slot-names
        (slot?            (lambda (item) (not (keywordp item))))
        (implements-decl? (lambda (item) (eq :implements item)))
        (slot-name        (lambda (slot)
                            (cond
                             ((consp slot) (car slot))
                             ((symbolp slot) slot)
                             (:else (mu-error "in mu-defstruct malformed slots"))))))

    ;; split into name and properties
    (when (listp struct-name)
      (setq struct-props (cdr struct-name))
      (setq struct-name (car struct-name)))

    ;; extract all :constructors, :include and :new props
    (setq struct-props (cl-loop for prop in struct-props
                                if (eq :constructor (car prop))
                                do (pushnew prop constructors)
                                else if (eq :new (car prop))
                                do (setq new (cadr prop))
                                else if (eq :include (car prop))
                                do (setq include-type (cadr prop)
                                         include-rest (cddr prop))
                                else collect prop))

    ;; ensure that :new is present whenever custom :constructor
    (when constructors
      (unless new
        (mu-error
         "in mu-defstruct requires a :new struct prop whenever custom :constructors are defined")))

    ;; define "-create" suffixed constructor unless supplied
    (unless (some (fn ((_ name . rest)) (eq (sym struct-name "-create") name)) constructors)
      (pushnew `(:constructor ,(sym struct-name "-create")) constructors))

    ;; never create default constructor
    (pushnew '(:constructor nil) constructors)

    ;; put constructors
    (setf struct-props (append constructors struct-props))

    ;; put include back
    (push `(:include ,(or include-type 'mu-struct) ,@include-rest) struct-props)

    ;; extract all :implements attr options after slots
    (setq implements (seq-drop-while slot? slots)
          implements (mu--split-when implements-decl? implements)
          implements (seq-remove #'null implements))
    (setq slots      (seq-take-while slot? slots))
    (setq slot-names (mapcar slot-name slots))

    `(progn

       ;; TODO Instead of throwing unless :include is a `mu-struct' shouldn't we
       ;; subsume `cl-defstruct' by delegating to it without `mu-struct' business

       ;; only allowed to inherit from a mu-type that is the type's inheritance
       ;; chain must be rooted in `mu-struct'
       (when (and ',include-type (not (mu-type? ',include-type)))
         (mu-error "no mu-struct in inheritance chain of type %s" ',include-type))

       ;; register new `mu-struct' child
       (pushnew ',struct-name mu--struct-children)

       ;; register new mu-type
       (put ',struct-name :mu-type? t)

       ;; defstruct
       (cl-defstruct (,struct-name ,@struct-props) ,@slots)

       ;; alias struct-pred? with struct-pred-p
       (defalias ',(sym struct-name "?") ',(sym struct-name "-p"))

       ;; store the :new constructor on the type symbol
       (put ',struct-name :mu-constructor #',(or new (sym struct-name "-create")))

       ;; implement protocols
       ,@(cl-loop for (protocol . methods) in implements
                  collect
                  `(progn

                     ;; cache type
                     (setf (ht-get (mu-protocol-types ,protocol) ',struct-name) t)

                     ;; add an `isa?' relation to `mu-global-hierarchy'
                     (mu-rel ',struct-name :isa (mu-protocol-name ,protocol))

                     ;; generate cl-defmethods
                     ,@(mu--cl-defmethods struct-name methods slot-names)))

       ;; store slots on the type symbol
       (cl-loop for (slot . _) in (cl-struct-slot-info ',struct-name)
                with slot-set = (ht)
                ;; exclude special slots
                unless (or (memq slot '(cl-tag-slot -keys)))
                do (ht-set slot-set slot t)
                finally
                do (put ',struct-name :mu-slots slot-set))

       ;; TODO Unnecessary dispatch with mu. below, use :mu-slots to avoid

       ;; define struct-name getter
       (defun ,struct-name (struct key &rest keys)
         (apply #'mu. struct key keys))

       ;; register struct-name setter
       (gv-define-setter ,struct-name (val struct key &rest keys)
         `(setf (mu. ,struct ,key ,@keys) ,val))

       ;; TODO generate `mu-defpattern'
       )))


;;* mu-table-protocol -------------------------------------------- *;;


(mu-defprotocol mu-table-protocol
  "Protocol for table-like types. Define protocol methods
`mu--slots', `mu--keys', `mu--get', `mu--set'."
  (defmethod mu--slots (table)           "Return required TABLE keys")
  (defmethod mu--keys  (table)           "Return all TABLE keys")
  (defmethod mu--get   (table key)       "Look up KEY in TABLE, nil if not present")
  (defmethod mu--set   (table key value) "Set KEY to VALUE in TABLE"))


;; NOTE Curious pattern that maybe be of use is to leverage generics. User may
;; choose to extend a protocol to their new struct that augments some or all
;; methods but then delegates to an ancestor e.g. `mu-struct' by invoking
;; `cl-call-next-method', which to my surprise even lets you specify arguments to
;; use. Haven't tried but since protocol methods are generic methods I don't see
;; why it wouldn't work.


(mu-extend mu-table-protocol

  :to hash-table
  (defmethod mu--slots (obj)         (ht-keys obj))
  (defmethod mu--keys  (obj)         (ht-keys obj))
  (defmethod mu--get   (obj key)     (ht-get obj key))
  (defmethod mu--set   (obj key val) (setf (ht-get obj key) val))

  :to mu-struct
  (defmethod mu--slots (obj)
    (ht-keys (get (type-of obj) :mu-slots)))

  (defmethod mu--keys (obj)
    (append (ht-keys (get (type-of obj) :mu-slots))
            (ht-keys (mu-struct--keys obj))))

  (defmethod mu--get (obj key)
    ;; covert any :key into 'key to lookup slot
    (let ((slot (sym key))
          (type (type-of obj)))
      (if (ht-get (get type :mu-slots) slot)
          (cl-struct-slot-value type slot obj)
        ;; no such slot, lookup the original :key in --keys
        (ht-get (mu-struct--keys obj) key))))

  (defmethod mu--set (obj key val)
    ;; convert any :key into 'key to setf slot
    (let ((slot (sym key))
          (type (type-of obj)))
      (if (ht-get (get type :mu-slots) slot)
          (setf (cl-struct-slot-value type slot obj) val)
        ;; no such slot, setf the original :key in --keys
        (setf (ht-get (mu-struct--keys obj) key) val))))

  ;; root cl-struct
  :to cl-structure-object
  (defmethod mu--slots (obj)
    ;; 'cl-tag-slot is special e.g. u get a missing slot error if u try to get its
    ;; value even though techically its there, so we remove it.
    (remove 'cl-tag-slot (mapcar #'car (cl-struct-slot-info (type-of obj)))))

  (defmethod mu--keys (obj)
    (mu--slots obj))

  (defmethod mu--get (obj key)
    ;; covert any :key into 'key to lookup slot
    (let ((slot (sym key)))
      (when (member slot (mu--slots obj))
        (cl-struct-slot-value (type-of obj) slot obj))))

  ;; TODO Is there any reasonabe way to store missing keys on a cl-struct? Also we
  ;; could just let the cl-struct machinery fail with its own missing slot error.
  (defmethod mu--set (obj key val)
    ;; convert any :key into 'key to setf slot
    (let ((slot (sym key))
          (type (type-of obj)))
      (if (member slot (mu--slots obj))
          (setf (cl-struct-slot-value type slot obj) val)
        (mu-error "cl-struct of type %s has no slot %s to set" type slot))))

  ;; Today in WTF ELisp: (type-of nil) => symbol. IMO nil as empty list fits
  ;; average lisper experience better. Generics define rudimentary type-hierarchy
  ;; so maybe I should specialize on type `null' whose parent is symbol.

  :to symbol
  (defmethod mu--slots (obj)
    (when obj (symbol-plist obj)))

  (defmethod mu--keys (obj)
    (when obj (symbol-plist obj)))

  (defmethod mu--get (obj key)
    (when obj (get obj key)))

  (defmethod mu--set (obj key val)
    (cond
     (obj           (put obj key val))
     ((numberp key) (mu-error "in mu--set index out of bounds"))
     (:else         (mu-error "in mu--set expected a numeric key got %s" key))))

  ;; TODO Is there anything clever I can do about (setf (mu. var 0) val) where
  ;; var is either '() or []? Elisp fails to setf either. Prob because `() = nil
  ;; and isn't a place, and [] is out of bounds for index 0, so I don't much hope.

  :to cons
  (defmethod mu--slots (obj)
    (when obj
      (cl-loop for i from 0 below (length obj)
               collect i)))

  (defmethod mu--keys (obj)
    (mu--slots obj))

  (defmethod mu--get (obj key)
    (nth key obj))

  (cl-defmethod mu--set (obj key val)
    (setf (nth key obj) val))

  :to vector
  (defmethod mu--slots (obj)
    (when obj
      (cl-loop for i from 0 below (length obj)
               collect i)))

  (defmethod mu--keys (obj)
    (mu--slots obj))

  (defmethod mu--get (obj key)
    (aref key obj))

  (cl-defmethod mu--set (obj key val)
    (setf (aref obj key) val)))


;; TODO Do we even want to check `mu-implements?' in mu. and friends below. Their
;; only PRO is they report errors in terms of protocol. But I wonder if an error
;; thrown by generic method would be just fine? Their biggest CON is that now
;; neither default (obj t) nor nil (obj (eql nil)) can be riched from mu.


(defun mu.slots (obj)
  "Return required keys in OBJ. OBJ must implement
`mu-table-protocol'."
  (unless (mu-implements? obj mu-table-protocol)
    (mu-protocol-error mu-table-protocol obj "in `mu.slots'"))
  (mu--slots obj))


(defun mu.keys (obj)
  "Return all keys in OBJ. OBJ must implement
`mu-table-protocol'."
  (unless (mu-implements? obj mu-table-protocol)
    (mu-protocol-error mu-table-protocol obj "in `mu.keys'"))
  (mu--keys obj))


;; TODO that's bound to be slot because of mu. in the loop. Since OBJ type won't
;; change could we somehow use its getter and avoid dispatch?
(defun mu.vals (obj)
  "Return a list of all values stored in table OBJ"
  (cl-loop for key in (mu.keys obj)
           collect (mu. obj key)))


(defun mu. (table key &rest keys)
  "Look up KEYs in TABLE. Return nil if any KEYs missing. This is
a generalized variable and therefore `setf'-able. TABLE must
implement `mu-table-protocol'."
  (unless (mu-implements? table mu-table-protocol)
    (mu-protocol-error mu-table-protocol table "in mu."))
  (when-let ((table (mu--get table key)))
    (if keys
        (apply #'mu. table keys)
      table)))


(defun mu--set* (val obj key keys)
  (unless (mu-implements? obj mu-table-protocol)
    (mu-protocol-error mu-table-protocol obj "in mu."))
  (if keys
      (let ((table (or (mu--get obj key) (mu--set obj key (ht)))))
        (mu--set* val table (car keys) (cdr keys)))
    (mu--set obj key val)))


(gv-define-setter mu. (val table key &rest keys)
  `(if (mu-implements? ,table mu-table-protocol)
       (mu--set* ,val ,table ,key (list ,@keys))
     (mu-protocol-error mu-table-protocol table "in mu.")))


(gv-define-setter mu: (val table key &rest keys)
  `(if (mu-implements? ,table mu-table-protocol)
       (mu--set* ,val ,table ,key (list ,@keys))
     (mu-protocol-error mu-table-protocol table "in mu.")))


(defalias 'mu:slots 'mu.slots)
(defalias 'mu:keys 'mu.keys)
(defalias 'mu: 'mu.)


;;* mu-equatable-protocol ----------------------------------------- *;;


;; TODO note that for objects to be equal we demand that they have the same type.
;; This is reasonable and imo what u want most of the time. But I wonder if its
;; worth having a relaxed version that only performs structural equality check,
;; then e.g. '(1) and (ht (0 1)) would be equal. Is it valuable?


;; TODO Hash-table equality as (ht) defines it compares keys with `equal'. Looking
;; up and therefore comparing keys will work for most compound values but somehow
;; not for hash-tables:
;;
;; (ht-get (ht ('(1) 1)) '(1)) => 1
;; (ht-get (ht ((bar-struct-create) 42)) (bar-struct-create)) => 42
;;
;;   but since (not (equal (ht) (ht)))
;;
;; (ht-get (ht ((ht) 1)) (ht)) => nil
;;
;; This has far-reaching and unfortunate consequences:

;; cl-struct
;;   (equal (bar-struct-create) (bar-struct-create)) => t
;;
;; mu-struct because it alraedy installs hash-table as default in a slot
;;   (equal (foo-struct-create) (foo-struct-create)) => nil
;;
;; The only reasonable way to allow comparing tables with keys that are themselves
;; hash-tables IMO is to implement your own hash-table supplying your own equality
;; test and hash function to `make-hash-table', see `define-hash-table-test'


(mu-defprotocol mu-equatable-protocol
  "Protocol for deep equality. Define protocol methods
`mu--equal'."
  (defmethod mu--equal (l r)))


(defun mu--equal-associative (l r)
  (and (eq (type-of l) (type-of r))
       (= (length (mu.keys l))
          (length (mu.keys r)))
       (every
        (lambda (key) (mu--equal (mu. l key) (mu. r key)))
        (mu.keys l))))


(mu-extend mu-equatable-protocol

  :to hash-table
  (defmethod mu--equal (l r)
    (mu--equal-associative l r))

  :to cl-structure-object
  (defmethod mu--equal (l r)
    ;; try simple equality that works for simple structs
    (or (equal l r)
        ;; but if structs have hash-tables as values `equal' isn't enough
        (mu--equal-associative l r))))


(cl-defmethod mu--equal ((l t) r)
  (equal l r))


(defalias 'mu.equal 'mu--equal)
(mu-docfun mu.equal
  "Test if OBJ1 and OBJ2 are of the same type and structurally equal.
Unlike `equal' perform deep equality comparison of hash-tables as
values. Like `equal' report nil when comparing hash-tables that
have hash-tables as keys.

\(fn obj1 obj2)")


;;* mu-callable-protocol ----------------------------------------- *;;


(mu-defprotocol mu-callable-protocol
  "Protocol for types that exhibit function-like behaviour.
Define protocol method `mu--call'."
  (defmethod mu--call (f args) "Call object F with ARGS"))


(mu-extend mu-callable-protocol

  ;; NOTE both structs and hash-tables pass self as the first argument to their
  ;; function be it :call member or the default lookup with mu. IMO it makes
  ;; perfect sense and offers great power.

  ;; should cover any struct including `mu-struct'
  :to cl-structure-object
  (defmethod mu--call (obj args)
    (if-let ((f (or (get (type-of obj) :call) (mu. obj :call))))
        (apply f obj args)
      (apply #'mu. obj args)))

  :to hash-table
  (defmethod mu--call (obj args)
    (if-let ((f (ht-get obj :call)))
        (apply f obj args)
      (apply #'mu. obj args))))


;; default method delegates to `funcall' to cover normal functions
(cl-defmethod mu--call ((obj t) args) (apply obj args))


(defun mu.call (f &rest args)
  "Like `funcall' but invoke object F with ARGS. Unless F
implements `mu-callable-protocol' it is assumed to be a function
and `funcall' is used."
  (mu--call f args))


(defun mu.apply (f &rest args)
  "Like `apply' but apply object F to ARGS. Unless F implements
`mu-callable-protocol' it is assumed to be a function and `apply'
is used."
  (apply #'apply #'mu.call f args))


(defmacro mu-defcallable (struct function)
  "Put FUNCTION into :call property of STRUCT-symbol."
  `(put ',struct :call ,function))


;; NOTE making any instance of a struct callable is as trivial as
;;
;;   (mu-defstruct foo-struct
;;     (call (lambda (self &rest args) 42))
;;     props)
;;
;; which happily works for any cl-struct under your control. One downside is that
;; that lambda and extra slot will be carried by every instance. Alternatively:
;;
;;   (mu-defcallable foo-struct (lambda (self &rest args) 42))
;;
;; Will put call on the symbol. No extra slot and you can make callable any struct
;; even ones u don't control. This last bit is actually terrifying, so I wonder if
;; this is a good idea, but hey this is Emacs Lisp.


;;* docs --------------------------------------------------------- *;;


;; TODO Protocol methods must mention and link to their protocol. Also the right
;; thing to do is what `cl--generic-describe' does. It shows every arglist and
;; tries to link to every method implementation, but we re-write arglists so they
;; don't look like something user specified and at best link to `cl-defgeneric'.
;; Worthy goal but atm isn't the rabbit hole I'm eager to jump into.


(mu-docfun mu-defprotocol
  "Combine a set of generic METHODS as protocol NAME.

----------------------------------------------------------
NAME    = protocol-id

METHODS = (method ...)

method  = (defmethod method-id arglist [docstring] . rest)

rest    = see `cl-defgeneric'
----------------------------------------------------------

Bind variable NAME to the newly created `mu-protocol' struct.
Translate every method to a `cl-defgeneric' (which see). Store
arglists as metadata and for documentation but otherwise ignore.
Tag every method-id symbol with a property :mu-protocol.

Protocol METHODS are cl-generic functions that dispatch on the
type of their first argument.")


(mu-docfun mu-extend
  "Extend PROTOCOL to one or more existing types.

------------------------------------------------------------------------
  PROTOCOL = protocol-id

      TYPE = type-id

    method = (defmethod method-id [qualifiers] arglist [docstring] body)

   arglist = ((arg-id type-id) arg ...)
           | see `cl-defmethod'

qualifiers = see `cl-defmethod'
------------------------------------------------------------------------

Also register an `isa?' relation between TYPE and protocol name
as reported by (mu-protocol-name PROTOCOL) in the active
multi-methods hierarchy. Do the same for each descendant of TYPE.

To extend protocols to structs under your control consider using
:implements option of `mu-defstruct' instead.

\(fn PROTOCOL [:to TYPE method ...] ...+)")


(mu-docfun mu-defstruct
  "Like `cl-defstruct' but with mu-struct extensions.

------------------------------------------------------------------------
      NAME = struct-id
           | see `cl-defstruct'

      SLOT = slot-id
           | see `cl-defstruct'

  PROTOCOL =  protocol-id

    METHOD = (defmethod method-id [qualifiers] arglist [docstring] body)

   arglist = ((arg-id type-id) arg ...)
           | see `cl-defmethod'

qualifiers = see `cl-defmethod'
------------------------------------------------------------------------

Every mu-struct implicitly inherits from `mu-struct' type. If
:include struct property is present its value must be a type that
ultimately inherits from `mu-struct'. Any other type will raise
an error.

Define extra predicate of the form NAME? as alias for NAME-p.

Define NAME as a getter function for slots and keys of the
struct. Make NAME a generalized `setf'-able variable (see `mu.').
In general mu-structs are open maps whose keys are not limited to
slots. Generalized variables `mu.' (or `mu:') and NAME can be
used to set slots or keys of a struct.

Slots maybe followed by protocol implementations. Every protocol
implementation starts with :implements attribute followed by
protocol-name, followed by method implementations. Multiple
methods maybe implemented for the same method-id but different
arities. Since protocol methods dispatch on the type of their
first argument every method will have the structure instance
bound to it. Each method body implicitly binds every slot-id to
its respective value in the structure instance.

Set two properties on struct-id symbol :mu-type? tagging it as a
`mu-struct' and :mu-slots that keeps a list of all slot-ids.

\(fn NAME SLOT ... [:implements PROTOCOL METHOD ...] ...)")


;;* provide ------------------------------------------------------ *;;


(provide 'multi-structs)


;;* todo --------------------------------------------------------- *;;

;; TODO Use `mu:' as accessor, `mu.' for functions e.g. `mu.fold', `mu.map' etc.

(comment

 ;; TODO lazy-streams by defining `mu-streamable-protocol'. Then someone could
 ;; implement a crap ton of cool functions that work with sequences. See Racket
 ;; `gen:stream' for inspiration.
 (mu-defprotocol mu-streamable-protocol
   (defmethod mu--stream-empty? (stream))
   (defmethod mu--stream-first (stream))
   (defmethod mu--stream-rest (stream)))
 ;; comment
 )

(comment

 ;; TODO `mu-extend' establishes isa? rel between type and protocol as it should.
 ;; Problem is inheritance. Any descendants of the struct inherit the protocols it
 ;; implements. But `isa?' check atm can't know that, it only checks for equality
 ;; and active hierarchy. For structs defined with `mu-defstruct' we could collect
 ;; all protocols in its inheritance chain and register relations in the hierarchy,
 ;; but `cl-structs' aren't under our control. Is there a way to discover all
 ;; structs defined in the system? Then we could pre-populate global hierarchy. If
 ;; not then we'd have to add potentially expensive check to `isa?' one that given
 ;; a symbol (any symbol, cause types are symbols, argh) checks if it has a class,
 ;; if so collects all its parents, collects all their protocols, updates
 ;; hirerarchy as needed and then answers the `isa?' request. Yes, we can amortize
 ;; by caching but man this is unpleasant. It'll a real perf hit especially when
 ;; hierarchy is empty and for any symbol no less. Beginning to thin it isn't worth
 ;; it. Should it then be on the user to register appropriate relations?
 ;;
 ;; Turns out I can get children! Yay!
 ;;
 (symbol-value (cl--struct-class-children-sym (cl-find-class 'mu-struct)))
 ;; => all mu-structs
 (symbol-value (cl--struct-class-children-sym (cl-find-class 'cl-structure-object)))
 ;; => all cl-structs
 ;;
 ;; This also means that there may be a way to pre-populate global-hierarchy with
 ;; type relations i.e. inheritance relations for all structs (beyond protocols).
 ;; Not saying its better than dynamic check and caching but there needs to be at
 ;; least some form check in `isa?'. Choices, choices. Sigh.
 ;;
 ;; Compromise solution could be to serialize all parent-child relations for every
 ;; single struct defined in an "empty" Emacs installation (no packages). Then
 ;; load those when `multi' is its load time isn't effected by extra computation.


 ;; TODO register a mu-pattern for `struct-name' that works just like ht-pattern
 ;; but also tests with `struct-name-p'
 (mu-defstruct multi name)
 (setq m (make-multi :name 'foo))
 (mu-case m ((multi name) name))
 ;; => 'foo


 ;; TODO corollary to that is to limit ht-pattern to just hash-tables and have a
 ;; separate (: key...) or (mu. key...) or (mu: key...) pattern for general match


 ;; TODO arity dispatch in protocol methods doesn't require `pcase'. Suppose we
 ;; store method to arity map for every method in `mu-protocols':
 ;;
 (ht ('mu--get (ht (2 (lambda (obj key) body))
                   (3 (lambda (obj k1 k2) body)))))
 ;;
 ;; then dispatch becomes trivial, also simplifies arity checks etc
 (cl-defmethod mu--get ((obj type) &rest args)
   (let ((args (cons obj args)))
     ;; apply stored relevant method implementation explicitly
     (apply (ht-get mu-protocols 'mu--get (length args)) args)))


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


 ;; TODO what does it mean to extend protocol to another protocol? Semantically
 ;; IMO it's as simple as treating protocol as a type, so any type that is `isa?'
 ;; protocol-type should match. With multiple inheritance we could probably solve
 ;; this with cl-generics alone, maybe. But cl-generics don't perform an `isa?'
 ;; check, so unless I hack and extend them, I guess we should use multi-methods
 ;; for this particular case? It should be rare enough, right?
 (mu-defprotocol foolable-protocol
   (defmethod fool (obj))
   (defmethod fooled (obj)))
 ;; =>
 ;; register generics
 (defgeneric fool (obj))
 (defgeneric fooled (obj))
 ;; register multi-methods
 (mu-defmulti multi-fool (obj) (type-of obj))
 (mu-defmulti multi-fooled (obj) (type-of obj))
 ;; generate default generic methods that delegate to multi
 (defmethod fool ((obj t))
   ;; delegate to multi-method
   (multi-fool obj))
 (defmethod fooled ((obj t))
   ;; delegate to multi-method
   (multi-fooled obj))
 ;;
 (mu-isa-extend foolable-protocol
                :to bazzable
                (defmethod fool (obj))
                (defmethod fooled (obj)))
 ;; =>
 (mu-rel 'bazzable :isa 'foolable)
 (mu-defmethod multi-fool (obj) :when 'bazzable)
 (mu-defmethod multi-fooled (obj) :when 'bazzable)
 ;; dispatch will go
 (isa? (type-of obj) 'bazzable)
 ;; => t
 foo-struct implements bazzable-protocol
 foo-struct implements barrable-protocol
 ;; then we extend foolable to both protocols
 (mu-isa-extend foolable-protocol
                :to bazzable
                :to barrable)
 ;; =>
 (mu-rel 'bazzable :isa 'foolable)
 (mu-rel 'barrable :isa 'foolable)
 ;; but this creates hierarchical ambiguity, which in multi-methods only the user
 ;; can prefer away unlike in generics that order by most precise method
 (mu-defmethod multi-fool (obj) :when 'bazzable)
 (mu-defmethod multi-fool (obj) :when 'barrable)
 ;; Is this contrived or can this actually happen?


 ;; TODO or perhaps even have multi-methods based protocols in addition to
 ;; multi-protocols - mostly like protocols but use `isa?' dispatch on the
 ;; `type-of' of their first argument to dispatch to multi-methods:
 (mu-defmulti-protocol multi-foolable-protocol
                       (defmethod multi-fool (obj))
                       (defmethod multi-fooled (obj)))
 ;; =>
 (defconst multi-foolable )
 (mu-defmulti multi-fool (obj) (type-of obj))
 (mu-defmulti multi-fooled (obj) (type-of obj))
 ;; then
 (mu-isa-extend multi-foolable-protocol
                :to bazzer
                (defmethod fool (obj))
                (defmethod fooled (obj)))
 ;; =>
 ;; update metadata, then
 (mu-rel 'bazzable :isa 'foolable)
 (mu-defmethod multi-fool (obj) :when 'bazzer)
 (mu-defmethod multi-fooled (obj) :when 'bazzer)


 ;; NOTE above example suggests interesting pattern - dispatch to a multi-method
 ;; inside a generic method when dispatching on type is too coarse:
 (cl-defmethod some-method ((obj t) arg)
   (cond
    ;; ... earlier branches ...
    ((pred? obj) (mu--multi-get obj arg))
    ;; ... later   branches ...
    (:else (some default action))))

 ;; comment
 )
