;; -*- lexical-binding: t; -*-


(require 'multi-prelude)


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


(defmacro mu-defprotocol (name &rest methods)
  (declare (indent 1))
  (let ((by-length (lambda (a b) (> (length a) (length b)))))
    `(progn
       (defconst ,name (make-mu-protocol :name ',name))
       ,@(loop for (_ sym . rest) in methods
               with arglists
               with docstring
               do (setf arglists (seq-take-while #'listp rest)) and
               do (setf docstring (seq-drop-while #'listp rest)) and
               collect
               `(progn
                  (setf (ht-get (mu-protocol-methods ,name) ',sym)
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
               `(progn
                  ;; cache type
                  (setf (ht-get (mu-protocol-types ,protocol) ',type) t)
                  ;; generate cl-defmethods
                  ,@(mu--cl-defmethods type methods))))))


(defun mu-implements? (object protocol)
  (mu-extends? :type (type-of object) :protocol protocol))


(cl-defun mu-extends? (&key type protocol)
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


;;* mu-structs --------------------------------------------------- *;;


(cl-defstruct mu-struct
  (-keys (ht)))


(defun mu-type? (type)
  (and type
       (symbolp type)
       (get type :mu-type?)))


(defalias 'mu-struct? 'mu-struct-p)


(defmacro mu-defstruct (name-props &rest slots)
  (declare (indent defun))
  (let (struct-name
        struct-props
        include
        include-type)

    ;; extract props if present
    (pcase name-props
      (`(,name . ,props) (setf struct-name name
                               struct-props props))
      (name              (setf struct-name name
                               struct-props nil)))

    ;; extract :include if present
    (setq include (assq :include struct-props))
    (setq include-type (cadr include))
    (setf struct-props `((:include ,(or include-type 'mu-struct))
                         ,@(remove include struct-props)
                         (:copier nil)))

    `(progn

       ;; TODO not a fan of throwing, so when supplied :include isn't a
       ;; `mu-struct' we could generate a cl-defstruct with no mu-struct traits.
       ;; Pro: uniform interface. Con: if user intended for it to be a mu-struct
       ;; they will only discover when some mu-struct specific feature doesn't
       ;; work. Wonder what's the right choice here:

       ;; only allowed to inherit from a mu-type that is the type's inheritance
       ;; chain must be rooted in `mu-struct'
       (when (and ',include-type (not (mu-type? ',include-type)))
         (mu-error "no mu-struct in inheritance chain of type %s" ',include-type))

       ;; register new mu-type
       (put ',struct-name :mu-type? t)

       ;; defstruct
       (cl-defstruct (,struct-name ,@struct-props) ,@slots)

       ;; alias struct-pred? with struct-pred-p
       (defalias ',(sym struct-name "?") ',(sym struct-name "-p"))

       ;; store slots on the type symbol
       (cl-loop for (slot . _) in (cl-struct-slot-info ',struct-name)
                with slot-set = (ht)
                do (ht-set slot-set slot t)
                finally
                do (put ',struct-name :mu-slots slot-set))


       ;; TODO Use :mu-slots on STUCT type to avoid dispatch for KEY and
       ;; triggering of unkwnown-slot error when KEY isn't a slot
       (defun ,struct-name (struct key &rest keys)
         (apply #'mu. struct key keys))

       ;; TODO like above avoid delegating to mu.
       (gv-define-setter ,struct-name (val struct key &rest keys)
         `(setf (mu. ,struct ,key ,@keys) ,val))

       ;; TODO generate `mu-defpattern'
       )))


;;* mu-table-protocol -------------------------------------------- *;;


(mu-defprotocol mu-table-protocol
  (defmethod mu--slots (table))
  (defmethod mu--keys  (table))
  (defmethod mu--get   (table key))
  (defmethod mu--set   (table key value)))


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
    (mapcar #'car (cl-struct-slot-info (type-of obj))))

  (defmethod mu--keys (obj)
    (mu--slots obj))

  (defmethod mu--get (obj key)
    ;; covert any :key into 'key to lookup slot
    (let ((slot (sym key)))
      (when (member slot (mu--slots obj))
        (cl-struct-slot-value (type-of obj) slot obj))))

  (defmethod mu--set (obj key val)
    ;; convert any :key into 'key to setf slot
    (let ((slot (sym key))
          (type (type-of obj)))
      ;; TODO Is there any reasonabe way to store missing keys on a cl-struct?
      ;; Also we could just let the cl-struct machinery fail with its own missing
      ;; slot error and avoid checking here, but whatever.
      (if (member slot (mu--slots obj))
          (setf (cl-struct-slot-value type slot obj) val)
        (mu-error "cl-struct of type %s has no slot %s to set" type slot))))


  ;; TODO appears cl-generic defines a type-hierarchy e.g. I can specialize on
  ;; type `null' whose parent is symbol. That makes more sense.
  :to symbol
  ;; Today in WTF ELisp: special case to handle (type-of nil) => symbol. IMO
  ;; treating nil as an empty list fits averages lisper experience better.
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

  :to cons
  (defmethod mu--slots (obj)
    (when obj
      (cl-loop for i from 0 below (length obj)
               collect i)))

  (defmethod mu--keys (obj)
    (mu--slots obj))

  (defmethod mu--get (obj key)
    (nth obj key))

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


;;* mu. ---------------------------------------------------------- *;;


;; TODO Do we even want to check `mu-extends?' in mu. and friends below. Their
;; only PRO is they report errors in terms of protocol. But I wonder if an error
;; thrown by generic method would be just fine? Their biggest CON is that now
;; neither default (obj t) nor nil (obj (eql nil)) can be riched from mu.


(defun mu.slots (obj)
  (unless (mu-implements? obj mu-table-protocol)
    (mu-protocol-error mu-table-protocol obj "in `mu.slots'"))
  (mu--slots obj))


(defun mu.keys (obj)
  (unless (mu-implements? obj mu-table-protocol)
    (mu-protocol-error mu-table-protocol obj "in `mu.keys'"))
  (mu--keys obj))


(defun mu. (table key &rest keys)
  "Look up KEY in TABLE. Return nil if no such KEY. Works for any
TABLE that implements generic `mu--get'."
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


;;* provide ------------------------------------------------------ *;;


(provide 'multi-structs)


;;* todo --------------------------------------------------------- *;;


(comment
 ;; TODO Consider alternative implementation of `mu-implements?' that's cute but
 ;; hella scary. Also raises the question of performance. Dunno how good multiple
 ;; dispatch in cl-generic really is.

 (cl-defgeneric mu-implements? (obj protocol))

 ;; now every time protocol is extended to a type we add a method
 (cl-defmethod mu-implements? ((obj foo-struct) (protocol (eql 'table-protocol)))
   t)

 ;; NOTE that the above method will fire for any descendants of foo-struct that
 ;; don't necessarily extend this protocol explicitly

 ;; default
 (cl-defmethod mu-implements? ((obj t) (protocol t))
   (mu-error "object %S does not extend protocol %s" obj protocol))

 ;; now
 (mu-implements? (make-foo-struct) 'mu-table-protocol)
 ;; comment
 )


(comment
 ;; TODO symmetric `mu-extend', because mu-extend sounds hella ambiguous at least
 ;; for non-native English ears.

 (defmacro mu-extend-protocol (protocol &rest body))
 (defmacro mu-extend-type (type &rest body))

 (defmacro mu-extend (what &rest body)
   `(if (mu-protocol? what)
        (mu-extend-protocol ,what ,@body)
      (mu-extend-type ,what ,@body)))

 ;; now either should work and there'll no confusion
 (mu-extend protocol
   type
   (defmethod mu--get (obj key))
   (defmethod mu--set (obj key val))
   type
   (defmethod mu--get (obj key))
   (defmethod mu--set (obj key val)))

 (mu-extend type
   table-protocol
   (defmethod mu--get ())
   (defmethod mu--set ())
   seq-protocol)

 ;; comment
 )


(comment

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


 ;; TODO protocol methods should mention their protocol in docs. See how
 ;; `cl--generic-describe' does it.


 ;; TODO steal debug declaration from `cl-defmethod'


 ;; TODO register a mu-pattern for `struct-name' that works just like ht-pattern
 ;; but also tests with `struct-name-p'
 (mu-defstruct multi name)
 (setq m (make-multi :name 'foo))
 ;;
 (mu-case m
   ((multi name) name))
 ;; => 'foo


 ;; TODO corollary to that is to limit ht-pattern to just hash-tables and have a
 ;; separate (: key...) or (mu. key...) or (mu: key...) pattern for general match


 ;; TODO mu-callable protocol that effectively makes structs callable
 (mu-defprotocol mu-callable-protocol
   (defmethod mu--call (f &rest args))
   (defmethod mu--apply (f &rest args)))
 ;;
 ;; lets make struct callable (assume it defines some lambda in :call slot)
 (mu-extend mu-callable-protocol
   :to foo-struct
   (defmethod mu--call (foo &rest args) (apply (foo-struct foo :mu-call) args))
   (defmethod mu--apply (foo &rest args) (apply #'apply (foo-struct foo :call) args)))
 ;;
 ;; Actually we could just extend it to `mu-struct' or even any `cl-struct'. As
 ;; long as instance has function in its :call slot, any mu-defstruct would work
 (mu-extend mu-callable-protocol
   :to mu-struct
   (defmethod mu--call (obj &rest args) (apply (mu. obj :call) args))
   (defmethod mu--apply (obj &rest args) (apply #'apply (mu. obj :call) args)))
 ;;
 ;; Even better I can also have default cover any function, so that mu--call
 ;; effectively becomes a replacement for `funcall' and `apply'
 ;;
 (cl-defmethod mu--call ((obj t) &rest args) (apply obj args))
 (cl-defmethod mu--apply ((obj t) &rest args) (apply #'apply obj args))
 ;;
 ;; IMO `mu--apply' is redundant and can be derived from `m-call':
 (mu--apply foo arg (list a b))
 ;; =>
 (apply #'apply #'mu--call foo args)
 ;; ==
 (apply #'apply #'mu--call foo (list arg (list a b)))
 ;; =>
 (apply #'mu--call foo arg (list a b))
 ;; =>
 (mu--call foo arg a b)


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
    (:else (some default action))))


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

 ;; comment
 )
