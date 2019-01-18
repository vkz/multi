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


(comment
 (mu-implements? nil    mu-table-protocol)
 (mu-implements? t      mu-table-protocol)
 (mu-implements? '(foo) mu-table-protocol)
 (mu-implements? [1]    mu-table-protocol)
 (mu-implements? (make-foo-struct) mu-table-protocol)
 ;; comment
 )


(comment
 ;; TODO Alternatively, mu-extends? for instance could be this trick, which is cute
 ;; but hella scary and depending on how generic dispatch implemented may intro
 ;; significant overhead as the number of mu-types grow:
 (cl-defgeneric mu-implements? (obj protocol))
 ;; now every mu-defstruct generates
 (cl-defmethod mu-implements? ((obj foo-struct) protocol)
   (or (ht-get* mu-protocols :types 'foo-struct protocol)
       (mu-implements (make-mu-struct) protocol)))
 ;; default
 (cl-defmethod mu-implements? ((obj t) protocol)
   (mu-error "object %S does not extend protocol %s" obj protocol))
 ;; now
 (mu-implements? (make-foo-struct) 'mu-table-protocol)
 ;; comment
 )


(comment
 ;; TODO symmetric `mu-extend'

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

 (cl-defun mu-extends? (&key instance type protocol))
 (mu-extends? :instance baz :protocol mu-table-protocol)
 ;; comment
 )


(example

 (mu-defprotocol foo-protocol
   (defmethod foo-stringify (self) (self format) "docstring")
   (defmethod foo-symbolify (self) "docstring"))


 (cl-defstruct foo-struct (type :foo) name (value "always foo"))
 (cl-defstruct bar-struct (type :bar) nm (val "always bar"))

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


 (mu-implements? (make-foo-struct :name 'my-foo) foo-protocol)
 (mu-implements? (make-bar-struct :nm 'my-bar) foo-protocol)


 (foo-stringify (make-foo-struct :name 'my-foo) "string of %s value %s")
 (foo-symbolify (make-foo-struct :name 'my-foo))

 (foo-stringify (make-bar-struct :nm 'my-bar) "string of %s value %s")
 (foo-symbolify (make-bar-struct :nm 'my-bar))

 ;; example
 )


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


(mu-defprotocol mu-table-protocol
  (defmethod mu--slots (table))
  (defmethod mu--keys  (table))
  (defmethod mu--get   (table key))
  (defmethod mu--set   (table key value)))


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
             (setf (ht-get (mu-struct--keys ,obj) ,key) ,val)
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
   (mu-call (foo &rest args) (apply (mu. foo :mu-call) args))
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
    (:else (some default action))))


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
   (hierarchy :type 'ht)
   (sub :type 'bar-struct))
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

 ;; comment
 )
