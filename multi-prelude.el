;; -*- lexical-binding: t; -*-


(require 'cl-lib)
(require 'cl)
(require 'ht)


(defmacro example (&rest body) nil)
(defmacro comment (&rest body) nil)


(cl-defmacro default (binding &key ((:to expr)))
  `(condition-case err (unless ,binding (setf ,binding ,expr))
     (void-variable
      (error "Defaulting undefined variable %s is not allowed" ',binding))))


(defmacro fn (arglist &rest body)
  "Like lambda but wraps it in `cl-function' and thus supports
cl-arglist destructuring."
  (declare (indent defun))
  `(cl-function (lambda ,arglist ,@body)))


(defun sym (&rest args)
  "Interns a symbol by concatenating args that can be symbols,
strings or keywords (with leading colon removed). Like
Clojure's (str ...) but for interned symbols."
  (intern
   (mapconcat
    (lambda (s)
      (cond
       ((keywordp s) (substring (symbol-name s) 1))
       ((symbolp s) (symbol-name s))
       ((stringp s) s)
       (t (error "sym: don't know how to stringify obj %s" s))))
    args
    "")))


;; Define convenient setters for `ht-get' and `ht-get*'


;; `ht-get' setter
(gv-define-simple-setter ht-get ht-set! t)


(defun ht--set* (table keys val)
  (if (cdr keys)

      (progn
        ;; if table has no such key we're free to set it to a fresh table
        (unless (ht-contains? table (car keys))
          (setf (ht-get table (car keys)) (ht)))

        ;; invariant: we expect nested tables for every key but the last
        (unless (ht? (ht-get table (car keys)))
          (error "ht: expected to find a nested table under %s but found %s"
                 (car keys)
                 (ht-get table (car keys))))
        ;; recur
        (ht--set* (ht-get table (car keys)) (cdr keys) val))

    ;; finally set the deepest key to val
    (setf (ht-get table (car keys)) val)))


(defmacro ht-set* (table keys val)
  "Set KEYS path in nested hash tables to VAL, starting with TABLE.
The lookup for each key should return another hash table, except
for the final key, which may return any value. If any levels do
not exist tables will be created.

\(fn table (&rest keys) val)"
  `(ht--set* ,table (list ,@keys) ,val))


;; NOTE `ht-get*' is simply wrong. It should return nil if nested path doesn't
;; exist, just like `ht-get' returns nil for a key that's not present. As such
;; implementation is inconsistent! So, lets redefine it.


;; TODO Upstream this and and my `ht-set*' and gv-setter for `ht-get*'


(defun ht-get* (table &rest keys)
  "Returns the value in a nested TABLE for a sequence of KEYS.
  Returns nil if any key is not present."
  (when-let ((table (ht-get table (car keys))))
    (if (cdr keys)
        (apply #'ht-get* table (cdr keys))
      table)))


;; `ht-get*' setter
(gv-define-setter ht-get* (val table &rest keys)
  `(ht--set* ,table (list ,@keys) ,val))


(defmacro eval-when-compile-let (bindings &rest body)
  "Like `let' but only install BINDINGS for the duration of BODY
when compiling. Revert or unbind as needed right after."
  (declare (indent 1))
  (let* ((table (gensym "old-values"))
         (unbound (gensym "unbound"))
         (syms (mapcar #'car bindings)))
    `(progn

       (eval-when-compile
         ;; collect unbound symbols
         (setq ,unbound (seq-remove #'boundp '(,@syms)))
         ;; bind unbound symbols to nil
         (dolist (unbound-sym ,unbound) (set unbound-sym nil))
         ;; store old values for all symbols in a vector
         (setq ,table (vector ,@syms))
         ;; bind symbols to new values
         (setq ,@(apply #'append bindings)))

       ,@body

       (eval-when-compile
         ;; restore symbols to old-values
         (setq ,@(apply #'append (seq-map-indexed (lambda (s i) `(,s (aref ,table ,i))) syms)))
         ;; unbind symbols that were unbound before
         (dolist (unbound-sym ,unbound) (makunbound unbound-sym))
         ;; unbind temporaries
         (makunbound ',table)
         (makunbound ',unbound)))))


(defun mu--split-when (pred lst)
  "Partition the list LST on every item that satisfies predicate
PRED. Do not include such items into partitions. Return a list of
partitions."
  (cl-loop for item in lst
           if (funcall pred item)
           collect partition into partitions
           and do (setq partition '())
           else
           collect item into partition
           finally return (nconc partitions (list partition))))


(provide 'multi-prelude)
