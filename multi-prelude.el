;; -*- lexical-binding: t; -*-


(require 'cl-lib)
(require 'cl)
(require 'gv)
(require 'ht)


;; TODO I want both to behave as if I actually commented some sections out, that
;; is I'd like to avoid eval of either of these because I'd like in cases where
;; comment is the last expression to return value of the previous one, not the
;; comment, e.g.: (progn 'foo (comment 'bar)) should return 'foo rather than nil.
;; Normally I could probably do this at reader level, but not in elisp, so maybe I
;; could walk the body and somehow hoist every example and comment to the
;; beginning of the "file"?
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


(defmacro with-gensyms (syms &rest body)
  "For every symbol in SYMS gensym a symbol from its
`symbol-name', make it available in the BODY by let-binding it to
SYM."
  (declare (indent 1)
           (debug ((&rest symbolp) def-body)))
  (let ((bindings (cl-loop for s in syms
                           collect (list s `(gensym (symbol-name ',s))))))
    `(let ,bindings
       ,@body)))


;; Define convenient setters for `ht-get' and `ht-get*'


;; `ht-get' setter
(gv-define-simple-setter ht-get ht-set! t)


;; TODO we could make it a generic that works on hash-tables, alists and plists
;; simply by dispatching on the TABLE argument.
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
  ;; TODO oops bug if no keys then we'll end up with (ht-get table nil), we should
  ;; check with (if keys ...) first
  (when-let ((table (ht-get table (car keys))))
    (if (cdr keys)
        (apply #'ht-get* table (cdr keys))
      table)))


;; `ht-get*' setter
(gv-define-setter ht-get* (val table &rest keys)
  `(ht--set* ,table (list ,@keys) ,val))


(example
 (let ((tb (ht)))
   (setf (ht-get tb :foo) '(0))
   (push 1 (ht-get tb :foo))
   tb)
 ;; => (ht (:foo '(1 0)))

 (let ((tb (ht (:foo (ht (:bar 1))))))
   (ht-set* tb (:foo :bar) 2)
   (incf (ht-get* tb :foo :bar))
   tb)
 ;; => (ht (:foo (ht (:bar 3))))

 (let ((tb (ht)))
   (setf (ht-get* tb :foo :bar :baz) 3)
   tb)
 ;; => (ht (:foo (ht (:bar (ht (:baz 3))))))

 (let ((tb (ht)))
   (push 3 (ht-get* tb :foo :bar :baz))
   (pushnew 4 (ht-get* tb :foo :bar :baz))
   tb)
 ;; => (ht (:foo (ht (:bar (ht (:baz 4 3))))))

 ;; example
 )


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


;;* mu-error ----------------------------------------------------- *;;


;; Introduce a custom mu-error to differentiate signals specific to the multi
;; feature. Consider raising mu-error whenever it relates to multi-pattern
;; matching or multi-dispatch. `mu-error' function simplifies this by
;; intentionally following the exact same calling convention as `error'. Please,
;; use it.


(define-error 'mu-error "mu-error")


(defconst mu--errors
  (ht

   ;; multi-structs
   (:no-protocol             '("protocol %s does not extend to object %S of type %s %s"))

   ;; multi-patterns
   (:lst-pattern             '("in mu-case lst-pattern doesn't support &rest,"
                               " use l-pattern instead in: %S"))
   (:vec-pattern             '("in mu-case vec-pattern doesn't support &rest,"
                               " use v-pattern instead in: %S"))
   (:pattern                 '("in mu-case unrecognized pattern %S"))
   (:ht-pattern              '("in mu-case malformed ht pattern in %S"))
   (:seq-pattern             '("in mu-case seq pattern applied to unrecognized type %s"))
   (:rest-pattern            '("in mu-case malformed &rest pattern %S"))
   (:let-malformed           '("in mu-let malformed binding list in %S"))
   (:defun-malformed-arglist '("in mu-defun malformed arglist %S"))
   (:defun-no-match          '("in mu-defun no matching clause found for call %s"))
   (:defun-malformed-body    '("in mu-defun malformed body %S"))
   (:defun-return            '("in mu-defun :return attribute must be a variable name"))
   (:setter-no-match         '("in mu-setter no matching clause for %s"))

   ;; multi-methods
   (:lexical-binding         '("mu-methods require `lexical-binding' to work properly. "
                               "If you know what you are doing you may disable this check "
                               "by unsetting `mu-lexical-binding'."))

   (:rel-semantics           '("in mu-rel no meaningful semantics "
                               "relate structured data\n  %s\n  %s"))

   (:rel-cycle               '("in mu-rel cyclic relationship between %s and %s: %s"))

   (:malformed-methods       '("in mu-methods malformed arglist at %s"))

   (:cyclic-prefer           '("in mu-prefer cyclic preference %s over %s "
                               "would form a cycle %s"))

   (:malformed-prefer        '("in mu-prefer malformed arglist at %s"))

   (:malformed-unprefer      '("in mu-unprefer malformed arglist at %s"))

   (:ambiguous-methods       '("multiple methods match in multi-method call %s "
                               "dispatch value %s:\n%s\n"))

   (:inconsistent-prefers    '("possible cycle in prefers in multi-method call %s "
                               "for dispatch value \n%s\n"
                               "with hierarchy:\n%S\n"
                               "with prefers:\n%S\n"))

   (:malformed-defmulti      '("in mu-defmulti %s malformed arglist or body"))

   (:no-methods              '("no mu-methods match dispatch value %s for dispatch %s "))

   (:malformed-defmethod     '("in mu-defmethod %s malformed arglist or body")))

  "Predefined error messages that can be used in `mu-error' by
passing it an attribute as the first argument.")


(defun mu-error (&rest args)
  "Like `error' but raise a custom `mu-error'. Alternatively
take a keyword as the first ARG to use a predefined message."
  (let* ((mu-err (ht-get mu--errors (car args)))
         (msg (if mu-err (list* (string-join mu-err "") (cdr args)) args)))
    (signal 'mu-error (list (apply #'format-message msg)))))


(defcustom mu-lexical-binding 'error
  "multi-methods may not work correctly without
`lexical-binding'. By default check and signal an error if an
attempt is made to use multi-methods in dynamic scope.")


;; TODO (mu-lexical-binding) check is somehow subtly broken when you
;; byte-compile-file that defines multimethods and then load. With lexical-binding
;; set it remains on when you compile, but on load it appears nil. I don't know
;; what's going on. Either byte-compile is subtly broken, or by the time we
;; byte-compile every defun is already a closure and load happens in dynamic
;; environment. Until I figure this out, I am disabling this check. See:
;; https://emacs.stackexchange.com/questions/46812/byte-compile-and-lexical-binding
(setq mu-lexical-binding nil)


(defun mu-lexical-binding ()
  "Signal an error depending on the setting of
`mu-lexical-binding' and `lexical-binding'."
  (when mu-lexical-binding
    (unless lexical-binding
      (mu-error :lexical-binding))))


(defmacro mu-docvar (var docstring)
  (declare (indent 1))
  `(put ',var 'variable-documentation ,docstring))


(defmacro mu-docfun (var docstring)
  (declare (indent 1))
  `(put ',var 'function-documentation ,docstring))


;;* Font-lock & Imenu--------------------------------------------- *;;


;; NOTE ultimately imenu setup sets `imenu-generic-expression'. Since its
;; buffer-local you want `lisp-imenu-generic-expression' set before you ever open
;; any Elisp buffer, so this needs to be called from init.el. Neither imenu nor
;; font-lock depend on multi. So I think this setup ought to be in a separate
;; namespace that doesn't load multi features.


(defvar mu-imenu-expressions nil
  "Imenu generic expressions for mu-def* forms.")


(let* ((blanks '(+ (or space "\n")))
       (symbol '(+ (any "-" "_" word)))
       (types  '("mu-defstruct"  "mu-defprotocol" "mu-extend" "cl-defstruct"))
       (funs   '("mu-defmulti"   "mu-defmethod"   "mu-defun"  "mu-defmacro"
                 "mu-defpattern" "mu-defsetter"))
       (rx      (lambda (defs) (rx-to-string
                           `(seq "(" (group (or ,@defs)) symbol-end ,blanks
                                 (?  "(") (group ,symbol))))))

  ;; font-lock
  (font-lock-add-keywords
   'emacs-lisp-mode
   `((,(funcall rx types)
      (1 font-lock-keyword-face)
      (2 font-lock-type-face nil t))
     (,(funcall rx funs)
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face nil t))))

  ;; imenu
  (setq mu-imenu-expressions
        `((nil         ,(funcall rx funs)                            2)
          ("Types"     ,(funcall rx types)                           2)
          ("Patterns"  ,(funcall rx '("mu-defpattern"))              2)
          ("Protocols" ,(funcall rx '("mu-defprotocol" "mu-extend")) 2)
          ("Variables" ,(funcall rx '("mu-defprotocol"))             2))))


(defun mu-enable-imenu-support ()
  "Add mu-def* expressions to `imenu' by extending
`lisp-imenu-generic-expression'."
  (eval-after-load 'lisp-mode
    (dolist (expr mu-imenu-expressions)
      (add-to-list 'lisp-imenu-generic-expression expr))))


(defun mu-disable-imenu-support ()
  "Remove mu-def* expressions from `imenu'."
  (eval-after-load 'lisp-mode
    (dolist (expr mu-imenu-expressions)
      (setq lisp-imenu-generic-expression
            (remove expr lisp-imenu-generic-expression)))))


;; temp hack so I can have imenu in multi repo
(defun mu-enable-imenu-support-refind-file ()
  (interactive)
  (mu-enable-imenu-support)
  (when-let ((file (buffer-file-name (current-buffer))))
    (kill-buffer-if-not-modified (current-buffer))
    (find-file file)))


;;* Provide ------------------------------------------------------ *;;


(provide 'multi-prelude)
