;; -*- lexical-binding: t; -*-


(require 'cl)


;;* prelude ------------------------------------------------------ *;;


;; Definitions that aren't multi-pattern specific and may as well belong in a
;; separate helper module. Safe to skip on the first read.


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


;;* edebug-specs ------------------------------------------------- *;;


;; Offer a decent baseline debugging experience by annotating macros with
;; edebug-specs. These can, no doubt, be improved upon and they will be, once I
;; figure out a way to inspect how edebug matches a spec. I roughly follow
;; examples in the INFO and the specs I've seen around. Gives me grief but does
;; the trick.


;; NOTE make byte-compiler happy
(eval-when-compile
  (declare-function edebug-match "edebug" (cursor specs))
  (declare-function get-edebug-spec "edebug" (symbol)))


;; NOTE broadly mu-pattern is either a standard `pcase' pattern, a built-in
;; sequential pattern or a custom pattern instroduced with `mu-defpattern'.
(def-edebug-spec mu-pat
  (&or symbolp
       ("quote" symbolp)
       ;; standard pcase patterns
       ("or" &rest mu-pat)
       ("and" &rest mu-pat)
       ("guard" form)
       ("let" mu-pat form)
       ("pred" mu-fun-pat)
       ("app" mu-fun mu-pat)
       ;; built-in sequence patterns
       mu-seq-pat
       ;; custom patterns installed with `mu-defpattern'
       mu-defpattern-pat
       sexp))


(def-edebug-spec mu-fun-pat
  (&or lambda-expr
       (functionp &rest form)
       ;; TODO mu-lambda
       sexp))


(def-edebug-spec mu-seq-pat
  (&or [&or "&rest" "&" "|"]
       ("lst" &rest mu-pat)
       ("vec" &rest mu-pat)
       (vector &rest mu-pat)))


;; NOTE IIUC this is more or less what `pcase' does. We store custom patterns
;; differently and I'd like to namespace them, so we can't blindly copy pcase
;; solution, sigh.
(defun mu-pat--edebug-match (cursor)
  "Edebug matcher for custom mu-patterns installed with
`mu-defpattern'"
  (let (specs)
    (dolist (s (ht-keys (get 'mu--case :mu-patterns)))
      (when-let ((spec (get-edebug-spec s)))
        (push (cons (symbol-name s) spec) specs)))
    (edebug-match cursor (cons '&or specs))))


(def-edebug-spec mu-defpattern-pat
  mu-pat--edebug-match)


(def-edebug-spec mu-defun-arglist
  (&or (vector &rest mu-pat)
       symbolp
       ([&rest arg]
        [&optional ["&optional" arg &rest arg]]
        &optional [[&or "&rest" "&" "|"] arg])))


(def-edebug-spec mu-defun-body
  (&or
   ;; multi-head defun clauses
   [([&or (vector &rest mu-pat) "otherwise"] def-body)
    &rest
    ([&or (vector &rest mu-pat) "otherwise"] def-body)]
   ;; single-head defun body
   def-body))


(def-edebug-spec mu-defun
  (&define name mu-defun-arglist
           [&optional stringp]
           ;; TODO According to INFO interactive and declare are special somehow,
           ;; but it never mentions how exactly. Can they have bodies to eval?
           [&rest [keywordp sexp]]
           mu-defun-body))


;;* mu-error ----------------------------------------------------- *;;


;; Introduce a custom mu-error to differentiate signals specific to the multi
;; feature. Consider raising mu-error whenever it relates to multi-pattern
;; matching or multi-dispatch. `mu-error' function simplifies this by
;; intentionally following the exact same calling convention as `error'. Please,
;; use it.


(define-error 'mu-error "mu-error")


(defconst mu--errors
  (ht
   (:lst-pattern     '("in mu-case lst-pattern doesn't support &rest,"
                       " use l-pattern instead in: %S"))
   (:vec-pattern     '("in mu-case vec-pattern doesn't support &rest,"
                       " use v-pattern instead in: %S"))
   (:pattern         '("in mu-case unrecognized pattern %S"))
   (:ht-pattern      '("in mu-case malformed ht pattern in %S"))
   (:seq-pattern     '("in mu-case seq pattern applied to unrecognized type %s"))
   (:rest-pattern    '("in mu-case malformed &rest pattern %S"))
   (:let-malformed   '("in mu-let malformed binding list in %S"))
   (:defun-malformed '("in mu-defun/macro malformed arglist has no"
                       " &rest argument in %S")))
  "Predefined error messages that can be used in `mu-error' by
passing it an attribute as the first argument.")


(defun mu-error (&rest args)
  "Like `error' but raise a custom `mu-error'. Alternatively
take a keyword as the first ARG to use a predefined message."
  (let* ((mu-err (ht-get mu--errors (car args)))
         (msg (if mu-err (list* (string-join mu-err "") (cdr args)) args)))
    (signal 'mu-error (list (apply #'format-message msg)))))


;;* mu-patterns -------------------------------------------------- *;;


;; Internal machinery to fascilitate rewriting mu-patterns into pcase-patterns.
;; These definitions are the core that makes the rest of the library possible.
;; These should not be used directly lest you are prepared to suffer the
;; implementation being altered right from under you without warning.


(defun mu--pcase-nest (expr clauses)
  "Turn pcase-clauses into a nested tree of `pcase' expressions,
where every clause except the first becomes the 'otherwise body
of the preceding clause:

  ((pat1 body1)
   (pat2 body2))
   =>
  (pcase expr
    (pat1 body1)
    (otherwise
     (pcase expr
       (pat2 body2))))"

  ;; reverse clauses so that 'otherwise comes first if present
  (loop for clause in (nreverse clauses)
        with code = nil
        if (eq 'otherwise (car clause))
        do (setq code clause)
        else
        do (setq code `(otherwise (pcase ,expr
                                    ,clause
                                    ,@(when code (list code)))))
        ;; remove extraneous outermost (otherwise ..)
        finally return `(progn ,@(cdr code))))


(defvar mu-prefer-nested-pcase nil
  "`pcase' expander may on occasion produce pathological
expansions, where a reasonable 4-clause matcher expands into over
160K lines of code. Toggling this parameter where this happens
will force `mu-case' to convert generated pcase-clauses into a
tree of nested pcase-calls before handing it over to `pcase'.
This shrinks the expansion by orders of magnitude but may defeat
some optimizations `pcase' could have undertaken had it known all
the clauses (citation needed).")


;; TODO rather than intermediating with mu--case, should I allow to parameterize
;; mu-case directly. Might be cleaner. I hate function proliferation.
;;
;;   (mu-case expr
;;     :seq  seq
;;     :ns   custom-pat-namespace
;;     :nest t
;;     (pat1 body1)
;;     (pat2 body2))


(defmacro mu--case (seq-pat e &rest clauses)
  "`pcase'-like matching and destructuring. SEQ-PAT parameterizes
[] seq-pattern to be one of: `lv' for strict and `seq' for
permissive sequence matching."
  (declare (indent 2))

  ;; initialize storage for `mu-defpattern' defined patterns
  (unless (get 'mu--case :mu-patterns)
    (define-symbol-prop 'mu-case :mu-patterns (ht)))

  ;; propagate compile-time errors to runtime (is that ok?)
  (condition-case err
      (let ((pcase-clauses
             (mapcar (lambda (clause) (mu-case--clause seq-pat clause)) clauses)))
        (if mu-prefer-nested-pcase
            (mu--pcase-nest e pcase-clauses)
          `(pcase ,e
             ,@pcase-clauses)))
    (mu-error `(mu-error ,(cadr err)))))


(cl-defun mu-case--clause (seq-pat (pat . body))
  `(,(mu--pat-unquoted seq-pat pat) ,@body))


(defun mu--rest? (item)
  "Test if the ITEM is a `&rest'-like separator"
  (memq item '(| & &rest)))


(defun mu--pat-unquoted (seq-pat pat)
  "Translate mu-pat into pcase-pat assuming in unquoted context"
  (pcase pat
    ('otherwise               pat)
    ((pred symbolp)           pat)
    ;; list pattern
    (`(lst . ,_)              (list '\` (mu--pat-backquoted seq-pat pat)))
    ;; vector pattern
    (`(vec . ,_)              (list '\` (mu--pat-backquoted seq-pat pat)))
    ;; [] pattern parameterized by the type of seq-pattern ('lv or 'seq)
    ((pred vectorp)           (mu--pat-unquoted seq-pat `(,seq-pat ,@(seq-into pat 'list))))
    ;; standard pcase patterns
    (`(or . ,pats)            (cons 'or (mapcar (lambda (p) (mu--pat-unquoted seq-pat p)) pats)))
    (`(and . ,pats)           (cons 'and (mapcar (lambda (p) (mu--pat-unquoted seq-pat p)) pats)))
    (`(app ,fun ,pat)         (list 'app fun (mu--pat-unquoted seq-pat pat)))
    (`(let ,pat ,exp)         (list 'let (mu--pat-unquoted seq-pat pat) exp))
    ;; quoted symbol
    (`(quote ,(pred symbolp)) pat)
    ;; mu-defpatterns
    (`(,(and id (pred symbolp)) . ,pats)
     (if-let ((macro (ht-get (get 'mu--case :mu-patterns) id)))
         ;; registered pattern
         (mu--pat-unquoted seq-pat (apply macro pats))
       ;; unknown pattern
       pat))
    ((pred listp)             pat)
    ((pred atom)              pat)
    (otherwise                (mu-error :pattern pat))))


(defun mu--pat-backquoted (seq-pat pat)
  "Translate mu-pat into pcase-pat assuming in quoted context"
  (pcase pat
    ('()                      '())
    ;; empty list
    (`(lst)                   '())
    ;; empty vector
    (`(vec)                   [])
    ;; list pattern
    (`(lst . ,pats)           (if (some #'mu--rest? pats)
                                  ;; TODO this check might be expensive, should we do it?
                                  (mu-error :lst-pattern pats)
                                (mapcar (lambda (p) (mu--pat-backquoted seq-pat p)) pats)))
    ;; vector pattern
    (`(vec . ,pats)           (if (some #'mu--rest? pats)
                                  (mu-error :vec-pattern pats)
                                (seq-into
                                 (mapcar (lambda (p) (mu--pat-backquoted seq-pat p)) pats)
                                 'vector)))
    ;; [] pattern parameterized by the type of seq-pattern ('lv or 'seq)
    ((pred vectorp)           (list '\, (mu--pat-unquoted seq-pat `(,seq-pat ,@(seq-into pat 'list)))))
    ;; quoted symbol
    (`(quote ,(pred symbolp)) (cadr pat))
    ((pred keywordp)          pat)
    ((pred symbolp)           (list '\, pat))
    ;; TODO do I need to check for an empty list here?
    ((pred listp)             (list '\, (mu--pat-unquoted seq-pat pat)))
    ((pred atom)              pat)
    (otherwise                (mu-error :pattern pat))))


;;* mu-defpattern ------------------------------------------------ *;;


;; Let the user create custom patterns of the form (name mu-pat...). Every such
;; pattern is an expander - a function that takes mu-patterns as arguments and
;; must produce a known mu-pattern. All patterns are kept in a table that maps
;; NAME to an expander. NAME is but a key in that table and is global only in so
;; much as the table is directly accessible by the user, which the user shouldn't
;; really do. This means that while NAME doesn't make it into defun/defvar
;; namespaces, clashes are possible. Should someone else define a mu-pattern with
;; the same name, it would overwrite and thus shaddow the earlier pattern.
;; Sometimes useful this should probably be an opt-in rather than the default
;; behavior. To that end we are going to namespace patterns, so that the user can
;; register a fresh namespace (a pattern table) and install any custom patterns
;; there.
;;
;; Define several handy patterns: l-pattern to match lists; v-pattern to match
;; vectors; ht-pattern to match hash-tables and alists; seq-pattern to match lists
;; or vectors non-strictly - taking an open-world collection view; lv-pattern to
;; match lists or vectors strictly - all items of a collection must match.


(defmacro mu-defpattern (name arglist &optional docstring &rest body)
  "Define an expander for a custom mu-pattern of the form (NAME
&rest patterns) where actual patterns will be bound in the
expander according to the ARGLIST. Expander must produce a valid
mu-pattern. NAME is only required to identify the pattern, the
macro does not bind it. Optional DOCSTRING maybe supplied to
improve readability of your code. BODY may start with a :debug
EDEBUG-SPEC attribute pair.

\(fn NAME ARGLIST &optional DOCSTRING &rest BODY)"
  (declare (doc-string 3) (indent 2) (debug defun))

  ;; install edebug spec if present and remove it from the body
  (when (eq :debug (car body))
    (def-edebug-spec name (cadr body))
    (setq body (cddr body)))

  ;; just for consistency, when present, cons the docstring onto the body
  (when docstring
    (unless (stringp docstring)
      (setq body (cons docstring body))))

  (let ((mu-patterns `(or (get 'mu--case :mu-patterns)
                          (put 'mu--case :mu-patterns (ht))))
        (pattern-macro `(lambda ,arglist ,@body)))
    ;; add pattern to the mu-patterns table
    `(setf (ht-get ,mu-patterns ',name) ,pattern-macro)))


(defun mu--ht-pattern (patterns)
  (mu-case patterns
    ((l) '())

    ;; (ht :a :b)
    ((l (and kw (pred keywordp) (app sym id)) &rest pats)
     `(((app (lambda (ht) (or (ht-get ht ,kw) (ht-get ht ',id))) ,id)
        (app (lambda (ht) (or (alist-get ,kw ht) (alist-get ',id ht))) ,id))
       ,@(mu--ht-pattern pats)))

    ;; (ht 'a 'b)
    ((l (l 'quote (and id (pred symbolp) (app (lambda (id) (sym ":" id)) kw))) &rest pats)
     `(((app (lambda (ht) (or (ht-get ht ',id) (ht-get ht ,kw))) ,id)
        (app (lambda (ht) (or (alist-get ',id ht) (alist-get ,kw ht))) ,id))
       ,@(mu--ht-pattern pats)))

    ;; (ht a b)
    ((l (and id (pred symbolp) (app (lambda (id) (sym ":" id)) kw)) &rest pats)
     `(((app (lambda (ht) (or (ht-get ht ,kw) (ht-get ht ',id))) ,id)
        (app (lambda (ht) (or (alist-get ,kw ht) (alist-get ',id ht))) ,id))
       ,@(mu--ht-pattern pats)))

    ;; (ht (:a A) (:b B))
    ((l (l key id) &rest pats)
     `(((app (lambda (ht) (ht-get ht ,key)) ,id)
        (app (lambda (ht) (alist-get ,key ht)) ,id))
       ,@(mu--ht-pattern pats)))

    (otherwise
     (mu-error :ht-pattern patterns))))


(mu-defpattern ht (&rest patterns)
  "mu-pattern to match hash-tables and alists with values bound
according to the key PATTERNS that maybe one of:

  :a or a  - try keys in order :a, 'a and bind to a,
  'a       - try keys in order 'a, :a and bind to a,
  (key id) - try key and bind to id

Example:

  (mu-case (ht (:a 1) ('b 2) (:c 3) ('d 4))
    ((ht :a b 'c ('d D)) (list a b c D)))"

  (let* ((patterns (mu--ht-pattern patterns))
         (ht-pats (mapcar #'car patterns))
         (alist-pats (mapcar #'cadr patterns)))
    `(or (and (pred ht-p) ,@ht-pats)
         (and (pred listp) ,@alist-pats))))


(defcustom mu-seq-pattern-force-list nil
  "Force seq-pattern to always cast its &rest submatch to a list.
Without this setting the &rest supattern match will preserve the
type of the sequence being matched."
  :options '(list nil))


(defun mu--seq-split-and-pad (seq pat-len)
  (let* ((type (cond
                ((listp seq) 'list)
                ((vectorp seq) 'vector)
                ;; NOTE If ever signaled, then there must be a bug. Matching
                ;; non-seq with a seq-pattern should simply fail, not raise.
                (:else (mu-error :seq-pattern (type-of seq)))))
         (subseq (seq-take seq pat-len))
         (took (length subseq))
         (less-by (- pat-len took)))
    ;; return (list padded-head-seq rest-seq)
    (list (if (< took pat-len)
              ;; pad head with nils
              (seq-concatenate type subseq (make-list less-by nil))
            subseq)
          ;; rest is empty if seq was shorter than patterns
          (seq-subseq seq took))))

;; NOTE basic idea for seq-pattern: instead of trying to match the sequence, build
;; a new sequence by taking as many elements from the original as there are
;; patterns. If the sequence has fewer elements than the patterns pad with nils.
;; Now match patterns against that newly built sequence.
(mu-defpattern seq (&rest patterns)
  "mu-pattern to match lists and vectors taking an open-world
collection view: match as many PATTERNS as available. Fewer
patterns than items in a sequence will simply match the head of
the sequence; more patterns will match available items, then
match any excessive patterns against that many nils. Supports
&rest subpattern to match remaining items."
  :debug (&rest mu-pat)
  (if patterns
      (let* ((split (mu--split-when #'mu--rest? patterns))
             (head (car split))
             (rest (cadr split))
             (rest? (when rest t))
             (pat-len (length (if rest? head patterns)))
             (seqp `(pred (lambda (v) (or (listp v) (vectorp v))))))
        (when (> (length rest) 1)
          (mu-error :rest-pattern rest))
        (if rest?
            ;; match head and rest
            `(and ,seqp
                  (app (lambda (seq) (mu--seq-split-and-pad seq ,pat-len))
                       (lst (or (lst ,@head) (vec ,@head)) ,@rest)))
          ;; match head only
          `(and ,seqp
                (app (lambda (seq) (car (mu--seq-split-and-pad seq ,pat-len)))
                     (or (lst ,@head) (vec ,@head))))))
    ;; empty seq-pattern
    `(or (lst) (vec))))


(defun mu--seq-split (seq pat-len)
  (let* ((subseq (seq-take seq pat-len))
         (took (length subseq)))
    (list subseq
          ;; rest is empty if seq was shorter than patterns
          (seq-subseq seq took))))


(mu-defpattern lv (&rest patterns)
  "mu-pattern to match lists and vectors alike. Unlike
seq-pattern it is strict and behaves like l-pattern for lists or
v-pattern for vectors: must match the entire sequence to
succeed."
  :debug (&rest mu-pat)
  (if patterns
      (let* ((split (mu--split-when #'mu--rest? patterns))
             (head (car split))
             (rest (cadr split))
             (rest? (when rest t))
             (pat-len (length (if rest? head patterns)))
             (seqp `(pred (lambda (v) (or (listp v) (vectorp v))))))
        (when (> (length rest) 1)
          (mu-error :rest-pattern rest))
        (if rest?
            ;; match head and rest
            `(and ,seqp
                  (app (lambda (seq) (mu--seq-split seq ,pat-len))
                       (lst (or (lst ,@head) (vec ,@head)) ,@rest)))
          ;; match head only
          `(or (lst ,@head) (vec ,@head))))
    ;; empty seq-pattern
    `(or (lst) (vec))))


;; NOTE basic idea for list and vector patterns: keep splitting PATTERNS at &rest
;; and recursing into chunks. Chunk with no &rest should produce a built-in
;; lst-pattern or vec-pattern to break recursion.


(mu-defpattern l (&rest patterns)
  "mu-pattern to match lists. Unlike built-in lst-pattern allow a
&rest subpattern to match remaining items."
  :debug (&rest mu-pat)
  (if patterns
      (let* ((split (mu--split-when #'mu--rest? patterns))
             (head (car split))
             (rest (cadr split))
             (rest? (when rest t))
             (pat-len (length (if rest? head patterns))))
        (when (> (length rest) 1)
          (mu-error :rest-pattern rest))
        (if rest?
            ;; match head and rest
            `(and (pred listp)
                  (app (lambda (l) (mu--seq-split l ,pat-len))
                       (lst (lst ,@head) ,@rest)))
          ;; match head only
          `(and (pred listp)
                (lst ,@head))))
    `(lst)))


(mu-defpattern v (&rest patterns)
  "mu-pattern to match vectors. Unlike built-in vec-pattern allow
a &rest subpattern to match remaining items."
  :debug (&rest mu-pat)
  (if patterns
      (let* ((split (mu--split-when #'mu--rest? patterns))
             (head (car split))
             (rest (cadr split))
             (rest? (when rest t))
             (pat-len (length (if rest? head patterns))))
        (when (> (length rest) 1)
          (mu-error :rest-pattern rest))
        (if rest?
            `(and (pred vectorp)
                  (app (lambda (v) (mu--seq-split v ,pat-len))
                       (lst (vec ,@head) ,@rest)))
          `(and (pred vectorp)
                (vec ,@head))))
    `(vec)))


;;* mu-case ------------------------------------------------------ *;;


;; Define `mu-case' - a counterpart to `pcase' choice expression that uses
;; mu-patterns. By default any []-pattern is strict and must match the entire
;; input sequence. This behavior can be controlled by setting an optional
;; attribute :seq to a desired seq-pattern (defaults to `lv'). Optional attribute
;; :nest controls clause rewriting (see `mu-prefer-nested-pcase'), attribute :ns
;; allows to specify a user-defined pattern namespace to lookup custom patterns
;; defined with `mu-defpattern'. Many features in multi.el simply re-write into
;; some form of `mu-case', which makes it foundational to the library.


(defmacro mu-case (e &rest clauses)
  "`pcase'-like matching and destructuring with less noise.
Sequence pattern [] is strict: must match the entire sequence to
succeed."
  (declare (indent 1)
           (debug (form &rest (mu-pat body))))
  ;; overload []-pattern to be strict by using lv-pattern
  `(mu--case lv ,e ,@clauses))


;;* mu-let ------------------------------------------------------- *;;


;; Define `mu-let', `mu-when-let' and `mu-if-let'. Any []-pattern is non-strict
;; and will likely bind as long as the input is a sequence. This makes binding
;; behavior more permissive and hopefully better aligned with what a user may
;; reasonably expect. In so doing it mimics the Clojure's let destructuring but
;; with full expressiveness of mu-patterns.


(defcustom mu-let-parens 'yes
  "Control if `mu-let' shoud have a set of parens around each
binding clause like normal `let': 'yes (default), 'no, 'square -
no extra parens, but the entire set of bindings must be inside []."
  :options '(yes no square))


(defun mu--let (bindings body)
  (let* ((pair (car bindings))
         (pat (car pair))
         (val (cadr pair)))
    (cond
     (pair (unless (= 2 (length pair))
             (mu-error :let-malformed pair))
           `(mu--case seq ,val
              (,pat ,(mu--let (cdr bindings) body))
              (otherwise ,(mu--let (cdr bindings) body))))
     (:else
      `(progn ,@body)))))


(defun mu--when-let (bindings body)
  (let* ((pair (car bindings))
         (pat (car pair))
         (val (cadr pair)))
    (cond
     (pair (unless (= 2 (length pair))
             (mu-error :let-malformed pair))
           `(mu--case seq ,val
              (,pat ,(mu--when-let (cdr bindings) body))))
     (:else
      `(progn ,@body)))))


(defun mu--if-let (bindings then-body else-body)
  (let* ((pair (car bindings))
         (pat (car pair))
         (val (cadr pair)))
    (cond
     (pair (unless (= 2 (length pair))
             (mu-error :let-malformed pair))
           `(mu--case seq ,val
              (,pat ,(mu--if-let (cdr bindings) then-body else-body))
              (otherwise (progn ,@else-body))))
     (:else
      `(progn ,then-body)))))


(defun mu--let-bindings (bindings)
  (case mu-let-parens
    ('yes    bindings)
    ('no     (seq-partition bindings 2))
    ('square (seq-partition (seq-into bindings 'list) 2))))


(defmacro mu-let (bindings &rest body)
  "Like `let*' but allow mu-patterns in place of bindings. Any
[]-pattern is permissive and assumes open-world collections."
  (declare (indent 1)
           (debug ((&rest (sexp form)) body)))
  (condition-case err
      (mu--let (mu--let-bindings bindings) body)
    (mu-error `(mu-error ,(cadr err)))))


(defmacro mu-when-let (bindings &rest body)
  "Like `when-let*' but allow mu-patterns in place of bindings.
Any []-pattern is permissive and assumes open-world collections."
  (declare (indent 1)
           (debug ((&rest (sexp form)) body)))
  (condition-case err
      (mu--when-let (mu--let-bindings bindings) body)
    (mu-error `(mu-error ,(cadr err)))))


(defmacro mu-if-let (bindings then-body &rest else-body)
  "Like `if-let*' but allow mu-patterns in place of bindings. Any
[]-pattern is permissive and assumes open-world collections."
  (declare (indent 2)
           (debug ((&rest (sexp form)) form body)))
  (condition-case err
      (mu--if-let (mu--let-bindings bindings) then-body else-body)
    (mu-error `(mu-error ,(cadr err)))))


;;* mu-defun ----------------------------------------------------- *;;


;; Define `mu-defun' and `mu-defmacro' that allow pattern-matching and
;; destructuring their arguments. Both have single-head and multi-head versions,
;; where the former simply matches arguments against a []-pattern, while the
;; latter allows multiple clauses each with its own []-pattern and body.
;; Multi-head defun is not unlike and has been inspired by Clojure's multi-head
;; defn, but should be more expressive thanks to the might of `pcase' and
;; mu-patterns. Since mu-defun is about dispatching on arrity first and
;; destructuring second its []-pattern necessitates subtle semantics. The
;; outermost []-pattern is strict, that is it either matches the entire sequence
;; or fails and the next clause is tried. In multi-head case all []-patterns even
;; internal ones are strict, so that you can dispatch on the internal structure
;; even if multiple clauses have the same arrity; in a single-head only the
;; external []-pattern is strict, since you probably want to detect arrity errors,
;; but the internal []-patterns are permissive to fascilitate destructuring. None
;; of this is terribly important as long as it matches user expectation which I
;; hope it does.


;; NOTE Some observations re Clojure's defn and this implementation. Clojure's
;; defn is nice but somewhat limiting. IIUC multi-head defn is an fixed arrity
;; function that may allow an &rest pattern but only where such pattern is of
;; bigger arrity than every other fixed pattern. So the following is not allowed
;; in Clojure:
;;
;;   (mu-defun foo (&rest args)
;;     ([a b c] ...)
;;     ([a b &rest c] ...))
;;
;; nor can you dispatch on the same arrity
;;
;;   (mu-defun foo (&rest args)
;;     ([a [b c] d] (list a b c d))
;;     ([a [b]   c] (list a b c)))
;;
;; Why would you ever want that? Well, if you actually perform pattern-matching
;; like we do with mu-patterns, then it is quite desirable to be able to dispatch
;; not only on the arrity but on the internal structure as the above example
;; shows. Far as I can surmount Clojure dispatch and destructure aren't
;; pattern-based but rather much simpler. You can probably gain most or all of
;; that by leveraging Clojure Spec and I bet someone will release just such
;; library before soon. As it stands I see no reason for us to follow in Clojure
;; footsteps and surrender expressiveness afforded by patterns.


(defun mu--defun-meta (body &optional map)
  "Return a hash-table of attribute value pairs from `mu-defun'
preamble"
  (default map :to (ht))
  (cond
   ((keywordp (car body)) (ht-set map (pop body) (pop body)) (mu--defun-meta body map))
   (:else                 (ht-set map :body body) map)))


(defun mu--defun-sig (split-args body &optional doc sig sigs)
  "Create a docstring from DOC adding signature SIG if supplied
and extra signatures either supplied or generated from the
arglist and mu-patterns in BODY clauses."
  (default doc :to "")
  (let* ((head (car split-args))
         (tail (cadr split-args))
         (sig (if sig (concat "\n\n\(fn " (substring (format "%S" sig) 1)) ""))
         (gen-sig (lambda (s)
                    (unless (equal 'otherwise (car s))
                      (format "`%S'"
                              (append
                               head '(&rest)
                               (list (car s)))))))
         (sigs (cond
                ;; generate extra signatures
                ((equal sigs t) (mapcar gen-sig body))
                ;; use the ones supplied
                (sigs (mapcar (lambda (s) (format "`%S'" s)) sigs))))
         (doc (string-join
               ;; prepend supplied docstring
               `(,doc
                 ;; maybe add extra signatures
                 ,@(when sigs '("\nMay also be called according to signatures:\n"))
                 ,@sigs)
               "\n  ")))
    (string-trim
     ;; add special last line signature if supplied
     (concat doc sig))))


(defun mu--defun-clause? (clause)
  "Is CLAUSE a mu-clause ([pat...] body...)?"
  (and (listp clause)
       (not (null clause))
       (or (vectorp (car clause))
           (equal (car clause) 'otherwise))))


(defun mu--defun-clauses? (body)
  "Is BODY a list of mu-clauses?"
  (and (listp body)
       (not (null body))
       (every #'mu--defun-clause? body)))


(defun mu--defun (fun-type name arglist docstring attrs body)
  "Do all the heavy lifting to process the code from `mu-defun'
or `mu-defmacro': extract metadata, normalize arglist and body
and recursively call itself to generate actual defun or defmacro
respectively. See `mu-defun' for what can appear in ARGLIST and
ATTRS."

  ;; extract docstring from body
  (unless docstring
    (setq docstring "")
    (when (stringp (car body))
      (setq docstring (car body)
            body (cdr body))))

  ;; extract attributes from body
  (unless attrs
    (let ((meta (mu--defun-meta body)))
      (setq body (ht-get meta :body)
            attrs meta)))

  ;; BODY should now be either a list of mu-clauses (multi-head defun), or
  ;; anything at all (single-head defun).
  ;;
  ;; Dispatch on the ARGLIST. Each clause matches some permitted calling
  ;; convention and will either normalize arguments, body and recurse or generate
  ;; the final function definition.

  (mu-case arglist

    ;; ARGLIST: [patterns]
    ((pred vectorp)
     (let* ((args (gensym "args"))
            (clauses `((,arglist ,@body))))
       ;; arglist now (&rest random-id), single clause ([patterns] body), recurse
       (mu--defun fun-type name `(&rest ,args) docstring attrs clauses)))

    ;; ARGLIST: '() or nil
    ('()
     `(mu-error "in mu-defun malformed arglist"))

    ;; ARGLIST: id or _
    ((and (pred symbolp) id)
     ;; if asked to not bind all args, do it by generating some random id
     (when (equal id '_)
       (setq id (gensym "id")))
     ;; arglist now (&rest ID), body must already be mu-clauses, recurse
     (mu--defun fun-type name `(&rest ,id) docstring attrs body))

    ;; ARGLIST: (a b &rest args)
    ;; TODO should I allow _ and replace them with random symbols?
    ((and (pred (lambda (arg) (some #'mu--rest? arg))) arglist)
     (if (mu--defun-clauses? body)
         ;; having verified that body consists of mu-clauses, pull any metadata
         ;; supplied as :attributes before the actual body
         (mu-let (((ht sig ret sigs debug test (:declare dspec) (:interactive ispec))
                   attrs)
                  (split-args (mu--split-when #'mu--rest? arglist))
                  ;; split the arglist into positional head-args and the catch all
                  ;; argument after &rest
                  ([head-args [rest-arg]] split-args)
                  ;; ensure the &rest delimiter is used
                  (arglist (concatenate 'list head-args `(&rest ,rest-arg)))
                  ;; are we dealing with a (mu-defun foo [patterns] body) call?
                  (single-clause? (= 1 (length body))))

           ;; TODO :ret :test :debug

           `(,fun-type
             ,name ,arglist
             ;; enrich the docstring with signatures if supplied
             ,(mu--defun-sig split-args body docstring sig sigs)
             ,@(when dspec `((declare ,@dspec)))
             ,@(when ispec `((interactive ,@(if (equal 't ispec) '() (list ispec)))))

             ;; TODO decide whether I want to treat single-clause specially
             ,(if single-clause?

                  ;; re-write outermost []-pattern to be strict, so we catch
                  ;; arrity bugs, but treat any internal []-pattern as permissive
                  ;; `seq', single clause requires no dispatch, so we want to be
                  ;; generous with destructuring not strict with dispatch like in
                  ;; multi-head case below
                  `(mu--case seq ,rest-arg
                     ((lv ,@(seq-into (caar body) 'list)) ,@(cdr (car body)))
                     ;; default otherwise clause
                     (otherwise
                      (mu-error "no matching clause found for mu-defun call %s" ',name)))

                ;; all []-patterns are strict
                `(mu-case ,rest-arg
                   ,@body
                   ;; default otherwise clause if not supplied by the user
                   ,@(unless (some (lambda (clause) (equal (car clause) 'otherwise)) body)
                       (list
                        `(otherwise
                          (mu-error "no matching clause found for mu-defun call %s" ',name))))))))
       ;; expected body of mu-clauses
       `(mu-error "in mu-defun malformed body: expected mu-clauses, got %S" ',body)))

    ;; ARGLIST: unrecognized call-pattern
    (otherwise
     `(mu-error "in mu-defun malformed arglist %S" ',arglist))))


(defun mu--set-defun-docstring (fun-type)
  "Set docstring for `mu-defun' or `mu-defmacro'"
  (put (sym "mu-" fun-type) 'function-documentation
       (format
        "Like `%s' but with multiple clauses. Each clause
specifies a `mu-case' pattern to match against the &rest part of
the ARGLIST followed by the body to run if the match succeeds.
Clauses are tried in order as if one had multiple definitions of
the same function NAME. METADATA can be supplied as :attribute -
expression pairs before the BODY:

  (mu-%s foo (arg &rest args)
    \"docstring\"
    :sig         signature
    :sigs        extra-signatures
    :declare     dspec
    :interactive ispec
    ([mu-case-args-pat1] body1)
    ([mu-case-args-pat2] body2)
      ... ...)

In addition to any variable bound by the corresponding pattern
every clause has the entire ARGLIST in scope.

METADATA is optional and may include the following attributes:

  :doc dostring - a docstring to attach to the NAME function,

  :sig signature - an implicitly quoted arglist that showcases
                   the most likely use of the function, will be
                   stringified and added to the docstring,

  :sigs signatures - extra calling conventions to add to the
                     docstring: absent or `nil' - no extra
                     signatures; `t' - combine head of the
                     arglist and patterns in the clauses to
                     generate signatures; explicit list of
                     signatures.

  :declare dspec - a list of `declare' SPECS,

  :interactive ispec - t or `interactive' ARG-DESCRIPTOR,

\(fn NAME ARGLIST METADATA &rest BODY)"
        fun-type fun-type))
  nil)


(defmacro mu-defun (name arglist &rest body)
  (declare (indent 2))
  (mu--defun 'defun name arglist nil nil body))


(defmacro mu-defmacro (name arglist &rest body)
  (declare (indent 2)
           (debug mu-defun))
  (mu--defun 'defmacro name arglist nil nil body))


;; add docstring to `mu-defun'
(mu--set-defun-docstring 'defun)


;; add docstring to `mu-defmacro'
(mu--set-defun-docstring 'defmacro)


;; TODO Idea for extra attributes :test, :ret, :debug
(comment
 (mu-defun foo-fun [a b | args]
   "This is a foo function that performs foo and returns something
cool."
   ;; initial a b args are in scope for the ret check
   :ret (lambda (v) (and (pred #'some-struct?) etc))
   :test ([1 2] => (struct2 1 2)
          [1 2 3] => (lambda (v) do some checks)
          [1 2 0] => #'predicate)
   ;; enables ret check and test runs
   :debug t
   :interactive t
   :declare ((indent 2) (debug t))
   (some body here))
 ;; => when :debug t
 `(progn

    (defun foo-fun-temp (&rest arglist)
      (mu-case arglist
        ([a b | argls] body)))

    (defun foo-fun (&rest arglist)
      (let ((ret? (lambda (v) (and (pred #'some-struct?) etc)))
            (ret (apply #'foo-fun-temp arglist)))
        (cond
         ((functionp ret?) (funcall ret? ret))
         (:mu-pattern (mu-case ret
                        (ret? t)
                        (otherwise (mu-error "in foo-fun return value %S failed the check %S" ret 'ret?)))))))

    ;; also run tests
    (ert-deftest foo-fun-test ()
      "foo-fun must pass predefined tests"

      (should (mu-case (foo-fun 1 2)
                (some-pat t)
                (otherwise (mu-error "foo-fun failed test ... "))))

      (should (funcall #'predicate (foo-fun 1 2 3))))

    (ert 'foo-fun-test))
 ;; comment
 )


;;* mu-setters --------------------------------------------------- *;;


;; We extend the power of multi-dispatch to gv-setters by providing
;; `mu-defsetter'. Conceptually it is like `gv-define-setter' but with `mu-defun'
;; like matching and multiple clauses. Native `gv-define-setter' will work just
;; fine for anything you define with `mu-defun' or `mu-defmacro', this feature
;; might just make your setters cleaner and easier to write. Despite the name
;; neither gv-define-setter nor mu-defsetter really "define" anything, rather they
;; register an expander that'll be called by the Emacs gv machinery whenever a
;; generalized variable is being set. As such setters maybe tricky to instrument
;; for debugging. To ease the process we also provide `mu-debug-setter' lets you
;; run some code with a given setter instrumented for Edebug.


;;** - (v1) mu-defun-setter -------------------------------------- *;;


(defmacro mu-defun-setter (call-pattern val &rest clauses)
  (declare (indent 2))
  ;; TODO check that I cover all calling conventions
  (let* ((id         (car call-pattern))
         (arglist    (cdr call-pattern))
         (split-args (mu--split-when #'mu--rest? arglist))
         (head-args  (car split-args))
         (rest-arg   (car (cadr split-args)))
         (arglist    (concatenate 'list head-args `(&rest ,rest-arg)))
         (setter-id (sym id (gensym "--mu-setter")))
         (setter-defun `(mu-defun ,setter-id (,val ,@arglist)
                          ,@clauses
                          ,@(unless (some (lambda (clause) (equal (car clause) 'otherwise)) clauses)
                              (list
                               `(otherwise
                                 (mu-error "no setter matches a call to %s" ',id)))))))
    `(progn
       ;; function definition
       ,setter-defun
       ;; store function symbol (for `mu-debug-setter')
       (function-put ',id 'mu-setter #',setter-id)
       ;; store function source as a string (for `mu-debug-setter')
       (function-put ',id 'mu-setter-src ,(pp-to-string setter-defun))
       ;; store the setter function itself
       (function-put ',id 'gv-expander
                     (lambda (do &rest args)
                       (gv--defsetter ',id
                                      #',setter-id
                                      do args))))))


(defmacro mu-defun-simple-setter (call-pattern val &rest body)
  (declare (indent 2))
  (mu-case call-pattern
    ([(and (pred symbolp) id) | pattern]
     `(mu-defun-setter (,id &rest ,(gensym "rest-arg")) ,val
        ([,@pattern] ,@body)))
    (otherwise
     `(mu-error "in mu-defun-simple-setter malformed getter pattern %S" ',call-pattern))))


(defalias 'mu-defmacro-setter 'mu-defun-setter)
(defalias 'mu-defmacro-simple-setter 'mu-defun-simple-setter)

(example

 ;; TODO observe that the single-head is roughly 4-5x faster, than the multi-head
 ;; below. Re-arranging branches can also wildly effect performance. Ugh

 ;; define a getter
 (mu-defun foo [table [level-1-key level-2-key]]
   (ht-get* table :cache level-1-key level-2-key))

 ;; first try with just the simple setter, which should work for the first case of
 ;; setting :foo below, but should fail to set :updated-foo
 (mu-defun-simple-setter (foo table [level-1-key level-2-key]) val
   `(setf (ht-get* ,table :cache ,level-1-key ,level-2-key) ,val))

 ;; now this case should work, but passing a '(:a :b) as keys won't
 (mu-test-time
  (let ((table (ht)))
    (setf (foo table [:a :b]) :foo)
    (foo table [:a :b])))

 ;; now we fix it for both cases by switching to a multi-head setter
 (mu-defun-setter (foo | args) val
   ([table ['quote [level-1-key level-2-key]]]
    `(setf (ht-get* ,table :cache ,level-1-key ,level-2-key) ,val))
   ([table [level-1-key level-2-key]]
    `(setf (ht-get* ,table :cache ,level-1-key ,level-2-key) ,val)))

 ;; and now both setf's should work
 (mu-debug-setter foo
   (mu-test-time
    (let ((table (ht)))

      (list (progn
              (setf (foo table [:a :b]) :foo)
              (foo table [:a :b]))

            (progn
              (setf (foo table '(:a :b)) :updated-foo)
              (foo table '(:a :b))))))))


;;** - (v2) mu-defsetter ----------------------------------------- *;;


(defun mu--split-at-rest (arglist)
  (when (or (listp arglist) (vectorp arglist))
    (mu--split-when #'mu--rest? arglist)))


(defun mu--otherwise-clause? (clause)
  (and (listp clause)
       (eq 'otherwise (car clause))))


(defun mu--defsetter (val-id id head-args rest-arg clauses)
  (cond
   ((mu--defun-clauses? clauses)
    (let* ((arglist (concatenate 'list head-args `(&rest ,rest-arg)))
           (setter-id (sym id (gensym "--mu-setter")))
           (setter-defun `(mu-defun ,setter-id (,val-id ,@arglist)
                            ,@clauses
                            ,@(unless (some #'mu--otherwise-clause? clauses)
                                (list
                                 `(otherwise
                                   (mu-error "no setter matches a call to %s" ',id)))))))
      `(progn
         ;; function definition
         ,setter-defun

         ;; TODO byte-compiling generated defuns gives a massive speedup in how
         ;; the setter executes, so we may really want to do that especially since
         ;; the setter function is actually opaque to the user
         ;; (byte-compile #',setter-id)

         ;; store function symbol (for `mu-debug-setter')
         (function-put ',id 'mu-setter #',setter-id)
         ;; store function source as a string (for `mu-debug-setter')
         (function-put ',id 'mu-setter-src ,(pp-to-string setter-defun))
         ;; store the setter function itself
         (function-put ',id 'gv-expander
                       (lambda (do &rest args)
                         (gv--defsetter ',id
                                        #',setter-id
                                        do args))))))
   (:malformed-body
    `(mu-error "in mu-defsetter malformed body: expected mu-clauses, got %S" ',clauses))))


;; NOTE `pcase' expander generates 160K loc from just 4-clauses below, so the byte
;; compiler eventually errors out with bytecode overflow. We avoid this by
;; toggling `mu-prefer-nested-pcase' for compilation only, which shrinks expansion
;; to just ~1500 loc. This doesn't effect loading.
(eval-when-compile-let ((mu-prefer-nested-pcase t))
  (mu-defmacro mu-defsetter (id | args)
    :declare ((indent 2))

    ;; ARGS: ([pattern] body)
    ([(v (and (pred symbolp) val-id) | pattern) | body]
     (mu--defsetter val-id id '() (gensym "rest-arg") `(([,@pattern] ,@body))))

    ;; ARGS: mu-lambda or a function
    ([(and (or (l 'mu | _) (pred functionp)) fun)]
     `(progn
        (function-put ',id 'mu-setter ,fun)
        (function-put ',id 'gv-expander
                      (lambda (do &rest args)
                        (gv--defsetter ',id
                                       ,fun
                                       do args)))))

    ;; ARGS: (arglist mu-clauses)
    ([[val-id | (app mu--split-at-rest [head-args [rest-arg]])] |
      (and (pred mu--defun-clauses?) clauses)]
     (mu--defsetter val-id id head-args rest-arg clauses))

    (otherwise
     `(mu-error "in mu-defsetter malformed arglist or body"))))


(example

 ;; simple single-head setter
 (mu-defsetter foo [val table [level-1-key level-2-key]]
   `(setf (ht-get* ,table :cache ,level-1-key ,level-2-key) ,val))

 ;; now only this case should work
 (mu-test-time
   (let ((table (ht)))
     (setf (foo table [:a :b]) :foo)
     (foo table [:a :b])))

 ;; multi-head setter
 (mu-defsetter foo (val | args)
   ([table ['quote [level-1-key level-2-key]]]
    `(setf (ht-get* ,table :cache ,level-1-key ,level-2-key) ,val))
   ([table [level-1-key level-2-key]]
    `(setf (ht-get* ,table :cache ,level-1-key ,level-2-key) ,val)))

 ;; multi-headed setter as mu-lambda
 (mu-defsetter foo
     (mu (val | args)
       ([table ['quote [level-1-key level-2-key]]]
        `(setf (ht-get* ,table :cache ,level-1-key ,level-2-key) ,val))
       ([table [level-1-key level-2-key]]
        `(setf (ht-get* ,table :cache ,level-1-key ,level-2-key) ,val))))

 (mu-debug-setter foo
   (mu-test-time
     (let ((table (ht)))

       (list (progn
               (setf (foo table [:a :b]) :foo)
               (foo table [:a :b]))

             (progn
               (setf (foo table '(:a :b)) :updated-foo)
               (foo table '(:a :b)))))))
 ;; example
 )


;;** - mu-debug-setter ------------------------------------------- *;;


;; NOTE While better than nothing I consider it a HACK to debug setters. Interplay
;; with Edebug is intricate and this can have unexpected behavior if the user does
;; something out of the ordinary.


(defun mu-debug-quit ()
  (interactive)
  (edebug-stop)
  ;; TODO hacky way to remove instrumentation, is there a better way? A
  ;; counterpart to `edebug-instrument-function'?
  (eval
   ;; eval to remove Edebug instrumentatino
   (car
    (read-from-string
     ;; get its source-code string
     (function-get
      ;; get instrumented symbol name
      (get 'mu-debug-setter 'mu-setter)
      'mu-setter-src)))
   'lexical)
  (kill-buffer "*mu-debug*"))


(define-minor-mode mu-debug-mode "Minor mode" nil " μ-debug" (make-sparse-keymap))
(define-key mu-debug-mode-map (kbd "q") #'mu-debug-quit)

(defmacro mu-debug-setter (name &rest body)
  (declare (indent 1))
  (let ((buffer (gensym "buffer"))
        (src (function-get name 'mu-setter-src)))
    (if (symbolp (function-get name 'mu-setter))
        `(progn
           ;; store instrumented function name, so we can uninstrument it on quit
           (put 'mu-debug-setter 'mu-setter ,name)
           (let ((,buffer (get-buffer-create "*mu-debug*")))
             (with-current-buffer ,buffer
               (erase-buffer)
               (unless (equal major-mode 'emacs-lisp-mode)
                 (emacs-lisp-mode))
               (mu-debug-mode 1)
               (insert ,src)
               (eval-buffer))
             (edebug-instrument-function (function-get ',name 'mu-setter))
             (pop-to-buffer ,buffer))
           ,@body)
      `(mu-error "no source available for the setter %S" ,name))))


;;* mu-lambda ---------------------------------------------------- *;;


;; Provide single-head and multi-head anonymous functions. These translate into
;; simple lambdas, so should be fine to use in most places a lambda would work.
;; However, it isn't unusual for a macro to introspect that something is a lambda
;; e.g. with a `functionp' or by checking the car of a list etc. I know of no way
;; to extend `functionp', so mu-lambdas won't be recognised as functions until
;; evaluated. Exercise caution when passing mu-lambdas as arguments to macros.


(defun mu--single-head-lambda (patterns body)
  (let ((args (gensym "args")))
    `(lambda (&rest ,args)
       (mu--case seq ,args
         ((lv ,@patterns) ,@body)
         (otherwise (mu-error "in mu-lambda call: no matching clause found"))))))


(defun mu--multi-head-lambda (arglist rest-arg body)
  `(lambda ,arglist
     (mu-case ,rest-arg
       ,@body
       ,@(unless (some (lambda (clause) (equal (car clause) 'otherwise)) body)
           (list
            `(otherwise (mu-error "in mu-lambda call: no matching clause found")))))))


(mu-defmacro mu (&rest args)
  "Create a lambda-like anonymous function but allow `mu-defun'
style single-head destructuring or multi-head dispatching and
destructuring:

  (mu [patterns] body)
  (mu arglist ([patterns] body) mu-clause...)

\(fn [patterns] body)"

  :declare ((indent 1)
            (debug (&define mu-defun-arglist mu-defun-body)))

  ;; Dispatch by pattern-matching ARGS:

  ;; ARGS: (mu [patterns] body)
  ([(v &rest patterns) | body]
   ;; wrap in a lambda that destructures arguments according to [patterns]
   (mu--single-head-lambda (seq-into patterns 'list) body))

  ;; ARGS: (mu '() body) aka (mu nil body)
  (['() | _]
   `(mu-error "in mu-lambda malformed arglist"))

  ;; ARGS: (mu id clauses) or (mu _ clauses)
  ([(and (pred symbolp) id) | (and (pred mu--defun-clauses?) clauses)]
   ;; if asked to not bind all args, do it by generating some random id
   (when (equal id '_)
     (setq id (gensym "id")))
   ;; wrap in a lambda dispatching to clauses with mu-case
   (mu--multi-head-lambda `(&rest ,id) id clauses))

  ;; ARGS: (mu (a b &rest args) clauses)
  ([(and (pred listp) (pred (lambda (arg) (some #'mu--rest? arg))) arglist) |
    (and (pred mu--defun-clauses?) clauses)]
   (mu-let ((split-args (mu--split-when #'mu--rest? arglist))
            ;; split the arglist into positional head-args and the catch all
            ;; argument after &rest
            ([head-args [rest-arg]] split-args)
            ;; ensure only the &rest delimiter is used
            (arglist (concatenate 'list head-args `(&rest ,rest-arg))))
     ;; wrap in a lambda that uses the supplied arglist and dispatches to clauses
     ;; by mu-case matching the rest-arg
     (mu--multi-head-lambda arglist rest-arg clauses)))

  ;; ARGS: unrecogsized call-pattern
  (otherwise
   `(mu-error "in mu-lambda malformed arglist or body")))


(defalias 'μ 'mu)


;;* todo --------------------------------------------------------- *;;


;; TODO (ht), other prelude.el that's used

;; TODO replace (pred ..) pattern with a shorter (? ..) maybe, alternatively we
;; could just allow straight up #'functions or (lambda (a) ..) and treat them as
;; predicates, but this may obscure intent.
;;
;; (mu-case e
;;   [(and (? listp) (? rest-arglist?) arglist) |
;;    (and (? mu--defun-clauses?) clauses)]
;;   (body))
;;
;; or even cleaner:
;;
;; (mu-case e
;;   [(and #'listp #'rest-arglist? arglist) |
;;    (and #'mu--defun-clauses? clauses)]
;;   (body))

;; TODO define custom mu-patterns to clarify hairy pattern-matching!
;; - (id foo) => (and (pred symbolp) foo)
;; - (mu-arglist head-args rest-arg) => (app mu--split-at-rest [head-args [rest-arg]])

;; TODO with the above in mind I may want to have a mechanism to namespace
;; user-defined patterns lest they start to clash with each other. Would that be
;; an overkill? I could also expand into an actual defun with randomly or
;; systematically generated name, then the :mu-patterns table would map pattern
;; NAME to that defun. I could also store edebug-specs on that defun's symbol.

;; TODO Perfwise byte-compilation should work. Also consider byte-compiling
;; generated functions, or maybe even generate an actual named function instead of
;; a lambda so that we can byte-compile it, e.g. for custom patterns? Also see
;; opportunities for inlining, where an actual function call can be avoided.
;; Before any of this perf-tuning I'd need a whole bunch of cases to time as make
;; changes.

;; TODO Replace lambdas with defuns where it would make code more readable. If we
;; are to byte-compile everything then having lambda or an actual #'defun
;; shouldn't effect performance.

;; TODO consider places where we could catch errors including `pcase' errors, so
;; that we can report in terms of mu-patterns and propagate some context. Sadly,
;; `pcase' unimaginatively throws 'error, ugh.

;; TODO Found a thread in emacs-devel that shows off how one might step-into
;; symbol being bound by `pcase'. I don't think it made it into the codebase, but
;; maybe smth helpful for us. IIUC the idea is to rewrite symbol pattern into an
;; explicit let-pattern. I don't understand the edebug mechanics.
;; https://lists.gnu.org/archive/html/emacs-devel/2015-10/msg02285.html

;; TODO I wonder if edebug-specs could somehow be used to make `eldoc' smarter
;; about highlighting arguments as you type?

;; TODO Allow to force-list in seq-patterns. See `mu-seq-pattern-force-list'.

;; TODO Allow mu-lambdas in standard patterns pred, app etc


;;* provide ------------------------------------------------------ *;;


(provide 'multi-patterns)
