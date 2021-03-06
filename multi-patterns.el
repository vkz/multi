;; -*- lexical-binding: t; -*-

;; Copyright (C) 2018, 2019 by Vlad Kozin

(require 'multi-prelude)


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
       ("app" mu-fun-pat mu-pat)
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

(def-edebug-spec mu-arglist-arg
  (&or ["&optional" "&rest" "&" "|"] arg))

(def-edebug-spec mu-defun-arglist
  (&or (vector &rest mu-pat)
       symbolp
       (&rest
        [&or "&optional"
             "&rest" "&" "|"
             arg])))


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


(def-edebug-spec mu
  (&define mu-defun-arglist mu-defun-body))


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

  ;; NOTE there's some nasty bug in `ert' where as best I can guess it holds onto
  ;; the arguments of functions being called between reruns, so if your function
  ;; mutates them and you re-run the test it may get incorrect value as argument.
  ;; It's as if arguments were passed as pointers, ugh. Introducing intermediate
  ;; local REVERSED-CLAUSES was the only thing I could think up.

  ;; reverse clauses so that 'otherwise comes first if present
  (let ((reversed-clauses (reverse clauses)))
    (loop for clause in reversed-clauses
          with code
          if (eq 'otherwise (car clause))
          do (setq code clause)
          else
          do (setq code `(otherwise (pcase ,expr
                                      ,clause
                                      ,@(when code (list code)))))
          ;; remove extraneous outermost (otherwise ..)
          finally return `(progn ,@(cdr code)))))


(defvar mu-prefer-nested-pcase nil
  "`pcase' expander may on occasion produce pathological
expansions, where a reasonable 4-clause matcher expands into over
160K lines of code. Toggling this parameter where this happens
will force `mu-case' to convert generated pcase-clauses into a
tree of nested pcase-calls before handing it over to `pcase'.
This shrinks the expansion by orders of magnitude but may defeat
some optimizations `pcase' could have undertaken had it known all
the clauses (citation needed).")


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
        ;; TODO remove redundant otherwise clauses
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
    ;; TODO allow mu-lambda in (app fun pat)
    (`(app ,fun ,pat)         (list 'app fun (mu--pat-unquoted seq-pat pat)))
    (`(let ,pat ,exp)         (list 'let (mu--pat-unquoted seq-pat pat) exp))
    (`(pred ,pred)            (list 'pred pred))
    ;; (? pred) pattern, which we can't mu-defpattern since ? translates into 32
    (`(32 ,pred)              (list 'pred pred))
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

  (let (install-debug-spec)

    (unless (stringp docstring)
      (push docstring body)
      (setq docstring "mu-pattern"))

    ;; extract edebug-spec if present
    (when (eq :debug (car body))
      ;; TODO this pollutes pattern symbol with edebug prop, I should store the
      ;; spec in :mu-patterns table instead and have `mu-pat--edebug-match'
      ;; extract them as needed
      (setq install-debug-spec `((def-edebug-spec ,name ,(cadr body)))
            body (cddr body)))

    (let ((mu-patterns `(or (get 'mu--case :mu-patterns)
                            (put 'mu--case :mu-patterns (ht))))
          (pattern-macro `(lambda ,arglist ,docstring ,@body)))
      ;; add pattern to the mu-patterns table
      `(progn
         ,@install-debug-spec
         (setf (ht-get ,mu-patterns ',name) ,pattern-macro)))))


(defun mu-pattern-documentation (name)
  "Extract docstring from custom mu-pattern NAME"
  (documentation (ht-get (get 'mu--case :mu-patterns) name)))


(defun mu--seq-split (seq pat-len)
  (let* ((subseq (seq-take seq pat-len))
         (took (length subseq))
         ;; rest is empty if seq was shorter than patterns
         (rest (seq-subseq seq took)))
    (when mu-seq-pattern-force-list
      (setq rest (seq-into rest 'list)))
    (list subseq rest)))


(defun mu--seq-split-and-pad (seq pat-len)
  (let* ((type (cond
                ((listp seq) 'list)
                ((vectorp seq) 'vector)
                ;; NOTE If ever signaled, then there must be a bug. Matching
                ;; non-seq with a seq-pattern should simply fail, not raise.
                (:else (mu-error :seq-pattern (type-of seq)))))
         (subseq (seq-take seq pat-len))
         (took (length subseq))
         (less-by (- pat-len took))
         ;; rest is empty if seq was shorter than patterns
         (rest (seq-subseq seq took)))
    (when mu-seq-pattern-force-list
      (setq rest (seq-into rest 'list)))
    ;; return (list padded-head-seq rest-seq)
    (list (if (< took pat-len)
              ;; pad head with nils
              (seq-concatenate type subseq (make-list less-by nil))
            subseq)
          rest)))


;;** - l-pattern ------------------------------------------------- *;;


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


;;** - v-pattern ------------------------------------------------- *;;


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


;;** - lv-pattern ------------------------------------------------ *;;


(defcustom mu-seq-pattern-force-list nil
  "Force seq-pattern to always cast its &rest submatch to a list.
By default &rest submatch preserves the type of sequence being
matched."
  :options '(list nil))


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


;;** - seq-pattern ----------------------------------------------- *;;


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


;;** - ht-pattern ------------------------------------------------ *;;


(defun mu--ht-pattern (patterns)
  (mu-case patterns
    ((l) '())

    ;; (ht :a :b)
    ((l (and kw (pred keywordp) (app sym id)) &rest pats)
     `(((app (lambda (ht) (or (ht-get ht ,kw) (ht-get ht ',id))) ,id)
        (app (lambda (ht) (or (alist-get ,kw ht) (alist-get ',id ht))) ,id)
        (app (lambda (ht) (mu. ht ,kw)) ,id))
       ,@(mu--ht-pattern pats)))

    ;; (ht 'a 'b)
    ((l (l 'quote (and id (pred symbolp) (app (lambda (id) (sym ":" id)) kw))) &rest pats)
     `(((app (lambda (ht) (or (ht-get ht ',id) (ht-get ht ,kw))) ,id)
        (app (lambda (ht) (or (alist-get ',id ht) (alist-get ,kw ht))) ,id)
        (app (lambda (ht) (mu. ht ,kw)) ,id))
       ,@(mu--ht-pattern pats)))

    ;; (ht a b)
    ((l (and id (pred symbolp) (app (lambda (id) (sym ":" id)) kw)) &rest pats)
     `(((app (lambda (ht) (or (ht-get ht ,kw) (ht-get ht ',id))) ,id)
        (app (lambda (ht) (or (alist-get ,kw ht) (alist-get ',id ht))) ,id)
        (app (lambda (ht) (mu. ht ,kw)) ,id))
       ,@(mu--ht-pattern pats)))

    ;; (ht (:a A) (:b B))
    ((l (l key id) &rest pats)
     `(((app (lambda (ht) (ht-get ht ,key)) ,id)
        (app (lambda (ht) (alist-get ,key ht)) ,id)
        (app (lambda (ht) (mu. ht ,key)) ,id))
       ,@(mu--ht-pattern pats)))

    (otherwise
     (mu-error :ht-pattern patterns))))


(mu-defpattern ht (&rest patterns)
  "mu-pattern for hash-tables, structs and alists.

------------------------------------------------
PATTERNS = (key-pat ...)
 key-pat = id | keywordp | 'symbolp | (key id)
------------------------------------------------

Keyword key-pat looks up :key then 'key in order binding value to
variable `key'. Quoted symbol key-pat tries in order 'key then
:key. (key id) looks up `key' binding value to `id' on success."
  (let* ((patterns (mu--ht-pattern patterns))
         (ht-pats (mapcar #'car patterns))
         (alist-pats (mapcar #'cadr patterns))
         (struct-pats (mapcar #'caddr patterns)))
    `(or (and (pred ht-p) ,@ht-pats)
         (and (pred recordp) ,@struct-pats)
         (and (pred listp) ,@alist-pats))))


;;** - ht|-pattern ----------------------------------------------- *;;


;; NOTE basic idea for ht| pattern: rewrite into an app-pattern where the applied
;; function collects all :attr value pairs at the start of the sequence, shoves
;; them into a hash-table and returns (list hash-table seq-tail) for matching
;; against a [(ht pat...) tail-pat].


;; NOTE My first version was a trivial recursive function. This loop is ugly but I
;; guess more performant? I should've measured.
(cl-defun mu--prefix-map (body &optional (key-pred #'keywordp))
  "Collect prefix of a sequence into a hash-table, return (list
ht seq-tail) for further pattern-matching."
  (let* ((vector? (vectorp body))
         (body    (if vector? (seq-into body 'list) body))
         (pairs   (cl-loop for item in body
                           when (funcall key-pred item)
                           collect (cons (pop body) (pop body)))))
    (list (ht<-alist pairs)
          (if vector? (apply #'vector body) body))))


(mu-defpattern ht| (&rest patterns)
  "Mu-pattern for key-value sequence prefix. Try to match and
collect sequence elements pair-wise as though they were elements
of a hash-table. PATTERNS are key-patterns like in ht-pattern
followed by an optional []-pattern to match the rest of the
sequence.

------------------------------------------------
PATTERNS = (key-pat ... [seq-pattern])
 key-pat = id | keywordp | 'symbolp | (key id)
------------------------------------------------"
  (let* ((patterns (nreverse patterns))
         (seq-pat (if (vectorp (car patterns)) (pop patterns) '_)))
    `(app mu--prefix-map [(ht ,@patterns) ,seq-pat])))


(comment
 ;; implementation that lets you specify a custom key-predicate
 (mu-defpattern ht| (&optional key-predicate &rest patterns)
   (unless (mu-function? key-predicate)
     (push key-predicate patterns)
     (setq key-predicate nil))
   (let* ((patterns (nreverse patterns))
          (seq-pat (if (vectorp (car patterns)) (pop patterns) '_))
          (prefix-extract (if key-predicate
                              (lambda (seq) (mu--prefix-map seq key-predicate))
                            'mu--prefix-map)))
     `(app ,prefix-extract [(ht ,@patterns) ,seq-pat])))

 (mu-case '(a 1 b 2 3 4 5)
   ([| (ht| symbolp a b [| rest])] (list a b rest)))

 ;; Alternative syntax that maybe cleaner:
 (ht| (? symbolp) pat pat [pat])
 ;; comment
 )


;;** - id-pattern ------------------------------------------------ *;;


(mu-defpattern id (binding)
  "Mu-pattern for identifiers - symbols that maybe used as
variable names. E.g. it wil not match `t' or `nil'."
  (let ((id? (lambda (s) (and (symbolp s)
                         (not (eq s '()))
                         (not (eq s 't))))))
    `(and (pred ,id?) ,binding)))


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
  "Like `pcase' but uses mu-patterns for matching ..."
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
  (if (vectorp bindings)
      (seq-partition (seq-into bindings 'list) 2)
    bindings))


(defmacro mu-let (bindings &rest body)
  "Like `let*' but allow mu-patterns in place of bindings ..."
  (declare (indent 1)
           (debug ((&rest (sexp form)) body)))
  (condition-case err
      (mu--let (mu--let-bindings bindings) body)
    (mu-error `(mu-error ,(cadr err)))))


(defmacro mu-when-let (bindings &rest body)
  "Like `when-let*' but allow mu-patterns in binding clauses. See
`mu-let'."
  (declare (indent 1)
           (debug ((&rest (sexp form)) body)))
  (condition-case err
      (mu--when-let (mu--let-bindings bindings) body)
    (mu-error `(mu-error ,(cadr err)))))


(defmacro mu-if-let (bindings then-body &rest else-body)
  "Like `if-let*' but allow mu-patterns in binding clauses. See
`mu-let'."
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


(defun mu-function? (obj)
  "Like functionp but accounts for #'function and mu-lambda.
Intended to be used at compile time on code objects. Not
guaranteed to always do the right thing at runtime."
  (or (functionp obj)
      (when (listp obj)
        (or (eq 'function (car obj))
            (eq 'mu (car obj))
            (eq 'μ (car obj))
            (and (eq 'quote (car obj))
                 (functionp (cadr obj)))))))


(defun mu--defun-meta (body &optional map)
  "Return a hash-table of attribute value pairs from `mu-defun'
preamble"
  (default map :to (ht))
  (cond
   ((keywordp (car body)) (ht-set map (pop body) (pop body)) (mu--defun-meta body map))
   (:else                 (ht-set map :body body) map)))


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


;; TODO used in `mu-defmulti' but really confusingly named and probably doesn't
;; belong here!
(defun mu--defun-multi-head-body (body)
  "If body is a list of :attr value pairs followed by
mu-defun-clauses return the result of applying `mu--prefix-map'
to it, else nil. Do not treat emty body as mu-defun-clauses."
  (let ((attrs-clauses (mu--prefix-map body)))
    (when (mu--defun-clauses? (cadr attrs-clauses))
      attrs-clauses)))


;; :before form
(defun mu--add-before-call (body attrs)
  "If ATTRS has :before code alter BODY to run it first."
  ;; NOTE we expect BODY to be a mu-case expr
  (if-let ((before (and (ht-p attrs) (ht-get attrs :before)))
           (var    (gensym "before")))
      (progn
        ;; generate defvar and pass it to be installed at top level
        (setf (ht-get attrs :before-defvar) `(defvar ,var t))
        `(progn
           ;; only run before-code when var is set
           (when ,var ,before)
           ;; disable before-code in recursive calls
           (let ((,var nil)) ,body)))
    body))


;; :setup form
(defun mu--add-setup-call (body attrs)
  "If ATTRS has :setup code alter BODY to run it first."
  ;; NOTE we expect BODY to be a mu-case expr
  (if-let ((setup (and (ht-p attrs) (ht-get attrs :setup))))
      `(progn ,setup ,body)
    body))


;; :after form
(defun mu--add-after-call (body attrs)
  "If ATTRS has :after CODE alter BODY to run it last."
  ;; NOTE we expect BODY to be a mu-case expr
  (let (return after)

    (when (ht-p attrs)
      (setq return (ht-get attrs :return)
            after (ht-get attrs :after)))

    (when (and return (not (symbolp return)))
      (mu-error :defun-return))

    (if after
        ;; extend body with after code
        (let ((return (or return (gensym "return")))
              (var (gensym "after")))
          ;; generate defvar and pass it to be installed at top level
          (setf (ht-get attrs :after-defvar) `(defvar ,var t))
          ;; disable after-code in recursive calls by resetting var
          `(let ((,return (let ((,var nil)) ,body)))
             ;; only run after-code when var is set
             (when ,var ,after)
             ,return))
      ;; body unchanged
      body)))


;; :teardown form
(defun mu--add-teardown-call (body attrs)
  "If ATTRS has :teardown CODE alter BODY to run it last."
  ;; NOTE we expect BODY to be a mu-case expr
  (let (return teardown)

    (when (ht-p attrs)
      (setq return (ht-get attrs :return)
            teardown (ht-get attrs :teardown)))

    (when (and return (not (symbolp return)))
      (mu-error :defun-return))

    (if teardown
        ;; extend body with teardown code
        (let ((return (or return (gensym "return"))))
          `(let ((,return ,body))
             ,teardown
             ,return))
      ;; body unchanged
      body)))


(defun mu--single-head-body (name case-expr-arg body &optional attrs)
  (let* ((body `(mu--case seq ,case-expr-arg
                  ,@body
                  (otherwise (mu-error :defun-no-match ',name))))
         ;; setup & teardown
         (body (mu--add-setup-call body attrs))
         (body (mu--add-teardown-call body attrs))
         ;; before & after
         (body (mu--add-before-call body attrs))
         (body (mu--add-after-call body attrs)))
    body))


(defun mu--multi-head-body (name lambda-arglist case-expr-arg body &optional attrs)
  (let* ((body `(mu-case ,case-expr-arg
                  ,@body
                  (otherwise
                   (mu-error :defun-no-match ',name))))
         ;; setup & teardown
         (body (mu--add-setup-call body attrs))
         (body (mu--add-teardown-call body attrs))
         ;; before & after
         (body (mu--add-before-call body attrs))
         (body (mu--add-after-call body attrs)))
    (if lambda-arglist
        `(apply (lambda ,lambda-arglist ,body) ,case-expr-arg)
      body)))


(defun mu--wrap-defun (fun-type name arglist docstring attrs &rest body)
  (mu-let (((ht debug test before return after
                before-defvar after-defvar
                (:declare dspec)
                (:interactive ispec))
            attrs))
    `(progn
       ,before-defvar
       ,after-defvar
       (,fun-type
        ,name ,arglist
        ,docstring
        ,@(when dspec `((declare ,@dspec)))
        ,@(when ispec `((interactive ,@(if (equal 't ispec) '() (list ispec)))))
        ,@body))))


(defun mu--defun (fun-type name arglist body wrap-body)
  (let (attrs
        (docstring ""))

    ;; extract docstring from body
    (when (stringp (car body))
      (setq docstring (car body)
            body (cdr body)))

    ;; extract attributes from body
    (setq attrs (mu--defun-meta body))
    (setq body (ht-get attrs :body))

    (cond

     ;; arglist is [pat...]
     ((vectorp arglist) (let ((args (gensym "args")))
                          (funcall
                           wrap-body
                           fun-type name `(&rest ,args) docstring attrs
                           (mu--single-head-body
                            name args
                            `(((lv ,@(seq-into arglist 'list)) ,@body))
                            attrs))))

     ;; arglist is '()
     ((null arglist) `(mu-error :defun-malformed-arglist ',arglist))

     ;; arglist is a symbol
     ((symbolp arglist)
      (if (not (mu--defun-clauses? body))
          `(mu-error :defun-malformed-body ',body)
        (let ((args (if (eq arglist '_) (gensym "id") arglist)))
          (funcall
           wrap-body
           fun-type name `(&rest ,args) docstring attrs
           (mu--multi-head-body name nil args body attrs)))))

     ;; arglist is (a b &rest args)
     ((listp arglist)
      (if (not (mu--defun-clauses? body))
          `(mu-error :defun-malformed-body ',body)
        (let ((args (gensym "args")))
          (funcall
           wrap-body
           fun-type name `(&rest ,args) docstring attrs
           (mu--multi-head-body name arglist args body attrs)))))

     ;; arglist is matches no call-pattern
     (:unrecognized-call-pattern
      `(mu-error :defun-malformed-arglist ',arglist)))))


(defmacro mu-defun (name arglist &rest body)
  "Like `defun' but with multiple clauses ..."
  (declare (indent 2))
  (mu--defun 'defun name arglist body #'mu--wrap-defun))


(defmacro mu-defmacro (name arglist &rest body)
  "Like `defmacro' but with multiple clauses ..."
  (declare (indent 2)
           (debug mu-defun))
  (mu--defun 'defmacro name arglist body #'mu--wrap-defun))


;;* mu-lambda ---------------------------------------------------- *;;


;; Provide single-head and multi-head anonymous functions. These translate into
;; simple lambdas, so should be fine to use in most places a lambda would work.
;; However, it isn't unusual for a macro to introspect that something is a lambda
;; e.g. with a `functionp' or by checking the car of a list etc. I know of no way
;; to extend `functionp', so mu-lambdas won't be recognised as functions until
;; evaluated. Exercise caution when passing mu-lambdas as arguments to macros.


(defun mu--wrap-lambda (fun-type name arglist docstring attrs &rest body)
  `(lambda ,arglist ,@body))


(defmacro mu (arglist &rest body)
  "Create an anonymous function, otherwise like `mu-defun'.

\(fn ARGLIST METADATA BODY...)"

  (declare (indent 1))
  (mu--defun 'mu-lambda 'mu-lambda arglist body #'mu--wrap-lambda))


(defalias 'μ 'mu)


;;* mu-defsetter ------------------------------------------------- *;;


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


(defmacro mu-defsetter (id &rest body)
  "Like `gv-define-setter' but allow `mu-defun' dispatch and
destructuring."
  (declare (indent 2)
           (debug (&or [&define name mu] mu-defun)))
  (let* ((gv-args (gensym "gv-args"))
         (do      (gensym "do"))
         (setter  (gensym "setter"))
         (handler (if (mu-function? (car body))
                      (car body)
                    `(mu ,@body))))
    `(let ((,setter ,handler))
       (function-put ',id 'gv-expander
                     (lambda (,do &rest ,gv-args)
                       (gv--defsetter ',id
                                      ,setter
                                      ,do ,gv-args))))))


;;* docs --------------------------------------------------------- *;;


(mu-docfun mu-case
  "Like `pcase' but uses mu-patterns for matching.

------------------------------
      E = sexp
CLAUSES = (clause ...)
 clause = (pattern body ...)
        | (otherwise body ...)
------------------------------

Any sequence []-pattern is treated strictly - must match the
entire sequence to succeed.")


(mu-docfun mu-let
  "Like `let*' but allow mu-patterns in binding clauses. Any
pattern-variables bound during pattern matching will be available
in the BODY.

-------------------------------
BINDINGS = ((pattern expr) ...)
         | `['clause ...`]'

  clause = pattern expr
-------------------------------

Any sequence []-pattern is permissive.")


(mu-docfun mu-defun
  "Like `defun' but choose the body to execute by
pattern-matching on the arglist. Clauses are tried in order as if
multiple definitions of the same function NAME were defined.

------------------------------------
    ARGLIST = seq-pattern
            | _
            | id
            | (args ...)

   METADATA = [docstring] attr ...

       attr = :declare form
            | :interactive form
            | :before form
            | :after form
            | :return id
            | :setup form
            | :teardown form

       BODY = body
            | clause ...

     clause = (seq-pattern body ...)

seq-pattern = `['pattern ...`]'
------------------------------------

In addition to any pattern-variables bound by clause-patterns
each body has ARGLIST variables in scope.

In attribute options :declare takes a list of `declare' specs;
:interactive is either `t' or an `interactive' arg-descriptor;
:return binds VAR to the result of BODY; :setup and :teardown
execute their respective forms for side-effect before and after
BODY. Both forms have ARGLIST bindings in scope, :teardown form
has access to the VAR when :return is specified. To avoid before
and after forms being executed on every recursive call use
:before and :after attributes instead.

In a single-head function ARGLIST must be a []-pattern. In a
multi-head function ARGLIST that is an id will bind ARGLIST to
that id; ARGLIST that is `_' will be ignored; ARGLIST must be a
`defun' arglist otherwise.

\(fn NAME ARGLIST METADATA &rest BODY)")


(mu-docfun mu-defmacro (documentation 'mu-defun))


;;* todo --------------------------------------------------------- *;;

;; TODO maybe I should allow attr options in mu-case related forms: mu-case,
;; mu-let, mu-defun, mu:
;;
;;   (mu-case expr
;;     :seq  seq
;;     :ns   custom-pat-namespace
;;     :nest t
;;     (pat1 body1)
;;     (pat2 body2))

;; TODO [] in mu-defun should really be rewritten to l-pattern since we don't care
;; to match sequences here - arglist is a list after all, no reason for overhead.

;; TODO I'd like to be able to capture prefix-map as hash-table and bind it, so
;; maybe ht| should accept two arguments each a mu-pattern (:& instead of ht|):
;;
;; (:& ht-pat rest-seq-pat)
;; (:& table rest)
;; (:& (ht a b) [foo bar])

;; TODO allow to maintain state between recursive mu-defun calls:
;;
;;   (mu-defun foo (arglist)
;;     :with ((a (ht)))
;;     ;; we want vars A and B to persist between calls, so that e.g. clause1 can
;;     ;; update A and B, recurse, and the next clause will see those changes. Pretty
;;     ;; much a let-ovel-lambda pattern
;;     ([pat] body)
;;     ([pat pat] body))
;;
;;   =>
;;
;;   (let* ((a (ht)))
;;     (mu-defun foo-let-over-lambda _
;;       ([] a)
;;       ([v] (ht-set a 1 v)  (foo-let-over-lambda))))

;; TODO namespace custom patterns

;; TODO Perfwise byte-compilation should work. Also consider byte-compiling
;; generated functions, or maybe even generate an actual named function instead of
;; a lambda so that we can byte-compile it, e.g. for custom patterns? Also see
;; opportunities for inlining, where an actual function call can be avoided.
;; Before any of this perf-tuning I'd need a whole bunch of cases to time as make
;; changes.

;; TODO Replace lambdas with defuns where it would make code more readable. If we
;; are to byte-compile everything then having lambda or an actual #'defun
;; shouldn't effect performance or would it?

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

;; TODO Add support for `find-func'. I think its used internally to generate links
;; in Help buffers. I'd need to confirm this is something usefull. See
;; `find-function-regexp-alist' and see how `cl-generic.el' did it.

;; TODO Allow mu-lambdas in standard app-pattern etc: one way is to macroexpand
;; arguments at compile time, another is to generate a (let ..) pattern. Wonder if
;; the former would be less expensive?

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

;; TODO bugs in gv.el. This most certainly plagues my `mu-defsetter', too
(comment
 (cl-defstruct baz name)

 (setq foo (make-baz :name 'baz))

 ;; just a getter
 (defmacro bazzer (struct slot)
   `(cl-struct-slot-value 'baz ',slot ,struct))

 (bazzer foo name)

 ;; Setter that doesn't work, because `gv--defsetter' is broken in at least two
 ;; ways and it hits both in this case. We could fix it in this case by defining
 ;; `multi' as a defun instead of a macro passing it slot as a quoted symbol.
 (gv-define-setter bazzer (val struct slot)
   `(setf (cl-struct-slot-value 'baz ',slot ,struct) ,val))

 ;; this signals that `name' is not defined
 (setf (bazzer foo name) 'bar)

 ;; the expansion shows us exactly why: `gv--defsetter' is broken in at least two
 ;; ways.
 ;;   (let* ((v foo)
 ;;          (v name))
 ;;     (setf (cl-struct-slot-value 'baz 'v v) 'bar))
 ;;
 ;; 1. the variables it uses to let-bind args in the arglist all use the same
 ;; symbol `v' so they end up shadowing each other when there's more than one
 ;; argument.
 ;;
 ;; 2. If gv isn't a defun but a macro it unassumingly inserts its arguments as
 ;; let-values so if like in our example your macro expects to receive an unquoted
 ;; symbol which it'll correctly quote in the code it generates, well, tough luck.
 ;; `gv--defsetter' will ignore that.

 ;; Possible fix for problem 1. is something along these lines:
 (defun gv--defsetter (name setter do args &optional vars)
   ;; if-test and consequent unchanged from the original
   (if (null args)
       (let ((vars (nreverse vars)))
         (funcall do `(,name ,@vars) (lambda (v) (apply setter v vars))))
     ;; new code
     (let ((v (gensym "v")))
       `(let* ((,v ,(car args)))
          ,(gv--defsetter name setter do (cdr args) (cons v vars))))))

 ;; Dunno about problem 2.

 ;; comment
 )


;;* provide ------------------------------------------------------ *;;


(provide 'multi-patterns)
