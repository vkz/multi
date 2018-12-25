;; -*- lexical-binding: t; -*-


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


;;* mu-error ----------------------------------------------------- *;;


;; Introduce a custom mu-error to differentiate signals specific to the multi
;; feature. Consider raising mu-error whenever it relates to multi-pattern
;; matching or multi-dispatch. `mu-error' function simplifies this by
;; intentionally following the exact same calling convention as `error'. Please,
;; use it.


(define-error 'mu-error "mu-error")


(defconst mu--errors
  (ht
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
   (:setter-no-match         '("in mu-setter no matching clause for %s")))
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

    ;; extract edebug-spec if present
    (when (eq :debug (car body))
      ;; TODO this pollutes pattern symbol with edebug prop, I should store the
      ;; spec in :mu-patterns table instead and have `mu-pat--edebug-match'
      ;; extract them as needed
      (setq install-debug-spec `((def-edebug-spec ,name ,(cadr body)))
            body (cddr body)))

    ;; just for consistency, when present, cons the docstring onto the body
    (when docstring
      (unless (stringp docstring)
        (setq body (cons docstring body))))

    (let ((mu-patterns `(or (get 'mu--case :mu-patterns)
                            (put 'mu--case :mu-patterns (ht))))
          (pattern-macro `(lambda ,arglist ,@body)))
      ;; add pattern to the mu-patterns table
      `(progn
         ,@install-debug-spec
         (setf (ht-get ,mu-patterns ',name) ,pattern-macro)))))


(defun mu--seq-split (seq pat-len)
  (let* ((subseq (seq-take seq pat-len))
         (took (length subseq)))
    (list subseq
          ;; rest is empty if seq was shorter than patterns
          (seq-subseq seq took))))


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
Without this setting the &rest supattern match will preserve the
type of the sequence being matched."
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


;;** - ht|-pattern ----------------------------------------------- *;;


;; NOTE basic idea for ht| pattern: rewrite into an app-pattern where the applied
;; function collects all :attr value pairs at the start of the sequence, shoves
;; them into a hash-table and returns (list hash-table seq-tail) for matching
;; against a [(ht pat...) tail-pat].


;; TODO This implementation maybe too general. Do I ever expect to match vectors
;; with an ht| pattern? Do I ever expect keys to be anything but :keywords?


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
  "Match sequence prefix as if its a hash-table with :keyword
keys. Patterns are keyword-patterns allowed in ht-pattern
followed by an optional []-pattern to match the rest of the
sequence:

  (mu-case '(:a 1 :b 2 body)
     ([| (ht| a b)] (list a b)))
  =>
  '(1 2)

  (mu-case '(:a 1 :b 2 body)
     ([| (ht| a b [| rest])] (list a b rest)))
  =>
  '(1 2 (body))"
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


;; TODO Is it a bad idea? Should I simply allow the two cases:
;;   (mu-let [pat val   ...] body)
;;   (mu-let ((pat val) ...) body
;; and dispense with this stupid option?
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


(defun mu-function? (obj)
  (or (functionp obj)
      (when (listp obj)
        (or (eq 'mu (car obj))
            (eq 'μ (car obj))))))


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


(defun mu--single-head-body (name case-expr-arg body)
  `(mu--case seq ,case-expr-arg
     ,@body
     (otherwise (mu-error :defun-no-match ',name))))


(defun mu--multi-head-body (name lambda-arglist case-expr-arg body)
  (let ((body `(mu-case ,case-expr-arg
                 ,@body
                 (otherwise
                  (mu-error :defun-no-match ',name)))))
    (if lambda-arglist
        `(apply (lambda ,lambda-arglist ,body) ,case-expr-arg)
      body)))


(defun mu--wrap-defun (fun-type name arglist docstring attrs &rest body)
  (mu-let (((ht ret debug test
                (:declare dspec)
                (:interactive ispec))
            attrs))
    `(,fun-type
      ,name ,arglist
      ,docstring
      ,@(when dspec `((declare ,@dspec)))
      ,@(when ispec `((interactive ,@(if (equal 't ispec) '() (list ispec)))))
      ,@body)))


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
                            `(((lv ,@(seq-into arglist 'list)) ,@body))))))

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
           (mu--multi-head-body name nil args body)))))

     ;; arglist is (a b &rest args)
     ((listp arglist)
      (if (not (mu--defun-clauses? body))
          `(mu-error :defun-malformed-body ',body)
        (let ((args (gensym "args")))
          (funcall
           wrap-body
           fun-type name `(&rest ,args) docstring attrs
           (mu--multi-head-body name arglist args body)))))

     ;; arglist is matches no call-pattern
     (:unrecognized-call-pattern
      `(mu-error :defun-malformed-arglist ',arglist)))))


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
    :declare     dspec
    :interactive ispec
    ([mu-case-args-pat1] body1)
    ([mu-case-args-pat2] body2)
      ... ...)

In addition to any variable bound by the corresponding pattern
every clause has the entire ARGLIST in scope.

METADATA is optional and may include the following attributes:

  :doc dostring - a docstring to attach to the NAME function,

  :declare dspec - a list of `declare' SPECS,

  :interactive ispec - t or `interactive' ARG-DESCRIPTOR,

\(fn NAME ARGLIST METADATA &rest BODY)"
        fun-type fun-type))
  nil)


(defmacro mu-defun (name arglist &rest body)
  (declare (indent 2))
  (mu--defun 'defun name arglist body #'mu--wrap-defun))


(defmacro mu-defmacro (name arglist &rest body)
  (declare (indent 2)
           (debug mu-defun))
  (mu--defun 'defmacro name arglist body #'mu--wrap-defun))


;; add docstring to `mu-defun'
(mu--set-defun-docstring 'defun)


;; add docstring to `mu-defmacro'
(mu--set-defun-docstring 'defmacro)


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
  "Create a lambda-like anonymous function but allow `mu-defun'
style single-head destructuring or multi-head dispatching and
destructuring:

  (mu [patterns] body)
  (mu arglist ([patterns] body) mu-clause...)

\(fn [patterns] body)"

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


;;* todo --------------------------------------------------------- *;;


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

;; TODO Allow to force-list in seq-patterns. See `mu-seq-pattern-force-list'.

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


;;* provide ------------------------------------------------------ *;;


(provide 'multi-patterns)
