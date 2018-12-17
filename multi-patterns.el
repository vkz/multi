;; -*- lexical-binding: t; -*-


(require 'cl)


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


;;* prelude ------------------------------------------------------ *;;




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


;; NOTE I have no idea what I'm doing here. It works but I wish there were a way
;; to inspect how Edebug performs matching. I roughly follow examples in the INFO
;; and the specs I've seen in other people's code. Gives me grief but does the
;; trick.


;; make byte-compiler happy
(declare-function edebug-match "edebug" (cursor specs))
(declare-function get-edebug-spec "edebug" (symbol))


;; main pattern spec:
;; - standard `pcase' patterns,
;; - built-in sequential patterns we add,
;; - any custom patterns instroduced with `mu-defpattern'
(def-edebug-spec mu-pat
  ;; TODO Found a thread in emacs-devel that shows off how one might step-into
  ;; symbol being bound by `pcase'. I don't think it made it into the codebase,
  ;; but maybe smth helpful for us. IIUC the idea is to rewrite symbol pattern
  ;; into an explicit let-pattern. I don't understand the edebug mechanics.
  ;; https://lists.gnu.org/archive/html/emacs-devel/2015-10/msg02285.html
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
;; differently and I'd like to namspace them eventually, so we can't blindly copy
;; pcase solution, sigh
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


;; TODO I wonder if I could could use this in the docstring and eldoc would be
;; smart enough to infer and highlight the case as the user's typing?
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
                       " &rest argument in %S"))))


(defun mu-error (&rest args)
  "Signals errors specific to `multi' library. Can be caught with
'mu-error ERROR-SYMBOL in `condition-case', otherwise behaves
exactly like `error'. Alternatively takes a keyword as the first
ARG to lookup corresponding error-msg in the `mu--errors' table,
passing the rest of ARGS to the message.

\(fn string &rest args)"
  (if-let ((msgs (ht-get mu--errors (car args))))
      (signal 'mu-error (list (apply #'format-message
                                     (string-join msgs "")
                                     (cdr args))))
    (signal 'mu-error (list (apply #'format-message args)))))


;;* mu-patterns -------------------------------------------------- *;;


(defun mu--pcase-nest (expr clauses)
  "Generate a `pcase' matcher where each clause is itself a
`pcase' expression. Every clause except the first becomes the
'otherwise body of the preceding clause:

  '((pat1 body1) (pat2 body2))
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


(comment
 (mu--pcase-nest 'expr '())
 (mu--pcase-nest 'expr '((pat1 body1)))
 (mu--pcase-nest 'expr '((pat1 body1) (pat2 body2)))
 (mu--pcase-nest 'expr '((pat1 body1) (pat2 body2) (otherwise body)))
 ;; erroneousely placed otherwise simply cuts clauses short, imo reasonable
 (mu--pcase-nest 'expr '((pat1 body1) (otherwise body) (pat2 body2)))
 ;; lone otherwise simply returns its body, imo reasonable
 (mu--pcase-nest 'expr '((otherwise body1 body2))))


(defmacro mu--case (seq-pat e &rest clauses)
  "`pcase'-like matching and destructuring with [] seq-pattern,
which maybe parameterized by setting SEQ to one of:

  - 'lv for strict sequence matching,
  - 'seq for non-strict sequence matching."
  (declare (indent 2))

  ;; storage for patterns defined with `mu-defpattern'
  (unless (get 'mu--case :mu-patterns)
    (define-symbol-prop 'mu-case :mu-patterns (ht)))

  ;; TODO consider catching pcase errors, so that we can report in terms of
  ;; mu-patterns, else all the guts get spilled and very difficult to debug.
  ;; Sadly, `pcase' stupidly unimaginatively throws 'error, so I'll have to catch
  ;; everything.

  ;; TODO consider adding some use-context then re-throwing and catching in the
  ;; user-facing API functions, which should also enrich context, so the error is
  ;; meaningful to the user

  ;; hack to propagate expansion time errors to runtime
  (condition-case err
      ;; TODO nesting `pcase' or not should be optional, IMO we should let the
      ;; user decide e.g. based on the code the see generated. With default
      ;; nesting we maybe throwing away some optimisations `pcase' has. But I
      ;; really don't know
      (let ((pcase-clauses (mapcar
                            (lambda (clause) (mu-case--clause seq-pat clause))
                            clauses)))
        (mu--pcase-nest e pcase-clauses))
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


;; NOTE All we do here is wrap user "macro" into a (lambda (ARGLIST) BODY) and
;; store it in the hash-table stored in mu-case's plist under :mu-patterns slot.
;; Whenever `mu-case' encounters a (NAME PATTERNS) pattern it looks up the NAME in
;; its :mu-patterns property and calls (apply (lambda (ARGLIST) BODY) PATTERNS)
;; which must produce a valid mu-case pattern. So, technically the user doesn't
;; really define a macro so much as a function that takes patterns and must
;; generate a mu-case pattern, that is `mu-case' effectively plays the role of a
;; "macro expander" by simply invoking the function the user provided at compile
;; time.


(defmacro mu-defpattern (name arglist &optional docstring &rest body)
  "Define a new kind of mu-case PATTERN. Patterns of the
form (NAME &rest PATTERNS) will be expanded by this macro with
PATTERS bound according to the ARGLIST. Macro expansion must
produce a valid `mu-case' pattern. The macro is allowed to throw
`mu-error' to signal improper use of the pattern. This will be
handled correctly to inform the user. Optional DOCSTRING maybe
supplied for the convenience of other programmers reading your
macro code. BODY may start with a :debug EDEBUG-SPEC attribute
pair.

\(fn NAME ARGLIST &optional DOCSTRING &rest BODY)"
  (declare (doc-string 3) (indent 2) (debug defun))

  ;; install edebug spec if present and remove it from the body
  (when (eq :debug (car body))
    ;; TODO This pollutes the symbol NAME! Since we don't actually establish it as
    ;; a variable or a defun and only ever use it as a key in the table, we
    ;; shouldn't be doing that or we could inadvertently overwrite somebody else's
    ;; edebug-spec. Why not just store it in the :mu-pattern table alongside the
    ;; relevant expander and collect it in `mu-pat--edebug-match' as needed?
    (def-edebug-spec name (cadr body))
    (setq body (cddr body)))

  ;; cons the docstring onto the body if present
  (when docstring
    (unless (stringp docstring)
      (setq body (cons docstring body))))

  (let ((mu-patterns `(or (get 'mu--case :mu-patterns)
                          (put 'mu--case :mu-patterns (ht))))
        ;; TODO should I trap errors here to fascilitate debugging and better
        ;; error reporting? I could wrap the body in condition-case
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
  "`mu-case' pattern to match hash-tables and alists. ht expects
to receive key-patterns that would be used to lookup and bind
corresponding values in the hash-table or alist being matched.
Possible key-PATTERNS are:

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


;; TODO force-list in seq-patterns
(defcustom mu-seq-pattern-force-list nil
  "Controls if seq `mu-case' pattern should always produce
subsequences of type 'list even if they resulted from matching a
subsequence of a vector. Set it to 'list if you don't care
whether you're matching lists or vectors and want any pattern
variable bound to subsequence to be a list."
  :options '(list nil))


(defun mu--seq-split-and-pad (seq pat-len)
  (let* ((type (cond
                ((listp seq) 'list)
                ((vectorp seq) 'vector)
                ;; TODO we should never have gotten this error. It may only ever
                ;; happen if we attempt to match non-seq to a seq, but then the
                ;; pattern should just fail to match and move on to another case
                (:else (mu-error :seq-pattern (type-of seq)))))
         ;; TODO replace with loop that also counts elements?
         (subseq (seq-take seq pat-len))
         (took (length subseq))
         (less-by (- pat-len took)))
    ;; return (list padded-head-seq rest-seq)
    (list (if (< took pat-len)
              ;; pad head with nils
              (seq-concatenate type subseq (make-list less-by nil))
            subseq)
          ;; rest: empty if seq was shorter than patterns
          (seq-subseq seq took))))


(mu-defpattern seq (&rest patterns)
  "`mu-case' pattern to match lists and vectors alike. Match as
many patterns against the sequence as possible: fewer patterns
than the sequence will simply match the head of the sequence;
more patterns than the sequence will match available elements,
then match any excessive patterns against that many nils. Also
supports the &rest pattern to match the remaining elements."
  ;; Basic idea: instead of trying to match the sequence, build a new sequence by
  ;; taking as many elements from the original as there're patterns. If the
  ;; sequence has fewer elements than the patterns, simply fill with nils. Now
  ;; match patterns against that newly built seq.
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
          ;; rest: empty if seq was shorter than patterns
          (seq-subseq seq took))))


;; NOTE As I only just discovered seq patterns in Clojure's multi-head defn subtly
;; differ from those say in let-context: in defn you often want to dispatch based
;; on the length of the sequence i.e. how many elements your []-pattern attempts
;; to match, so blindly using mu-pattern [] would almost always have the very
;; first pattern succeed. What we want in the `mu-defun' is to have a strict
;; seq-pattern, where the pattern with the best matching arrity suceeds. One way
;; to do it is to re-write [] in our defuns into a `lv' seq-pattern below. Another
;; way is to re-write [] into (or (l pats) (v pats)). Computationally IMO `lv' has
;; less overhead, since the (or .. ..) would have to expand two almost identical
;; patterns. I think Clojure's defn is even more limiting. It is actually an fixed
;; arrity function with optional &rest pattern but only where such pattern is of
;; bigger arrity than every other fixed pattern:
;;
;;   (mu-defun foo (&rest args)
;;     ([a b c] ...)
;;     ;; is not allowed in Clojure: pat with &rest has to be of bigger arrity
;;     ;; than every other dispatch pattern
;;     ([a b &rest c] ...))
;;
;; neither is the following overloading of the same arrities is allowed:
;;
;;   (mu-defun foo (&rest args)
;;     ([a [b c] d] (list a b c d))
;;     ([a [b]   c] (list a b c)))
;;
;; I actually think this latter case is unreasonable. Btw if I am to allow such
;; dispatch, I'd need all [] acting as strict seq-patterns, not just the outer
;; ones. This implies that [] needs to be dynamic somehow - parameterized by the
;; type of seq-pattern I want it to become after the re-write in `mu--pat-unquoted'
;; and `mu--pat-backquoted'.
;;
;; Curiously, same could be desired whenever we dispatch by matching a sequence
;; rathen than simply bind by destructuring: I doubt we ever want the very first
;; pattern to shadow whatever follows, so similar semantics maybe prefered in
;; `mu-case' but not in `mu-let' or the simple one-armed `mu-defun', where the
;; latter two only destructure and bind, not choose a pattern to dispatch.
(mu-defpattern lv (&rest patterns)
  "`mu-case' pattern to match lists and vectors alike. Unlike
seq-pattern [] it is strict and behaves like l-pattern for lists
or v-pattern for vectors: must match the entire sequence to
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


(mu-defpattern l (&rest patterns)
  "`mu-case' list-pattern that allows &rest matching"
  ;; Basic idea: keep splitting PATTERNS at &rest and recursing into chunks. Chunk
  ;; with no &rest should produce a lst-pattern to break recursion.
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
  "`mu-case' vector-pattern that allows &rest matching"
  ;; Basic idea: just like l-pattern
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


(defmacro mu-case (e &rest clauses)
  "`pcase'-like matching and destructuring with less noise.
Sequence pattern [] is strict: must match the entire sequence to
succeed."
  (declare (indent 1)
           (debug (form &rest (mu-pat body))))

  ;; TODO should I only treat the outer-most [] as lv-pattern, but have permissive
  ;; [] in the internal patterns here?

  ;; Overload [] seq-pattern to be strict: [] will use lv-pattern
  `(mu--case lv ,e ,@clauses))


;;* mu-let ------------------------------------------------------- *;;


(defcustom mu-let-parens 'yes
  "Controls if `mu-let' shoud have a set of parens around each
binding clause like normal `let': 'yes (default), 'no, 'square -
no extra parens, but the entire set of bindings must be inside []
instead of ()."
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
  "Like `let*' but allow mu-case patterns to bind variables"
  (declare (indent 1)
           (debug ((&rest (sexp form)) body)))
  (condition-case err
      (mu--let (mu--let-bindings bindings) body)
    (mu-error `(mu-error ,(cadr err)))))


(defmacro mu-when-let (bindings &rest body)
  "Like `when-let*' but allow mu-case patterns to bind variables"
  (declare (indent 1)
           (debug ((&rest (sexp form)) body)))
  (condition-case err
      (mu--when-let (mu--let-bindings bindings) body)
    (mu-error `(mu-error ,(cadr err)))))


(defmacro mu-if-let (bindings then-body &rest else-body)
  "Like `if-let*' but allow mu-case patterns to bind variables"
  (declare (indent 2)
           (debug ((&rest (sexp form)) form body)))
  (condition-case err
      (mu--if-let (mu--let-bindings bindings) then-body else-body)
    (mu-error `(mu-error ,(cadr err)))))


;;* mu-defun ----------------------------------------------------- *;;


(defun mu--defun-meta (body &optional map)
  "Return a hash-table of attributes parsed from `mu-defun' or
`mu-defmacro' preamble"
  (default map :to (ht))
  (cond
   ((keywordp (car body)) (ht-set map (pop body) (pop body)) (mu--defun-meta body map))
   (:else                 (ht-set map :body body) map)))


(defun mu--defun-sig (split-args body &optional doc sig sigs)
  "Create a docstring from DOC adding signature SIG if supplied
and extra signatures either supplied or generated from the
arglist and `mu-case' patterns in the BODY."
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
  "Mu-clause is a `mu-case' clause: ([patterns] rest)"
  (and (listp clause)
       (not (null clause))
       (or (vectorp (car clause))
           (equal (car clause) 'otherwise))))


(defun mu--defun-clauses? (body)
  "Mu-clauses is just a list of `mu-case' clauses"
  (and (listp body)
       (not (null body))
       (every #'mu--defun-clause? body)))


(defun mu--defun (fun-type name arglist docstring attrs body)
  "Does all the heavy lifting to process the code from mu-defun
or mu-defmacro: extract metadata, normalize arglist and body and
recursively call itself to generate actual defun or defmacro
respectively. See `mu-defun' for what can appear in ARGLIST and
ATTRS"

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

  ;;  Now BODY is expected to be one of:
  ;; - list of mu-clauses when expanding a multi-headed defun,
  ;; - anything at all when expanding a single-headed defun.

  ;; Decide how to proceed by dispatching on the ARGLIST that should match one the
  ;; allowed calling conventions, modify arglist and body as needed and recurse so
  ;; that the penultimate case can deal with now normalized arguments and generate
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

                ;; all []-patterns are strict for multi-head defun, to fascilitate
                ;; dispatch and "delegate to another case by recursion" pattern
                ;; i.e. favour dispatch over destructuring
                `(mu-case ,rest-arg
                   ,@body
                   ;; default otherwise clause if not supplied by the user
                   ,@(unless (some (lambda (clause) (equal (car clause) 'otherwise)) body)
                       (list
                        `(otherwise
                          (mu-error "no matching clause found for mu-defun call %s" ',name))))))))
       ;; expected body of mu-clauses
       `(mu-error "in mu-defun malformed body: expected mu-clauses, got %S" ',body)))

    ;; ARGLIST: unrecognized
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


(comment
 ;; TODO Idea for how mu-defun should really work
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
  (mu-case clause
    ((l 'otherwise | _) t)
    (otherwise nil)))


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


;; TODO Even with the main case extracted into an external helper-function and
;; therefore no recursion this brings `byte-compile' to its knees. I'd like to
;; find out why, but meanwhile we could just byte-compile the `mu--defsetter'.
;; Compiling `mu--split-*' functions makes the biggest difference anyway.
;;
;; OMG Mx pp-macroexpand-all-last-sexp produces 160K loc! That would explain why
;; the compiler runs for 10min ending in bytecode overflow.
;;
;; Current hypothesis: I'm matching on seq, which introduces branching for lists
;; and vectors, and my guess is the pcase simply generates all permutations. Of
;; course the full expansion turns scary very quickly. Eval however runs fine,
;; simply because the interpeter quickly converges on the matching branch and
;; "branches not taken" simply never get to exist. This is fascinating.
;;
;; What to do? Well, either ditch the seq-pattern or find a way to avoid
;; branching. Maybe generate branches explicitly instead of generating or-pattern
;; like (or list-pat vector-pat). Latter would only work if my hypothesis is
;; correct. If it isn't just or-pattern but also the number of pcase-clauses then
;; the latter solution wouldn't make a difference.
;;
;; Ok, blaming branching on (or .. ..) was wrong:
;;
;; 121 loc
;;   (mu-case e
;;     ((lst a (or [0] (pred numberp))) a))
;;   =>
;;   (pcase e
;;     (`(,a ,(or (or `(0) `[0]) 42)) a))
;;
;; 121 loc even if  i rewrite all or-patterns into explicit clauses
;;   (mu-case e
;;     ((lst a (lst 0)) a)
;;     ((lst a (vec 0)) a)
;;     ((lst a 42) a))
;;   =>
;;   (pcase e
;;     (`(,a (0)) a)
;;     (`(,a [0]) a)
;;     (`(,a 42) a))
;;
;; Also see my question on SE:
;; https://emacs.stackexchange.com/questions/46622/byte-compiling-in-presence-of-pcase-patterns
;;
;; TODO I think I came up with a better idea, which I feel should've been part of
;; `pcase` core to begin with. It may have perf implications I don't realise of
;; course:
;;
;;   (mu-case e
;;     ([pat1] body1)
;;     ([pat2] body2)
;;     ([pat3] body3)
;;     (otherwise body))
;;   =>
;;   (mu-case e
;;     ([pat1] body1)
;;     (otherwise (mu-case e
;;                  ([pat2] body2)
;;                  (otherwise (mu-case e
;;                               ([pat3] body3)
;;                               (otherwise body))))))
;;
;; Rewriting it this way collapses the expansion of `mu-defsetter' from 169K loc,
;; to mere 1500 loc!

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
   `(mu-error "in mu-defsetter malformed arglist or body")))


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


;; HACK to debug setters. Interplay with Edebug is intricate and this can have
;; unexpected behavior if the user does something out of the ordinary. Guess it's
;; fine for now


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

  ;; ARGS: unrecogsized
  (otherwise
   `(mu-error "in mu-lambda malformed arglist or body")))


(defalias 'μ 'mu)


;;* provide ------------------------------------------------------ *;;


(provide 'multi-patterns)
