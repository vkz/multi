;; -*- lexical-binding: t; -*-


(require 'cl)

;; TODO (declare (debug ..))

;; TODO Should I alias mu-case as (mu ...)? Would make it occupy less space and
;; hopefully foster more frequent use?

;; TODO byte-compile and test for time

;; NOTE Although `mu--pat-unquoted' and `mu-case--inside' are superficially the
;; same we need both because `mu-case--inside' assumes to be inside backquoted
;; pattern e.g. `(,foo ,bar) in pcase syntax or [foo bar] in our dsl , while
;; `mu--pat-unquoted' assumes to work either outside of a backquoted context or
;; inside of an unquoted pattern.


;;* Prelude ------------------------------------------------------- *;;


;; TODO (ht), other prelude.el that's used


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

(comment

 ;; NOTE Dash's `-split-when' may on occasion be a tiny bit faster, because it
 ;; uses a destructive `!cdr' to update the list in a while loop. If you
 ;; macro-expand my cl-loop above u'd see the body that's almost exactly like
 ;; -split-when and in fact I could re-write the above my loop to be 100% like the
 ;; -split-when except the !cdr part but it'd make it less readable.

 (byte-compile 'mu--split-when)
 (byte-compile '-split-when)

 (list
  (mu-test-time
    (dotimes (_ 1000)
      (list
       (mu--split-when #'mu--rest? '(a b &rest c d &rest e f))
       (mu--split-when #'mu--rest? '(&rest c d &rest e f))
       (mu--split-when #'mu--rest? '(a b &rest c d &rest))
       (mu--split-when #'mu--rest? '())
       (mu--split-when #'mu--rest? '(&rest))
       (mu--split-when #'mu--rest? '(a b)))))


  (mu-test-time
    (dotimes (_ 1000)
      (list
       (-split-when #'mu--rest? '(a b &rest c d &rest e f))
       (-split-when #'mu--rest? '(&rest c d &rest e f))
       (-split-when #'mu--rest? '(a b &rest c d &rest))
       (-split-when #'mu--rest? '())
       (-split-when #'mu--rest? '(&rest))
       (-split-when #'mu--rest? '(a b))))))

 ;; comment
 )


;;* mu-error ------------------------------------------------------ *;;


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


;;* mu-patterns --------------------------------------------------- *;;


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
      `(pcase ,e
         ,@(mapcar (lambda (clause) (mu-case--clause seq-pat clause)) clauses))
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


;;* mu-defpattern ------------------------------------------------- *;;


;; NOTE All we do here is wrap user "macro" into a (lambda (ARGLIST) BODY) and
;; store it in the hash-table stored in mu-case's plist under :mu-patterns slot.
;; Whenever `mu-case' encounters a (NAME PATTERNS) pattern it looks up the NAME in
;; its :mu-patterns property and calls (apply (lambda (ARGLIST) BODY) PATTERNS)
;; which must produce a valid mu-case pattern. So, techincally the user doesn't
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
macro code.

\(fn NAME ARGLIST &optional DOCSTRING &rest BODY)"
  (declare (doc-string 3) (indent 2))
  (when docstring
    (unless (stringp docstring)
      (setq body (cons docstring body))))
  (let ((mu-patterns `(or (get 'mu--case :mu-patterns)
                          (put 'mu--case :mu-patterns (ht))))
        ;; TODO should I trap errors here to fascilitate debugging and better
        ;; error reporting? I could wrap the body in condition-case
        (pattern-macro `(lambda ,arglist ,@body)))
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


;; TODO As I only just discovered seq patterns in Clojure's multi-head defn subtly
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


;;* mu-case ------------------------------------------------------- *;;


(defmacro mu-case (e &rest clauses)
  "`pcase'-like matching and destructuring with less noise.
Sequence pattern [] is strict: must match the entire sequence to
succeed."
  (declare (indent 1))

  ;; TODO should I only treat the outer-most [] as lv-pattern, but have permissive
  ;; [] in the internal patterns here?

  ;; Overload [] seq-pattern to be strict: [] will use lv-pattern
  `(mu--case lv ,e ,@clauses))


;;* mu-let -------------------------------------------------------- *;;


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
  (declare (indent 1))
  (condition-case err
      (mu--let (mu--let-bindings bindings) body)
    (mu-error `(mu-error ,(cadr err)))))


(defmacro mu-when-let (bindings &rest body)
  "Like `when-let*' but allow mu-case patterns to bind variables"
  (declare (indent 1))
  (condition-case err
      (mu--when-let (mu--let-bindings bindings) body)
    (mu-error `(mu-error ,(cadr err)))))


(defmacro mu-if-let (bindings then-body &rest else-body)
  "Like `if-let*' but allow mu-case patterns to bind variables"
  (declare (indent 2))
  (condition-case err
      (mu--if-let (mu--let-bindings bindings) then-body else-body)
    (mu-error `(mu-error ,(cadr err)))))


;;* mu-defun ------------------------------------------------------ *;;


;; TODO re-write `mu-defun' functions with `mu-let'


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


;; TODO should I allow these signatures?
;;   (mu-defun foo        "docstring" ([pat2] body2))
;;   (mu-defun foo args   "docstring" ([pat1] body1))
;;   (mu-defun foo (args) "docstring" ([pat1] body1))


(defun mu--defun (fun-type name arglist body)
  (let ((docstring ""))

    ;; extract docstring if present
    (when (stringp (car body))
      (setq docstring (car body)
            body (cdr body)))

    ;; collect attributes
    (mu-let (((ht sig ret sigs debug test (:declare dspec) (:interactive ispec) body)
              (mu--defun-meta body))
             (simple-fun?          (vectorp arglist))
             (args                 (gensym "args"))
             (pattern              arglist)
             (arglist              (if simple-fun? `(&rest ,args) arglist))
             (split-args           (mu--split-when #'mu--rest? arglist))
             ([head-args [rest-arg]] split-args))

      ;; TODO :debug and :test

      `(,fun-type
        ,name ,arglist
        ,(mu--defun-sig split-args body docstring sig sigs)
        ,@(when dspec `((declare ,@dspec)))
        ,@(when ispec `((interactive ,@(if (equal 't ispec) '() (list ispec)))))

        ;; TODO decide whether I want permissive matching for internal []-patterns
        ,(if simple-fun?

             ;; re-write outermost []-pattern to be strict, so we catch arrity
             ;; bugs, but treat any internal []-pattern as permissive `seq'
             `(mu--case seq ,rest-arg
                ((lv ,@(seq-into pattern 'list)) ,@body)
                (otherwise
                 (mu-error "no matching clause found for mu-defun call %s" ',name)))

           ;; all []-patterns are strict for multi-head defun, to fascilitate
           ;; dispatch and delegation to self "pattern"
           `(mu-case ,rest-arg
              ,@body
              ,@(unless (some (lambda (clause) (equal (car clause) 'otherwise)) body)
                  (list
                   `(otherwise
                     (mu-error "no matching clause found for mu-defun call %s" ',name))))))))))



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
  "Create a lambda-like anonymous function, but use an [pats]
sequence PATTERN in place of an arglist to match and destructure
the incomming argument list"
  :declare ((indent 1))

  ;; (mu [patterns] body)
  ([(v &rest patterns) | body]
   (mu--single-head-lambda (seq-into patterns 'list) body))

  ;; (mu arglist clauses)
  ([(and (pred symbolp) id) | clauses]
   (mu--multi-head-lambda `(&rest ,id) id clauses))

  ;; (mu (args) clauses)
  ([(lst (and (pred symbolp) id)) | clauses]
   (mu--multi-head-lambda `(&rest ,id) id clauses))

  ;; (mu (&rest args) clauses)
  ([(lst (pred mu--rest?) (and (pred symbolp) id)) | clauses]
   (mu--multi-head-lambda `(&rest ,id) id clauses))

  ;; (mu (a b &rest args) clauses)
  ([(and (pred (lambda (arg) (some #'mu--rest? arg))) arglist) | clauses]
   (mu-let ((split-args (mu--split-when #'mu--rest? arglist))
            ([head-args [rest-arg]] split-args))
     (mu--multi-head-lambda arglist rest-arg clauses))))


;; TODO is this a good idea?
(defalias 'μ 'mu)


(comment
 (funcall (mu [a b | args] (list* a b args)) 1 2 3 4)

 (funcall (mu args
            ([a b] (list a b))
            ([a b c] (list a b c)))
          1 2)

 (funcall (mu args
            ([a b] (list a b))
            ([a b c] (list a b c)))
          1 2 3)
 ;; comment
 )


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


(defun mu--set-defun-docstring (fun-type)
  "Sets docstring for `mu-defun' or `mu-defmacro'"
  (put (sym "mu-" fun-type) 'function-documentation
       (format
        "Like `%s' but with multiple clauses. Each clause
specifies a `mu-case' pattern to match against the &rest part of
the ARGLIST followed by the body to run if the match succeeds.
Clauses are tried in order as if one had multiple definitions of
the same function NAME. METADATA can be supplied as :attribute -
expression pairs before the BODY:

  (mu-%s foo (arg &rest args)
    :doc         \"docstring\"
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
  (mu--defun 'defun name arglist body))


(defmacro mu-defmacro (name arglist &rest body)
  (declare (indent 2))
  (mu--defun 'defmacro name arglist body))


;; add docstring to `mu-defun'
(mu--set-defun-docstring 'defun)


;; add docstring to `mu-defmacro'
(mu--set-defun-docstring 'defmacro)


;;** - setter ----------------------------------------------------- *;;


(defmacro mu-defun-setter (call-pattern val &rest clauses)
  (declare (indent 2))
  (let* ((id          (car call-pattern))
         (arglist     (cdr call-pattern))
         (split-args  (mu--split-when #'mu--rest? arglist))
         (head-args   (car split-args))
         (rest-arg    (car (cadr split-args))))
    `(gv-define-setter ,id (,val ,@arglist)
       (mu-case ,rest-arg
         ,@clauses))))


(defalias 'mu-defmacro-setter 'mu-defun-setter)


(comment
 (mu-defun-setter (foo a b &rest args) val
   ([c d] `(setf foo 1))
   ([c] `(setf foo 2)))
 ;; =>
 (gv-define-setter foo (val a b &rest arglist)
   (mu-case arglist
     ([c d] body1)
     ([c] body2)
     (otherwise (multi-error "no setter for %S" arglist))))
 ;; comment
 )


;;** - simple-setter ---------------------------------------------- *;;


(defmacro mu-defun-simple-setter (call-pattern val &rest body)
  (declare (indent 2))
  (let* ((id          (car call-pattern))
         (pattern     (cdr call-pattern))
         (rest-arg (gensym "rest-arg")))
    `(gv-define-setter ,id (,val &rest ,rest-arg)
       (mu-case ,rest-arg
         ([,@pattern] ,@body)))))


(defalias 'mu-defmacro-simple-setter 'mu-defun-simple-setter)


(comment
 ;; arguments form an implicit [a b | args] pattern
 (mu-defun-simple-setter (foo a b | args) val
   `(setf (get foo ...) ,val))
 ;; =>
 (gv-define-setter foo (val &rest arglist)
   (mu-case arglist
     ([a b | args] body)
     (otherwise (multi-error "no setter for %S" arglist))))
 ;; comment
 )


;;* Provide ------------------------------------------------------- *;;


(provide 'multi-patterns)
