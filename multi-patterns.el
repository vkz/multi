;; -*- lexical-binding: t; -*-


(require 'cl)


;; TODO We match lists with [], vectors with '[], sequences with (seq ...). Since
;; seq-pattern works for both, a reasonable question to ask is why not match
;; sequences with []? So, something like that:
;;
;; - sequence with [],
;; - list     with (list ...), or (l ...)
;; - vector   with (vec ...),  or (v ...).
;;
;; Is this more concise? More natural?

;; TODO (mu-defpattern vec (&rest patterns)) and probably remove '[] from mu--case

;; TODO &rest pattern for vectors

;; TODO Should I alias mu-case as (mu ...)? Would make it occupy less space and
;; hopefully foster more frequent use?

;; TODO Also allow | instead of &rest, which gets much too heavy when matching
;; alists

;; TODO we can redeem sequential nature of pattern matching so it works for list
;; and vectors alike simply by replacing any seq pattern with an app-pattern like
;; so (app (fn (v) (seq-into v 'list))). Probably results in some overhead, so
;; make it optional? Looks like pcase has a (seq pat ...) but it has funky
;; semantics in the way it handles seq tails. We could allow it or build on it.

;; NOTE Although `mu-case--init' and `mu-case--inside' are superficially the
;; same we need both because `mu-case--inside' assumes to be inside backquoted
;; pattern e.g. `(,foo ,bar) in pcase syntax or [foo bar] in our dsl , while
;; `mu-case--init' assumes to work either outside of a backquoted context or
;; inside of an unquoted pattern.


;;* mu-error ------------------------------------------------------ *;;


(define-error 'mu-error "mu-error")


(defun mu-error (&rest args)
  "Signals errors specific to `multi' library. Can be caught with
'mu-error ERROR-SYMBOL in `condition-case', otherwise behaves
exactly like `error'

\(fn string &rest args)"
  (signal 'mu-error (list (apply #'format-message args))))


;;* mu-case ------------------------------------------------------- *;;


(defmacro mu-case (e &rest clauses)
  "`pcase' like matching and destructuring with less noise."
  (declare (indent 1))
  ;; NOTE without this prop being set I get "eager macro expansion failure", I
  ;; don't really understand why the code here runs on load file but whatever.
  (unless (get 'mu-case :mu-patterns)
    (define-symbol-prop 'mu-case :mu-patterns (ht)))
  (condition-case err
      `(pcase ,e
         ,@(mapcar #'mu-case--clause clauses))
    (mu-error `(mu-error ,(cadr err)))))


(cl-defun mu-case--clause ((pat . body))
  `(,(mu-case--init pat) ,@body))


(defun mu-case--init (pat)
  "Generate a pcase pattern from a mu-case pattern assuming an
unquoted context."
  (pcase pat
    ('otherwise               pat)
    ((pred symbolp)           pat)
    ;; list pattern
    (`(lst . ,_)              (list '\` (mu-case--inside pat)))
    ;; vector pattern
    (`(vec . ,_)              (list '\` (mu-case--inside pat)))
    ;; seq pattern
    ((pred vectorp)           (mu-case--init `(seq ,@(seq-into pat 'list))))
    (`(or . ,pats)            (cons 'or (mapcar #'mu-case--init pats)))
    (`(and . ,pats)           (cons 'and (mapcar #'mu-case--init pats)))
    (`(app ,fun ,pat)         (list 'app fun (mu-case--init pat)))
    (`(let ,pat ,exp)         (list 'let (mu-case--init pat) exp))
    (`(quote ,(pred symbolp)) pat)
    ;; registered mu-case-pattern
    (`(,(and id (pred symbolp))
       .
       ,pats)                  (if-let ((macro
                                         (ht-get
                                          (get 'mu-case :mu-patterns)
                                          id)))
                                   ;; known mu-case-pattern: expand, recurse
                                   (mu-case--init (apply macro pats))
                                 ;; unknown pattern: do nothing
                                 pat))
    ((pred listp)             pat)
    ((pred atom)              pat)
    (otherwise
     (mu-error "in mu-case unrecognized pattern %S" pat))))


(defun mu-case--inside (pat)
  "Generate a pcase pattern from a mu-case pattern assuming an
quoted context i.e. a list matching pattern."
  (pcase pat
    ('() '())
    ;; match empty list
    (`(lst) '())
    ;; match empty vector
    (`(vec) [])
    ;; TODO replace `-split-on' with some cl- combo
    (`(lst . ,pats) (if (memq '&rest pats)
                        (mu-error "in mu-case lst-pattern doesn't support &rest, use l-pattern instead in: %S" pats)
                      (mapcar #'mu-case--inside pats)))
    (`(vec . ,pats) (if (memq '&rest pats)
                        ;; vector pattern always has pre-defined length, so no &rest support
                        (mu-error "in mu-case vec-pattern doesn't support &rest, use v-pattern instead in: %S" pats)
                      (seq-into (mapcar #'mu-case--inside pats) 'vector)))
    ((pred vectorp) (list '\, (mu-case--init `(seq ,@(seq-into pat 'list)))))
    (`(quote ,(pred symbolp)) (cadr pat))
    ((pred keywordp)          pat)
    ((pred symbolp)           (list '\, pat))
    ;; TODO do I need to check for an empty list here?
    ((pred listp)             (list '\, (mu-case--init pat)))
    ((pred atom)              pat)
    (otherwise
     (mu-error "in mu-case unrecognized pattern %S" pat))))


;;* mu-defpattern ------------------------------------------------- *;;


;; NOTE All we do here is wrap user macro into a (lambda (ARGLIST) BODY) and store
;; it in the hash-table in mu-case's plist under :mu-patterns slot. Whenever
;; `mu-case' encounters a (NAME PATTERNS) pattern it looks up the NAME in its
;; :mu-patterns property and calls (apply (lambda (ARGLIST) BODY) PATTERNS)
;; which must produce a valid mu-case pattern. So, techincally the user doesn't
;; really define a macro so much as a function that takes patterns and must
;; generate a mu-case pattern, that is `mu-case' effectively plays the role
;; of a "macro expander" by simply invoking the function the user provided at
;; compile time.
(defmacro mu-defpattern (name arglist &optional docstring &rest body)
  "Define a new kind of mu-case PATTERN. Patterns of the
form (NAME &rest PATTERNS) will be expanded by this macro with
PATTERS bound according to the ARGLIST. Macro expansion must
produce a valid `mu-case' pattern. The macro is allowed to
throw `mu-error' to signal improper use of the pattern. This
will be handled correctly to inform the user. Optional DOCSTRING
maybe supplied for the convenience of other programmers reading
your macro code.

\(fn NAME ARGLIST &optional DOCSTRING &rest BODY)"
  (declare (doc-string 3) (indent 2))
  (when docstring
    (unless (stringp docstring)
      (setq body (cons docstring body))))
  (let ((mu-patterns `(or (get 'mu-case :mu-patterns)
                          (put 'mu-case :mu-patterns (ht))))
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
     (mu-error "in mu-case malformed ht pattern in %S" patterns))))


;; TODO this should probably be called t-pattern, not ht-pattern
(mu-defpattern ht (&rest patterns)
  "`mu-case' pattern to match hash-tables and alists. ht
expects to receive key-patterns that would be used to lookup and
bind corresponding values in the hash-table or alist being
matched. Possible key-patterns are:

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
  "Controls if seq `mu-case' pattern should always produce
subsequences of type 'list even if they resulted from matching a
subsequence of a vector. Set it to 'list if you don't care
whether you're matching lists or vectors and want any pattern
variable bound to subsequence to be a list."
  :options '(list nil))


(defun mu--seq-split-and-pad (seq pat-len)
  (let* ((type (case (type-of seq)
                 (cons 'list)
                 (vector 'vector)
                 (otherwise (mu-error
                             "in mu-case seq pattern applied to unrecognized type %s"
                             (type-of seq)))))
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
  ;; match patterns against that newly built
  (if patterns
      (let* ((split (-split-on '&rest patterns))
             (head (car split))
             (rest (cadr split))
             (rest? (when rest t))
             (pat-len (length (if rest? head patterns))))
        (when (> (length rest) 1)
          (mu-error "in mu-case malformed &rest pattern %S" rest))
        (if rest?
            `(app (lambda (seq) (mu--seq-split-and-pad seq ,pat-len))
                  (lst (or (lst ,@head) (vec ,@head)) ,@rest))
          `(app (lambda (seq) (car (mu--seq-split-and-pad seq ,pat-len)))
                (or (lst ,@head) (vec ,@head)))))
    ;; empty seq-pattern
    `(or (lst) (vec))))


(mu-defpattern l (&rest patterns)
  "`mu-case' pattern to match vectors but allows &rest matching."
  ;; Basic idea: keep splitting PATTERNS at &rest and recursing into chunks. Chunk
  ;; with no &rest should produce a vec-pattern to break recursion.
  (let* ((split (-split-on '&rest patterns))
         (head (car split))
         (rest (cadr split))
         (rest? (when rest t))
         (pat-len (length (if rest? head patterns))))
    (when (> (length rest) 1)
      (mu-error "in mu-case malformed &rest pattern %S" rest))
    (if rest?
        `(and (pred listp)
              (app (lambda (l) (seq-take l ,pat-len))
                   ,(if (null head)
                        ;; no patterns before &rest or at all - match with empty lst
                        `(lst)
                      ;; shove head patterns into a l-pattern and match against that
                      `(l ,@head)))
              (app (lambda (l) (seq-subseq l ,pat-len)) ,@rest))
      `(and (pred listp)
            (lst ,@head)))))


(mu-defpattern v (&rest patterns)
  "`mu-case' pattern to match vectors but allows &rest matching."
  ;; Basic idea: keep splitting PATTERNS at &rest and recursing into chunks. Chunk
  ;; with no &rest should produce a vec-pattern to break recursion.
  (let* ((split (-split-on '&rest patterns))
         (head (car split))
         (rest (cadr split))
         (rest? (when rest t))
         (pat-len (length (if rest? head patterns))))
    (when (> (length rest) 1)
      (mu-error "in mu-case malformed &rest pattern %S" rest))
    (if rest?
        `(and (pred vectorp)
              (app (lambda (v) (seq-take v ,pat-len))
                   ,(if (null head)
                        ;; no patterns before &rest or at all - match with empty vec
                        `(vec)
                      ;; shove head patterns into a v-pattern and match against that
                      `(v ,@head)))
              (app (lambda (v) (seq-subseq v ,pat-len)) ,@rest))
      `(and (pred vectorp)
            (vec ,@head)))))


;;* mu-let -------------------------------------------------------- *;;


(defcustom mu-let-parens 'yes
  "Controls if `mu-let' shoud have a set of parens around each
binding clause like normal `let': t (default) - yes, nil - no,
square - no extra parens, but the entire set of bindings must be
in [] instead of ()."
  :options '(yes no square))


(defun mu--let (bindings body)
  (let* ((pair (car bindings))
         (pat (car pair))
         (val (cadr pair)))
    (cond
     (pair (unless (= 2 (length pair))
             (mu-error "in mu-let malformed binding list in %S" pair))
           `(mu-case ,val
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
             (mu-error "in mu-when-let malformed binding list in %S" pair))
           `(mu-case ,val
              (,pat ,(mu--when-let (cdr bindings) body))))
     (:else
      `(progn ,@body)))))


(defun mu--if-let (bindings then-body else-body)
  (let* ((pair (car bindings))
         (pat (car pair))
         (val (cadr pair)))
    (cond
     (pair (unless (= 2 (length pair))
             (mu-error "in mu-if-let malformed binding list in %S" pair))
           `(mu-case ,val
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
  "Like `let*' but allows mu-case patterns in place of
identifiers being bound."
  (declare (indent 1))
  (condition-case err
      (mu--let (mu--let-bindings bindings) body)
    (mu-error `(mu-error ,(cadr err)))))


(defmacro mu-when-let (bindings &rest body)
  "Like `when-let*' but allows mu-case patterns in place of
identifiers being bound."
  (declare (indent 1))
  (condition-case err
      (mu--when-let (mu--let-bindings bindings) body)
    (mu-error `(mu-error ,(cadr err)))))


(defmacro mu-if-let (bindings then-body &rest else-body)
  "Like `if-let*' but allows mu-case patterns in place of
identifiers being bound."
  (declare (indent 2))
  (condition-case err
      (mu--if-let (mu--let-bindings bindings) then-body else-body)
    (mu-error `(mu-error ,(cadr err)))))


;;* mu-defun ------------------------------------------------------ *;;


(defun mu--defun-meta (body &optional map)
  "Parses mu-defun and mu-defmacro preamble for atribute
declarations. Returns a hash-table."
  (default map :to (ht))
  (cond
   ((keywordp (car body)) (ht-set map (pop body) (pop body)) (mu--defun-meta body map))
   (:else                 (ht-set map :body body) map)))


(defun mu--defun-sig (split-args body &optional doc sig sigs)
  "Creates a docstring from DOC adding signature SIG if supplied
and extra signatures either supplied or generated from the
arglist and mu-case patterns in the BODY."
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


;; TODO gv-setter
(defun mu--defun (fun-type name arglist body)
  (declare (indent defun))
  (let* ((meta           (mu--defun-meta body))
         (split-args     (-split-on '&rest arglist))
         (head-args      (car split-args))
         (rest-arg       (car (cadr split-args)))
         (body           (ht-get meta :body))
         (doc            (ht-get meta :doc))
         (sig            (ht-get meta :sig))
         (sigs           (ht-get meta :sigs))
         (dspec          (ht-get meta :declare))
         (ispec          (ht-get meta :interactive)))
    (if (or (not rest-arg) (not (symbolp rest-arg)))

        `(mu-error "in mu-defun/macro malformed arglist has no &rest argument in %S"
                   ',arglist)

      `(,fun-type
        ,name ,arglist
        ,(mu--defun-sig split-args body doc sig sigs)
        ,@(when dspec `((declare ,@dspec)))
        ,@(when ispec `((interactive ,@(if (equal 't ispec) '() (list ispec)))))
        ;; TODO what if I wrap the body in condition-case so that any mu-error
        ;; maybe reported in terms of current function e.g. mu-defun or
        ;; mu-defmacro in this case? Wonder how big of an overhead it'd be.
        (mu-case ,rest-arg
          ,@body)))))


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
    :gv-setter   gv-setter-body
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

  :gv-setter - Like gv-setter proprety, see Info
               node `(elisp)Declare Form'. A symbol will will be
               passed to `mu-defun-simple-gv-setter', while a
               form `(lambda (ARG) BODY)' will have access to the
               macro or function's arglist and will be passed to
               `mu-defun-gv-setter'. Just like in `mu-defun' BODY
               must consist of a set of `([pattern] body)'
               clauses.

\(fn NAME ARGLIST METADATA &rest BODY)"
        fun-type fun-type))
  nil)


(defmacro mu-defun (name arglist &rest body)
  (declare (indent 2))
  (mu--defun 'defun name arglist body))


(defmacro mu-defmacro (name arglist &rest body)
  (declare (indent 2))
  (mu--defun 'defmacro name arglist body))


(defmacro mu-defun-setter (name arglist &rest body)
  `(gv-define-setter ,name ,arglist ,@body))


(defmacro mu-defmacro-setter (name arglist &rest body)
  `(gv-define-setter ,name ,arglist ,@body))


;; add docstring to `mu-defun'
(mu--set-defun-docstring 'defun)


;; add docstring to `mu-defmacro'
(mu--set-defun-docstring 'defmacro)


;;* Provide ------------------------------------------------------- *;;


(provide 'multi-patterns)
