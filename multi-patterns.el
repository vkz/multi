;; -*- lexical-binding: t; -*-


(require 'cl)

;; TODO (declare (debug ..))

;; TODO Should I alias mu-case as (mu ...)? Would make it occupy less space and
;; hopefully foster more frequent use?

;; TODO byte-compile and test for time

;; NOTE Although `mu-case--init' and `mu-case--inside' are superficially the
;; same we need both because `mu-case--inside' assumes to be inside backquoted
;; pattern e.g. `(,foo ,bar) in pcase syntax or [foo bar] in our dsl , while
;; `mu-case--init' assumes to work either outside of a backquoted context or
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


;;* mu-case ------------------------------------------------------- *;;


(defmacro mu-case (e &rest clauses)
  "`pcase'-like matching and destructuring with less noise."
  (declare (indent 1))

  ;; storage for patterns defined with `mu-defpattern'
  (unless (get 'mu-case :mu-patterns)
    (define-symbol-prop 'mu-case :mu-patterns (ht)))

  ;; hack to propagate expansion time errors to runtime
  (condition-case err
      `(pcase ,e
         ,@(mapcar #'mu-case--clause clauses))
    (mu-error `(mu-error ,(cadr err)))))


(cl-defun mu-case--clause ((pat . body))
  `(,(mu-case--init pat) ,@body))


(defun mu--rest? (item)
  "Test if the ITEM is a `&rest'-like separator"
  (memq item '(| & &rest)))


(defun mu-case--init (pat)
  "Translate mu-pat into pcase-pat assuming in unquoted context"
  (pcase pat
    ('otherwise               pat)
    ((pred symbolp)           pat)
    ;; list pattern
    (`(lst . ,_)              (list '\` (mu-case--inside pat)))
    ;; vector pattern
    (`(vec . ,_)              (list '\` (mu-case--inside pat)))
    ;; seq pattern
    ((pred vectorp)           (mu-case--init `(seq ,@(seq-into pat 'list))))
    ;; standard pcase patterns
    (`(or . ,pats)            (cons 'or (mapcar #'mu-case--init pats)))
    (`(and . ,pats)           (cons 'and (mapcar #'mu-case--init pats)))
    (`(app ,fun ,pat)         (list 'app fun (mu-case--init pat)))
    (`(let ,pat ,exp)         (list 'let (mu-case--init pat) exp))
    ;; quoted symbol
    (`(quote ,(pred symbolp)) pat)
    ;; mu-defpatterns
    (`(,(and id (pred symbolp)) . ,pats)
     (if-let ((macro (ht-get (get 'mu-case :mu-patterns) id)))
         ;; registered pattern
         (mu-case--init (apply macro pats))
       ;; unknown pattern
       pat))
    ((pred listp)             pat)
    ((pred atom)              pat)
    (otherwise                (mu-error :pattern pat))))


(defun mu-case--inside (pat)
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
                                (mapcar #'mu-case--inside pats)))
    ;; vector pattern
    (`(vec . ,pats)           (if (some #'mu--rest? pats)
                                  (mu-error :vec-pattern pats)
                                (seq-into (mapcar #'mu-case--inside pats) 'vector)))
    ;; seq pattern
    ((pred vectorp)           (list '\, (mu-case--init `(seq ,@(seq-into pat 'list)))))
    ;; quoted symbol
    (`(quote ,(pred symbolp)) (cadr pat))
    ((pred keywordp)          pat)
    ((pred symbolp)           (list '\, pat))
    ;; TODO do I need to check for an empty list here?
    ((pred listp)             (list '\, (mu-case--init pat)))
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
  (let* ((type (case (type-of seq)
                 (cons 'list)
                 (vector 'vector)
                 (otherwise (mu-error :seq-pattern (type-of seq)))))
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
             (pat-len (length (if rest? head patterns))))
        (when (> (length rest) 1)
          (mu-error :rest-pattern rest))
        (if rest?
            ;; match head and rest
            `(app (lambda (seq) (mu--seq-split-and-pad seq ,pat-len))
                  (lst (or (lst ,@head) (vec ,@head)) ,@rest))
          ;; match head only
          `(app (lambda (seq) (car (mu--seq-split-and-pad seq ,pat-len)))
                (or (lst ,@head) (vec ,@head)))))
    ;; empty seq-pattern
    `(or (lst) (vec))))


(defun mu--seq-split (seq pat-len)
  (let* ((subseq (seq-take seq pat-len))
         (took (length subseq)))
    (list subseq
          ;; rest: empty if seq was shorter than patterns
          (seq-subseq seq took))))


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
             (mu-error :let-malformed pair))
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
             (mu-error :let-malformed pair))
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


(defun mu--simple-defun (fun-type name pattern body)
  (let* ((args (gensym "args"))
         (docstring (when (stringp (car body)) (list (car body))))
         (body (if docstring (cdr body) body)))
    `(defun ,name (&rest ,args)
       ,@docstring
       (mu-case ,args
         (,pattern ,@body)
         (otherwise
          (multi-error "in foo: arguments do not satisfy the arglist pattern %S" ,args))))))


(example
 (mu-defun foobar [a b | tail]
   "docstring"
   (list a b tail))
 (foobar 1 2 3)
 ;; example
 )


;; TODO gv-setter
(defun mu--defun (fun-type name arglist body)
  (declare (indent defun))
  (if (vectorp arglist)
      ;; (mu-defun foo [pat] "doc" body)
      (mu--simple-defun fun-type name arglist body)
    ;; (mu-defun foo (arglist) attrs (pat body) ...)
    (let* ((meta           (mu--defun-meta body))
           (split-args     (mu--split-when #'mu--rest? arglist))
           (head-args      (car split-args))
           (rest-arg       (car (cadr split-args)))
           (body           (ht-get meta :body))
           (doc            (ht-get meta :doc))
           (sig            (ht-get meta :sig))
           (sigs           (ht-get meta :sigs))
           (dspec          (ht-get meta :declare))
           (ispec          (ht-get meta :interactive)))
      (if (or (not rest-arg) (not (symbolp rest-arg)))
          `(mu-error :defun-malformed ',arglist)
        `(,fun-type
          ,name ,arglist
          ,(mu--defun-sig split-args body doc sig sigs)
          ,@(when dspec `((declare ,@dspec)))
          ,@(when ispec `((interactive ,@(if (equal 't ispec) '() (list ispec)))))
          ;; TODO what if I wrap the body in condition-case so that any mu-error
          ;; maybe reported in terms of current function e.g. mu-defun or
          ;; mu-defmacro in this case? Wonder how big of an overhead it'd be.
          (mu-case ,rest-arg
            ,@body))))))


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
