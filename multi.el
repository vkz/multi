;; -*- lexical-binding: t; -*-

(require 'cl)

;; TODO parsing all those defun args has become very tedious. I almost wish I
;; didn't insist on optional keyword args so much. Bulk of the code here does this
;; nonsence. Should drop this and go with the flow, or does Elisp has some hidden
;; way of dealing with them? Or maybe my patterns are repetitive enough that I
;; could abstract them into a macro?

;; TODO Instead of poluting symbol slots maybe I should have a multi struct. This
;; may make code quite a bit cleaner
;;
;;   (defstruct multi
;;     (methods (ht))
;;     default
;;     dispatch)
;;
;; I'll have to overload multi-methods accessor function so it can do what current
;; `multi-methods' can do. One interesting aspect is that the value of foo then
;; becomes a struct and predicate (multi-p foo) works as expected, this however
;; necessitates passing quoted foo where expected, but we could work around this
;; by adding an extra :name or :id or :symbol slot, what would carry 'foo.

;; TODO dispatch cache

;; TODO hierarchy cache

;; TODO Make `multi-test' available here to be used in comments and examples

;; TODO Create Makefile (stick to ANSI make): ert batch test, measure perf

;; TODO Implement `prefer-method' for disambiguation

;; TODO Implement `remove-method'

;; TODO Consider storing hierarchies the way Clojure does it. IMO benefit is that
;; descendants are precalculated. Anything else?
;;   {:parents     {:rect #{:shape}}
;;    :ancestors   {:rect #{shape}}
;;    :descendants {:shape #{rect}}}

;; Extras
;; --------

;; TODO Think about reasonable and practical global-hierarchy, e.g. one that works
;; for structs, isa relationship between predicates, maybe even eieio classes
;; although I have no experienc with those.

;; TODO Allow isa? with "_" patterns

;; TODO Allow predicates in patterns

;; TODO Hierarchy is orthogonal to `multi' dispatch function. However in Clojure
;; you may change it (only?) in `defmulti', but IMO it makes more sence to be able
;; to pass it to multimethod invocations (not even definitions). Need to think if
;; that'd be consistent and whether it has any practical value.

;; TODO Could we allow arbitrary relations? E.g. `parent-of'. Would that have any
;; practical benefit? When? How?

;; TODO Should I overload (multi-rel x relates-to? x) to be used as predicate:
;; (if (multi-rel y parent-of? x) do stuff) or define (multi-rel? ...)?

;; TODO How hard would it be to add body from (example foo body) forms to the
;; docstring of 'foo?


;;* Errors -------------------------------------------------------- *;;


(define-error 'multi-error "multi-error")


(defun multi-error (&rest args)
  "Signals errors specific to `multi' library. Can be caught with
'multi-error ERROR-SYMBOL in `condition-case', otherwise behaves
exactly like `error'

\(fn string &rest args)"
  (signal 'multi-error (list (apply #'format-message args))))


;;* State  -------------------------------------------------------- *;;


(defcustom multi-lexical-binding 'error
  "Control if multimethods can be defined when `lexical-binding'
 is disabled. Default to signaling an error if an attempt is made
 to define a new multi dispatch or method while in a dynamically
 scoped environment.")


(defun multi-lexical-binding ()
  "Signal an error depending on the setting of
`multi-lexical-binding' and `lexical-binding'."
  (when multi-lexical-binding
    (unless lexical-binding
      (multi-error
       (string-join
        (list
         "multimethods require `lexical-binding' to work properly."
         "If you know what you are doing you may disable this check"
         "by unsetting `multi-lexical-binding'.")
        " ")))))


(defstruct multi-hierarchy
  (table (ht))
  (cache (ht)))


(defun multi-hierarchy (hierarchy &rest keys)
  "Returns the value in the HIERARCHY's nested table, where KEYS
is a sequence of keys. Returns nil if the key is not present.
Without KEYS returns the entire table. Calls are `setf'-able."
  (declare
   (gv-setter (lambda (val)
                `(setf (ht-get* (multi-hierarchy-table ,hierarchy) ,@keys) ,val))))
  (if keys
      (apply #'ht-get* (multi-hierarchy-table hierarchy) keys)
    (multi-hierarchy-table hierarchy)))


(defconst multi-global-hierarchy (make-multi-hierarchy)
  "Global table that holds global hierachy. Has the following
structure:

  (ht (VAL (ht (:parents (p ...)) (:children (c ...)))) ...)")


(defun multi-global-hierarchy (&rest keys)
  "Returns the value in the global hierarchy nested table, where
KEYS is a sequence of keys. Returns nil if the key is not
present. Without KEYS returns the entire table. Calls are
`setf'-able."
  (declare
   (gv-setter (lambda (val)
                `(setf (multi-hierarchy multi-global-hierarchy ,@keys) ,val))))
  (apply #'multi-hierarchy multi-global-hierarchy keys))


(defun multi--methods (fun-symbol &rest keys)
  "Returns the multi-methods hash-table of FUN-SYMBOL or the
value nested in that table if the sequence of KEYS is supplied.
This form is `setf'-able."
  (declare
   (gv-setter (lambda (val)
                (if keys
                    ;; with keys set corresponding value in :multi-methods table
                    `(setf (ht-get* (get ,fun-symbol :multi-methods) ,@keys) ,val)
                  ;; without keys set :multi-methods prop itself to a new table
                  `(setf (get ,fun-symbol :multi-methods) ,val)))))
  (if keys
      (apply #'ht-get* (get fun-symbol :multi-methods) keys)
    (get fun-symbol :multi-methods)))


;; TODO the (multi-methods 'fun &rest keys) interface suggests an interesting
;; feature. We could go a bit further than Clojure and allow :before, :after,
;; :arround methods, so the multi-methods table doesn't just maps an isa? pattern
;; to a method but potentionally to a map of methods:
;;
;; (ht (:before #'before-fun)
;;     (:after #'after-fun)
;;     (:main #'main-fun))
;;
;; Can I come up with interesting semantics?


(defun multi-methods (&rest args)
  "When called with :for and :matching keywords returns an alist
of (VALUE . method) pairs where (multi-isa? VAL VALUE)
relationship holds. If no such pairs exist returns an alist of
one (:default . default-method), where default-method is either a
custom user-installed or the pre-installed one. If HIERARCHY not
supplied defaults to the global hierarchy.

When called without :for and :matching keywords returns the
multi-methods hash-table of FUN-SYM or the value nested in that
table if the sequence of KEYS is supplied. The form is
`setf'-able.

May be called according to one of the following signatures:

  (multi-methods :for fun-sym :matching val &optional :in hierarchy)
  (multi-methods fun-sym &rest keys)
  (setf (multi-methods fun-sym &rest keys) val)


\(fn :for fun-sym :matching val &optional :in hierarchy)"
  (declare
   (gv-setter (lambda (val)
                (message "%s" args)
                (pcase args

                  ;; (setf (multi-methods 'foo &rest keys) val)
                  (`((quote ,(multi fun-symbol :if symbolp)) . ,keys)
                   `(setf (multi--methods ',fun-symbol ,@keys) ,val))

                  ;; (setf (multi-methods foo &rest keys) val)
                  (`(,(multi fun-symbol :if symbolp) . ,keys)
                   `(setf (multi--methods ,fun-symbol ,@keys) ,val))

                  (otherwise
                   `(multi-error "in multi-methods malformed arglist at %s" ',args))))))
  ;; parse args
  (pcase args
    ;; (multi-methods :for 'fun :matching val)
    (`(:for ,fun :matching ,val)
     (multi-methods :for fun :matching val :in multi-global-hierarchy))

    ;; (multi-methods :for 'fun :matching val :in hierarchy)
    (`(:for ,fun :matching ,val :in ,hierarchy)
     (let* ((multi-methods (get fun :multi-methods))
            (methods
             (-non-nil
              (ht-map
               (fn (VAL method)
                 (and (multi-isa? val VAL hierarchy)
                      (cons VAL method)))
               multi-methods)))
            (default-method
              (unless methods
                (list
                 ;; Use custom :default if installed, fallback to the
                 ;; pre-installed default method otherwise
                 (cons :default (or (ht-get* multi-methods :default)
                                    (get fun :multi-default)))))))
       (or methods default-method)))

    ;; (multi-methods 'fun :rect)
    (`(,(multi fun-symbol :if symbolp) . ,keys)
     (apply #'multi--methods fun-symbol keys))

    (otherwise
     (multi-error "in multi-methods malformed arglist at %s" args))))


;; NOTE this naming `multi-methods-remove' becomes more consistent if or when we
;; allow :before :after :around methods
(defun multi-methods-remove (fun dispatch-value)
  "Removes the multimethod FUN associated with DISPATCH-VALUE."
  (ht-remove! (multi-methods fun) dispatch-value))

;;* Hierarchies --------------------------------------------------- *;;


(defun multi--cycle? (item parent &optional hierarchy)
  "Checks if ITEM and would be PARENT would form a cycle were the
relationship added to the HIERARCHY. If HIERARCHY not supplied
defaults to the global hierarchy"
  (default hierarchy :to multi-global-hierarchy)
  (or (equal item parent)
      (ormap
       (lambda (ancestor) (multi--cycle? item ancestor hierarchy))
       (multi-hierarchy hierarchy parent :parents))))


(defmacro multi-rel (&rest args)
  "Establishes an isa? (parent/child) relationship between PARENT
and CHILD. If HIERARCHY not supplied defaults to, and modifies,
the global hierarchy.

\(fn child :isa parent &optional :in hierarchy)"
  (destructuring-bind
      (err child parent hierarchy)
      (pcase args
        (`(,child ,(or 'isa :isa) ,parent) (list nil child parent nil))
        (`(,child ,(or 'isa :isa) ,parent ,(or 'in :in) ,hierarchy) (list nil child parent hierarchy))
        (otherwise
         (list `(multi-error "in multi-rel malformed arglist at %s" ',args) nil nil nil)))
    (or
     err
     (let ((hierarchy (or hierarchy 'multi-global-hierarchy)))
       `(progn
          (let ((child ,child)
                (parent ,parent))
            (when (multi--cycle? ,child ,parent ,hierarchy)
              (multi-error "in multi-rel cycle relationship between %s and %s "
                           ,child ,parent))
            (pushnew ,parent (multi-hierarchy ,hierarchy ,child :parents))
            (pushnew ,child (multi-hierarchy ,hierarchy  ,parent :children))
            ,hierarchy))))))


(defun ormap (pred lst)
  (when lst
    (or (funcall pred (car lst))
        (ormap pred (cdr lst)))))


(cl-defun multi--seq-isa? (seqx seqy hierarchy)
  (let ((rels (seq-mapn (lambda (x y) (multi-isa? x y hierarchy)) seqx seqy)))
    (and (cl-notany #'null rels)
         rels)))


(cl-defun multi-isa? (x y &optional (hierarchy) (generation 0))
  "Returns (generation 0) if (equal CHILD PARENT), or (generation
N) if CHILD is directly or indirectly derived from PARENT, where
N signifies how far down generations PARENT is from CHILD. If
HIERARCHY not supplied defaults to the global hierarchy

\(fn child parent &optional hierarchy)"
  (default hierarchy :to multi-global-hierarchy)
  (cond
   ((sequencep x)
    (and (sequencep y)
         (equal (length x) (length y))
         (multi--seq-isa? x y hierarchy)))

   ((sequencep y)
    ;; then x wasn't a seq or failed x isa? y test
    nil)

   ((equal x y)
    (cons :generation generation))

   ((member y (multi-hierarchy hierarchy x :parents))
    (cons :generation (1+ generation)))

   (:else
    (ormap
     (lambda (parent) (multi-isa? parent y hierarchy (1+ generation)))
     (multi-hierarchy hierarchy x :parents)))))


(defun multi-parents (x &optional hierarchy)
  "Returns immediate parents (multi-isa? X PARENT) of X. If
HIERARCHY not supplied defaults to the global hierarchy"
  (default hierarchy :to multi-global-hierarchy)
  (multi-hierarchy hierarchy x :parents))


(defun multi-ancestors (x &optional hierarchy)
  "Returns an immediate and indirect ancestors (multi-isa? X
ANCESTOR) of X. If HIERARCHY not supplied defaults to the global
hierarchy"
  (default hierarchy :to multi-global-hierarchy)
  (let ((parents (multi-hierarchy hierarchy x :parents)))
    (append parents
            (seq-mapcat
             (lambda (parent)
               (multi-ancestors parent hierarchy))
             parents))))


(defun multi-descendants (x &optional hierarchy)
  "Returns an immediate and indirect descendants (multi-isa?
DESCENDANT X) of X. If HIERARCHY not supplied defaults to the
global hierarchy"
  (default hierarchy :to multi-global-hierarchy)
  (let ((children (multi-hierarchy hierarchy x :children)))
    (append children
            (seq-mapcat
             (lambda (child)
               (multi-descendants child hierarchy))
             children))))


;;* Multi --------------------------------------------------------- *;;


;; NOTE on caching. There are two obvious things we can cache.
;;
;; One is the dispatch function with respect to its arguments. Since user can
;; perform all sorts of expensive calculations (even if shouldn't) memoizing
;; dispatch invocations can speed up calculating the value to be used later to
;; perform isa? search in a hierarchy. One possible gotcha here is that the
;; dispatch function must be pure! If the body relies on any external state, then
;; our cache can quite easily be stale, since it is obviously impossible to
;; determine when to invalidate it. Luckily, it should be fairly easy to make any
;; dispatch function pure simply by moving whatever stateful value you're looking
;; up to its arguments, that is look it up before you make a multi-call and call
;; with that extra argument. This may, potentially, lead to another gotcha with
;; respect to concurrency: you want the state lookup and multi-call performed in
;; transaction else you may end up with a race where relevant state gets updated
;; while the dispatch is in flight. This is a very generic comment and may not be
;; relevant to Emacs Lisp - I know nothing at all about its concurrency model -
;; does it ever have one? First, I need to make note about caching in
;; documentation; second, we probably want to let user turn this cache on and off
;; as needed; third, maybe I should allow another attribute in `multi' e.g. (:pure
;; t) so that user can declare that his dispatch is indeed pure and we can cache
;; as needed. Another potential issue with caching is that arguments maybe
;; something difficult to test for equality e.g. anonymous functions. Appears that
;; Emacs Lisp somehow manages to equate (lambda (x) x) with (lambda (x) x) but I
;; suspect it must be a hack that doesn't work in general. Since we are to use
;; args as keys in cache table this lead to said table to explode in size in tight
;; loops when keys are structurally equal, but Elisp can't really know that.
;;
;; The other is caching isa? hierarchy lookup. This implies that each hierarchy
;; needs to keep track of its cache and invalidate it every time a relationship is
;; added or removed. We'll want to change the signature of the `multi-isa?' or
;; introduce another function. New signature should be: (-> value hierarchy
;; result) that is it doesn't take a VALUE to check if (isa? val VALUE) but
;; instead tries every item in the hierarchy. This way we can easily memoize
;; hierarchy isa? checks with respect to just one val argument. Since this cache
;; is per hierarchy, every hierarchy should probably carry its isa? relationship
;; lookup with it. This probably means that hierarchies should be implemented as
;; structs or given a symbolic name so that they can keep such meta information in
;; the plist. IMO struct would be cleaner.


(pcase-defmacro multi (&rest patterns)
  (pcase patterns
    (`(,id :if ,predicate) `(and ,id (pred ,predicate)))
    (otherwise
     `(multi-error "malformed pcase multi pattern"))))


(example
 (pcase '([a b] "doc")
   (`(,(multi arglist :if vectorp) ,(multi doc :if stringp)) (list arglist doc))
   (otherwise
    (error "no match")))
 ;; example
 )


;; TODO Lexical vs dynamic scope. Something I ran into by chance. `multi-tests.el'
;; doesn't have lexical scope on and this has interesting implications for
;; multimethods. Say, this example won't work as expected in dynamic scope:
;;
;;   (let ((hierarchy (ht))
;;         (b 42))
;;     (multi-rel :rect isa :shape in hierarchy)
;;     (multi baz (lambda (x) (princ b) x) :in hierarchy)
;;     (multimethod baz (x) :when :shape :shape)
;;     (baz :rect)
;;     ;; prints  42 and returns :shape as expected
;;     )
;;
;;   ;; but outside of `let' neither variables hierarchy or b are bound due to dynamic
;;   ;; scope
;;
;;   (baz :rect)
;;   ;; => Error symbol value is void 'b
;;
;; Problem here is that although `multi' macro is defined in lexical scope, it
;; expansion may happen in dynamic scope (if the user so chooses or, like me,
;; forgets to enable lexical scope) and then much bafflement follows. E.g.
;; `hierarchy' parameter in the definition of `multi' is assumed to be bound in
;; the surrounding scope, so if that scope is dynamic the defined dispatch
;; function we intorduce doesn't close over `hirarchy', nor yet any other external
;; binding. So, if at any point those bindings go away dispatch goes ka-boom!
;;
;; If my stackoverflow question
;; https://emacs.stackexchange.com/questions/46088/forcing-lexical-scope-in-the-middle-of-dynamic-scope
;; ever gets answered I could check for `lexical-binding' at call site and
;; optionally hijack or force lexical scope for the body of my macro. For the rare
;; case where the user does actually mean dynamic scope, I could control whether
;; to force lexical or not with a variable they could set.
;;
;; At the very minimum I should make a note of this gotcha in documentation.


(defmacro multi (fun &rest args)
  "Creates a new multimethod dispatch function. The DOCSTRING and
HIERARCHY are optional. HIERARCHY if not supplied defaults to the
global hierarchy.

May be called according to one of the following signatures:

  (multi name argvector &optional docstring :in hierarchy body...)
  (multi name function &optional docstring :in hierarchy)

where ARGVECTOR is a vector of arguments that follows full Common
Lisp arglist convention, FUNCTION is any expression that returns
a function.

\(fn name argvector &optional docstring :in hierarchy body...)"
  (declare (indent 2))
  (destructuring-bind
      (err dispatch doc hierarchy)
      (pcase args
        ;; (multi foo [a b &rest args] "doc" :in hierarchy e1 e2)
        (`(,(multi arglist :if vectorp) ,(multi doc :if stringp) :in ,hierarchy . ,body)
         (list nil `(fn ,(seq-into arglist 'list) ,@body) doc hierarchy))

        ;; (multi foo [a b &rest args] :in hierarchy e1 e2)
        (`(,(multi arglist :if vectorp) :in ,hierarchy . ,body)
         (list nil `(fn ,(seq-into arglist 'list) ,@body) "" hierarchy))

        ;; (multi foo [a b &rest args] "doc" e1 e2)
        (`(,(multi arglist :if vectorp) ,(multi doc :if stringp) . ,body)
         (list nil `(fn ,(seq-into arglist 'list) ,@body) doc 'multi-global-hierarchy))

        ;; (multi foo [a b &rest args] e1 e2)
        (`(,(multi arglist :if vectorp) . ,body)
         (list nil `(fn ,(seq-into arglist 'list) ,@body) "" 'multi-global-hierarchy))

        ;; (multi foo fn-returning-expr "doc" :in hierarchy)
        (`(,f ,(multi doc :if stringp) :in ,hierarchy)
         (list nil f doc hierarchy))

        ;; (multi foo fn-returning-expr :in hierarchy)
        (`(,f :in ,hierarchy)
         (list nil f "" hierarchy))

        ;; (multi foo fn-returning-expr "doc")
        (`(,f ,(multi doc :if stringp))
         (list nil f doc 'multi-global-hierarchy))

        ;; (multi foo fn-returning-expr)
        (`(,f)
         (list nil f "" 'multi-global-hierarchy))

        (otherwise
         ;; TODO If we signal an an error immediately no relevant `condition-case'
         ;; would catch it, because IIUC it only traps runtime but we throw at
         ;; macro expansion, so instead I need to generate code that throws.
         ;; Wonder if there is a way to trap compile time errors?
         (list `(multi-error "in multi malformed arglist at %s" ',args) nil nil nil)))
    (or
     err
     `(progn

        ;; check if lexical binding is enabled
        (multi-lexical-binding)

        (defun ,fun (&rest args)
          ,doc
          (let* ((val     (apply (get ',fun :multi-dispatch) args))
                 (methods (multi-methods :for ',fun :matching val :in ,hierarchy))
                 ;; => ((VAL . method) ...)
                 ;; TODO Choose method with prefer method instead of cdar here
                 (method  (cdar methods)))
            (unless (null (cdr methods))
              (multi-error
               "multiple methods match dispatch value %s for dispatch %s:\n%s\n%s"
               val ',fun (string-join
                          (mapcar (fn ((VAL . _)) (format "  %s :isa %s" val VAL)) methods)
                          "\n")
               "and neither is preferred"))
            (apply method args)))

        ;; set fun value slot to return its quoted form, this lets us pass fun
        ;; around as value and still not break functions that expect a symbol
        (setf ,fun ',fun)

        ;; reset dispatch prop to the dispatch function
        (setf (get ',fun :multi-dispatch) ,dispatch)

        ;; reset multi-methods prop to a fresh table with :default pre-installed
        (setf (get ',fun :multi-methods) (ht))

        ;; pre-install default method
        (setf (get ',fun :multi-default)
              (fn (&rest args)
                (multi-error
                 "no multimethods match dispatch value %s for dispatch %s "
                 (apply (get ',fun :multi-dispatch) args)
                 ',fun)))

        ;; TODO Invalidate multi-methods cache here. Need to do this to catch
        ;; cases where fun simply gets redefined and may hold cache for previous
        ;; dispatch function
        ))))


(comment
 ;; Syntax examples
 (multi foo (lambda (a b &rest args) e1 e2) "doc" :in hierarchy)
 (multi foo (lambda (a b &rest args) e1 e2) :in hierarchy)
 (multi foo (lambda (a b &rest args) e1 e2) "doc")
 (multi foo (lambda (a b &rest args) e1 e2))

 (multi foo 'foo-fun "doc" :in hierarchy)
 (multi foo 'foo-fun :in hierarchy)
 (multi foo 'foo-fun "doc")
 (multi foo 'foo-fun)

 (multi foo [a b &rest args] "doc" :in hierarchy e1 e2)
 (multi foo [a b &rest args] :in hierarchy e1 e2)
 (multi foo [a b &rest args] "doc" e1 e2)
 (multi foo [a b &rest args] e1 e2)

 ;; Same but pseudo-coded with &optional and &rest
 (multi foo (lambda (a &rest args) body) &optional "doc" :in hierarchy)
 (multi foo #'dispatch &optional "doc" :in hierarchy)
 (multi foo [a &rest args] &optional "doc" :in hierarchy &rest body)
 ;; example
 )


;; TODO Consider generalizing syntax for installed relationship not just `isa'
;; (multimethod foo (a b) :isa :b body)
;; (multimethod foo (a b) :parent-of :b body)
(cl-defmacro multimethod (fun arglist &rest args)
  "Creates a new multimethod associated with the dispatch
function FUN and dispatch value VAL. ARGLIST follows full Common
Lisp conventions.

\(fn fun arglist :when val body...)"
  (pcase args
    (`(:when ,val . ,body)
     (let ((method `(fn ,arglist ,@body)))
       `(progn

          ;; check if lexical binding is enabled
          (multi-lexical-binding)

          ;; add new method to the multi-methods table
          (setf (ht-get* (get ',fun :multi-methods) ,val) ,method)

          ;; TODO invalidate multi-methods cache
          )))
    (otherwise
     `(multi-error "in multimethod malformed arglist at %s" ',args))))


;;* Playground ---------------------------------------------------- *;;


(comment
 (multimethod foo (&rest args) :when [a b c] body)
 (multimethod foo (&rest args) :when [a b _] body)
 (multimethod foo (&rest args) :when [a (?  pred-p) c] body)

 ;; degenerate case where computed multi val maybe a seq, pred-p should still be
 ;; applied even though this here val isn't a seq
 (multimethod foo (&rest args) :when (?  pred-p) body)
 ;; comment
 )


;;* Provide ------------------------------------------------------- *;;


(provide 'multi)
