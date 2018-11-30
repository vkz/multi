

;; Disable lexical-binding check for now
(setq mu-lexical-binding nil)

;; TODO I leave lexical binding off, cause I suspect my mu-test may on occasion
;; not work, cause it captures stuff lexically. I need to investigate cause I'd
;; rather have lexical scope here, too.


(require 'ert)
(require 'cl-lib)
(require 'multi)

;; TODO Cache

;; TODO Would implementing expect macro as per examples below be worth it?


;;* Prelude ------------------------------------------------------- *;;


(defun mu--symbol-function (s)
  "Like `symbol-function' but returns :unbound when S is
unbound."
  (declare
   (gv-setter (lambda (val)
                `(case ,val
                   ((:unbind :unbound) (fmakunbound ,s))
                   (otherwise          (fset ,s ,val))))))
  (or (symbol-function s) :unbound))


(defmacro mu-test (multis &rest body)
  "Set `mu-global-hierarchy' and `mu-methods' to empty
tables and unbind functions in the MULTIS list for the extent of
BODY, allowing it to bind them as needed. Restore everything
after BODY."
  (declare (indent defun))
  (let ((multis (mapcar (fn (m) `((mu--symbol-function ',m) :unbind)) multis)))
    `(cl-letf ((mu-global-hierarchy (make-mu-hierarchy))
               ,@multis)
       ,@body)))


(example
 (list
  (mu--symbol-function 'bar)
  (mu-test (bar)
    (mu-defmulti bar [a] a)
    (mu--symbol-function 'bar))
  (mu--symbol-function 'bar))
 ;; example
 )


(cl-defmacro mu--error-match (prefix &rest body)
  "Try catching `mu-error' thrown by BODY and test if its
message prefix matches PREFIX"
  `(string-prefix-p
    ,prefix
    (condition-case err
        (progn ,@body)
        (mu-error (cadr err)))
    'ignore-case))


(example
 (should (mu--error-match "foo" (mu-error "foo %s" 'bar)))
 ;; example
 )


(defun mu--set-equal? (s1 s2)
  "Plenty good set-equality for testing"
  (unless (and (listp s1) (listp s2))
    (mu-error
     "in mu--set-equal? expected list arguments, but got %s and %s"
     s1 s2))
  (and (null (cl-set-difference s1 s2 :test #'equal))
       (null (cl-set-difference s2 s1 :test #'equal))))

(example
 (mu--set-equal? (list 1 2 3) (list 3 2 1))
 (mu--set-equal? (list 1 2 3) (list 3 2))
 (mu--set-equal? '() (list 1 2 3))
 ;; example
 )


(defmacro mu--install-shape-rels ()
  ;; :dot  ->  :square  ->  :rect   *-> :shape
  ;;            |                   ^
  ;;            |                   |
  ;;            *->  :parallelogram *-> :multiangle
  `(progn
     (mu-rel :dot isa :square)
     (mu-rel :rect isa :shape)
     (mu-rel :square isa :rect)
     (mu-rel :square isa :parallelogram)
     (mu-rel :parallelogram isa :multiangle)
     (mu-rel :parallelogram isa :shape)))


;;* Tests --------------------------------------------------------- *;;


;;** - mu-methods ----------------------------------------------- *;;


(ert-deftest mu-test-implementation-details ()
  "Low-level implementation details should work until implementation changes"
  (mu-test (foo)

    ;; store mu-methods table on the symbol
    (mu-defmulti foo #'identity)
    (should (ht? (mu-methods 'foo)))

    ;; store dispatch and default functions on the symbol
    (should (functionp (get 'foo :mu-dispatch)))
    (should (functionp (get 'foo :mu-default)))

    ;; lookup a mu-defmethod by key
    (mu-defmethod foo (x) :when :rect :rect)
    (should (functionp (mu-methods 'foo :rect)))
    (should (equal :rect (funcall (mu-methods 'foo :rect) :rect)))

    ;; mu-methods should be setf-able when called with keys
    (setf (mu-methods 'foo :rect) (fn (x) :shape))
    (should (equal :shape (funcall (mu-methods 'foo :rect) :rect)))

    ;; mu-methods should be setf-able when called without keys
    (setf (mu-methods 'foo) (ht))
    (should (ht-empty? (mu-methods 'foo)))

    ;; hierarchy is a struct that stores relationships in a table
    (let ((h (make-mu-hierarchy)))
      (should (mu-hierarchy-p h))
      (should (symbolp (mu-hierarchy-id h)))

      ;; nested table values must be settable
      (setf (mu-hierarchy h :rect :parents) '(:shape))
      (should (mu--set-equal? '(:shape) (mu-hierarchy h :rect :parents)))

      ;; table is a hash-table
      (should (ht? (mu-hierarchy h))))

    ;; same should be true of the `mu-global-hierarchy'
    (setf (mu-global-hierarchy :rect :parents) '(:shape))
    (should (mu--set-equal? '(:shape) (mu-global-hierarchy :rect :parents)))
    (should (ht? (mu-global-hierarchy)))

    ;; TODO dispatch cache
    ;; (should (ht? (get 'foo :mu-cache)))
    ))


(ert-deftest mu-test-rel ()
  "Creating `mu-isa?' hierachy should work"
  (mu-test ()
    (should (mu--set-equal? '(:rect :shape) (ht-keys (mu-hierarchy (mu-rel :rect isa :shape)))))
    (should (mu--set-equal? '(:rect :shape :square) (ht-keys (mu-hierarchy (mu-rel :square isa :rect)))))
    (should (member :shape (mu-global-hierarchy :rect :parents)))
    (should (member :rect (mu-global-hierarchy :square :parents)))
    (should (member :square (mu-global-hierarchy :rect :children)))))


(ert-deftest mu-test-ancestors-descendants ()
  "Retrieving parents, ancestors, descendants should work"
  (mu-test ()
    (mu--install-shape-rels)

    ;; always computing mu-ancestors and mu-descendants should work
    (should (mu--set-equal?
             (list :parallelogram :rect :shape :multiangle)
             (mu-ancestors :square mu-global-hierarchy 'compute)))
    (should (mu--set-equal?
             (list :parallelogram :rect :square :dot :square)
             (mu-descendants :shape mu-global-hierarchy 'compute)))

    ;; accumulated :ancestors and :descendants must match recomputed
    (should (mu--set-equal?
             (mu-ancestors :parallelogram)
             (mu-ancestors :parallelogram mu-global-hierarchy 'compute)))
    (should (mu--set-equal?
             (mu-descendants :parallelogram)
             (mu-descendants :parallelogram mu-global-hierarchy 'compute)))

    (should (mu--set-equal?
             (mu-ancestors :multiangle)
             (mu-ancestors :multiangle mu-global-hierarchy 'compute)))
    (should (mu--set-equal?
             (mu-descendants :multiangle)
             (mu-descendants :multiangle mu-global-hierarchy 'compute)))

    (should (mu--set-equal?
             (mu-ancestors :shape)
             (mu-ancestors :shape mu-global-hierarchy 'compute)))
    (should (mu--set-equal?
             (mu-descendants :shape)
             (mu-descendants :shape mu-global-hierarchy 'compute)))

    (should (mu--set-equal?
             (mu-ancestors :square)
             (mu-ancestors :square mu-global-hierarchy 'compute)))
    (should (mu--set-equal?
             (mu-descendants :square)
             (mu-descendants :square mu-global-hierarchy 'compute)))))


(ert-deftest mu-test-isa-hierarchy ()
  "Checking isa relationship should work"
  (mu-test ()
    (mu--install-shape-rels)

    ;; atomic rels should work
    (should (mu-isa? 42 42))
    (should (mu-isa? :rect :shape))
    (should (mu-isa? :square :shape))
    ;; sequential rels should work
    (should (mu-isa? [:square :rect] [:rect :shape]))
    (should (mu-isa? [:square :shape] [:rect :shape]))
    (should (mu-isa? [[:dot :parallelogram] :square] [[:shape :multiangle] :rect]))
    ;; unrelated entities should fail
    (should (null (mu-isa? [:square :rect] [:shape :square])))
    (should (null (mu-isa? [:square] :rect)))
    (should (null (mu-isa? [:square] [])))

    ;; atomic rels should report correct generation
    (should (equal '(:generation . 0) (mu-isa/generations? 42 42)))
    (should (equal '(:generation . 1) (mu-isa/generations? :rect :shape)))
    (should (equal '(:generation . 2) (mu-isa/generations? :square :shape)))
    ;; sequential rels should report correct generatinos
    (should (equal '((:generation . 1) (:generation . 1)) (mu-isa/generations? [:square :rect] [:rect :shape])))
    (should (equal '((:generation . 1) (:generation . 0)) (mu-isa/generations? [:square :shape] [:rect :shape])))
    (should (equal '(((:generation . 3)
                      (:generation . 1))
                     (:generation . 1))
                   (mu-isa/generations? [[:dot :parallelogram] :square] [[:shape :multiangle] :rect])))
    ;; unrelated entities should fail returning no generations
    (should (null (mu-isa/generations? [:square :rect] [:shape :square])))
    (should (null (mu-isa/generations? [:square] :rect)))
    (should (null (mu-isa/generations? [:square] [])))))


(ert-deftest mu-test-multi ()
  "Installing new `multi' dispatch function should work"
  (mu-test (foo)

    (mu-defmulti foo #'identity)

    (should (functionp 'foo))
    (should (mu-methods 'foo))
    (should (null (ht-keys (mu-methods 'foo))))
    (should (functionp (get 'foo :mu-default)))))


(ert-deftest mu-test-mu-defmethod ()
  "Installing and removing `mu-method's should work"
  (mu-test (foo)

    (mu-defmulti foo #'identity)
    (should (ht? (mu-methods 'foo)))

    (mu-defmethod foo (x) :when :a :a)
    (should (mu--set-equal? '(:a) (ht-keys (mu-methods 'foo))))

    (mu-defmethod foo (x) :when :b :b)
    (should (mu--set-equal? '(:a :b) (ht-keys (mu-methods 'foo))))

    ;; one method for every match
    (should (mu--set-equal? '(:a) (ht-keys (mu-methods :for 'foo :matching :a))))
    (should (mu--set-equal? '(:b) (ht-keys (mu-methods :for 'foo :matching :b))))

    ;; :default method when no match installed
    (should (mu--set-equal? '(:default) (ht-keys (mu-methods :for 'foo :matching :c))))

    ;; but no longer :default when installed
    (mu-defmethod foo (x) :when :c :c)
    (should (mu--set-equal? '(:c) (ht-keys (mu-methods :for 'foo :matching :c))))

    ;; methods must be functions
    (should (cl-every #'functionp (ht-values (mu-methods 'foo))))

    ;; removing a method should work
    (mu-methods-remove foo :a)
    (should (mu--set-equal? '(:default) (ht-keys (mu-methods :for 'foo :matching :a))))))


(ert-deftest mu-test-equality-dispatch ()
  "Basic equality based dispatch should work"
  (mu-test (foo)

    (mu-defmulti foo (fn (&rest args) (apply #'vector args)))
    (mu-defmethod foo (&rest x) :when [:a] :a)
    (mu-defmethod foo (&rest x) :when [:b] :b)
    (mu-defmethod foo (&rest x) :when [:a :a] :a)
    (mu-defmethod foo (&rest x) :when [:b :b] :b)

    (should (equal :a (foo :a)))
    (should (equal :b (foo :b)))
    (should (equal :a (foo :a :a)))
    (should (equal :b (foo :b :b)))))


(ert-deftest mu-test-isa-dispatch ()
  "Full isa dispatch should work"
  (mu-test (foo)

    ;; Example from the mu-defmethod docs.
    (mu-rel 'vector :isa :collection)
    (mu-rel 'hash-table :isa :collection)
    (mu-defmulti foo #'type-of)
    (mu-defmethod foo (c) :when :collection :a-collection)
    (mu-defmethod foo (s) :when 'string :a-string)

    (should (equal :a-collection (foo [])))
    (should (equal :a-collection (foo (ht))))
    (should (equal :a-string (foo "bar")))))


(ert-deftest mu-test-ambiguous-methods ()
  "Dispatch ambiguity should be caught or preferred away"
  (mu-test (bar)

    ;; Example from the mu-defmethod docs.
    (mu-rel :rect isa :shape)
    (mu-defmulti bar #'vector)
    (mu-defmethod bar (x y) :when [:rect :shape] :rect-shape)
    (mu-defmethod bar (x y) :when [:shape :rect] :shape-rect)

    ;; since no val is preferred, we start with an empty prefers table
    (should (ht-empty? (mu-prefers bar mu-global-hierarchy)))

    ;; and therefore report error when unable to choose a method
    (should (mu--error-match "multiple methods" (bar :rect :rect)))

    ;; we should be able to register a prefer
    (mu-prefer 'bar [:rect :shape] :over [:shape :rect])
    (should (mu--set-equal?
             '([:shape :rect])
             (mu-prefers bar mu-global-hierarchy [:rect :shape])))

    ;; we should be able to register more than one prefer for the same value
    (mu-prefer 'bar [:rect :shape] :over [:parallelogram :rect])
    (should (mu--set-equal?
             '([:shape :rect] [:parallelogram :rect])
             (mu-prefers bar mu-global-hierarchy [:rect :shape])))

    ;; and the registered prefers should resolve the ambiguity
    (should (equal :rect-shape (bar :rect :rect)))

    ;; we should be able to remove a prefer
    (mu-prefers-remove bar [:rect :shape] :over [:shape :rect])
    (should (mu--set-equal?
             '([:parallelogram :rect])
             (mu-prefers bar mu-global-hierarchy [:rect :shape])))

    ;; and go back to ambiguity
    (should (mu--error-match "multiple methods" (bar :rect :rect)))

    ;; we should be able to remove all prefers for a value
    (mu-prefers-remove bar [:rect :shape])
    (should-not (mu-prefers bar mu-global-hierarchy [:rect :shape]))

    ;; we should be able to remove all registered prefers
    (mu-prefers-remove bar :in mu-global-hierarchy)
    (should (ht-empty? (mu-prefers bar mu-global-hierarchy)))

    ;; inconsintent preferences shouldn't make it into mu-prefers
    (mu-prefer 'bar [:rect :shape] :over [:shape :rect])
    (mu-prefer 'bar [:shape :rect] :over [:parallelogram :rect])
    (mu--error-match "in mu-prefer cyclic preference "
                        (mu-prefer 'bar [:parallelogram :rect] :over [:rect :shape]))

    ;; TODO Maybe test the above with a custom hierarchy
    ))


(ert-deftest mu-test-default-method ()
  "Default method should work"
  (mu-test (foo)

    (mu-defmulti foo #'identity)
    (mu-defmethod foo (x) :when :a :a)

    ;; pre-installed :default when method missing
    (should (mu--error-match "no mu-methods match" (foo :c)))

    ;; :default when method missing and :default installed
    (mu-defmethod foo (x) :when :default :default)
    (should (equal :default (foo :c)))

    ;; no longer :default when installed
    (mu-defmethod foo (x) :when :c :c)
    (should (equal :c (foo :c)))

    ;; removing custom :default should restore pre-installed :default
    (mu-methods-remove 'foo :default)
    (should (mu--error-match "no mu-methods match" (foo :d)))))


(ert-deftest mu-test-errors ()
  "Error conditions should be signaled and possible to catch"
  (mu-test (foo bar)

    (should (equal "mu-error" (get 'mu-error 'error-message)))
    (should-error (mu-error "foo %s" 'bar) :type 'mu-error)

    (mu-rel :rect isa :shape)
    (mu-rel :square isa :rect)
    (mu-rel :square isa :parallelogram)

    ;; should signal attempt to relate structured data
    (should (mu--error-match "in mu-rel no meaningful semantics"
                                (mu-rel [:a :b] isa [:c :d])))
    (should (mu--error-match "in mu-rel no meaningful semantics"
                                (mu-rel "foo" isa "bar")))
    (should (mu--error-match "in mu-rel no meaningful semantics"
                                (mu-rel (make-mu-hierarchy) :isa (make-mu-hierarchy))))

    (mu-defmulti foo #'identity)
    (mu-defmethod foo (a) :when :square :square)
    (mu-defmethod foo (a) :when :shape :shape)

    ;; catch malformed arglist in `mu-rel' call
    (should (mu--error-match "in mu-rel malformed arglist" (mu-rel :foo :bar)))

    ;; signal ambiguous methods
    (should (mu--error-match "multiple methods" (foo :square)))

    ;; preinstalled :default method should signal method missing
    (should (mu--error-match "no mu-methods match" (foo :triangle)))

    ;; catch cycle relationships
    (should (mu--error-match "in mu-rel cyclic relationship" (mu-rel :shape isa :square)))
    ;; full cycle path should be reported
    (should (equal '(:square :rect :shape) (mu--cycle? :shape :square)))

    ;; catch malformed arglist in `multi' call
    (should (mu--error-match "in mu-defmulti malformed arglist" (mu-defmulti bar :val [a b])))

    ;; catch malformed arglist in `mu-method' call
    (should (mu--error-match "in mu-defmethod malformed arglist" (mu-defmethod bar :val [a b])))))


(ert-deftest mu-test-custom-hierarchy ()
  "Mu-Methods should work with custom hierarchies"
  ;; TODO Current implementation bakes the hierarchy at (mu-defmulti foo ...)
  ;; definition, so every (foo ...) invocation will use the same hierarchy. In
  ;; this respect we follow Clojure. My hunch, however, is this introduces
  ;; unnecessary coupling between mu-methods and hierarchies. IMO hierarchies
  ;; are in fact independent, so why marry the two linked but orthogonal concepts?
  ;; Understandably, the case could be made that decoupling them doesn't add
  ;; expressive power that would actually be used. That's certainly a good reason
  ;; to follow Clojure.

  ;; NOTE see note on lexical vs dynamic scope in `multi.el'

  (eval
   ;; NOTICE that we must `quote' the form for this to work!!!
   '(mu-test (bar)
      (let ((hierarchy (make-mu-hierarchy)))
        ;; override :rect rel in custom hierarchy
        (mu-rel :rect isa :parallelogram in hierarchy)
        (mu-rel :square isa :rect in hierarchy)
        ;; define mu-dispatch over the custom hierarchy
        (mu-defmulti bar #'identity :in hierarchy)
        (mu-defmethod bar (a) :when :parallelogram :parallelogram)
        (let ((hierarchy (make-mu-hierarchy)))
          (mu-rel :rect isa :shape in hierarchy)
          (mu-rel :square isa :rect in hierarchy)
          ;; Method calls should still use custom hierarchy, so that :rect and
          ;; :square are :parallelograms
          (should (equal :parallelogram (bar :rect)))
          (should (equal :parallelogram (bar :square)))))
      ;; even outside of both `let's we should get the same result
      (should (equal :parallelogram (bar :rect)))
      (should (equal :parallelogram (bar :square))))
   'lexical-scope))


;;** - mu-case ------------------------------------------------- *;;


(ert-deftest mu-test-simple-mu-case-patterns ()
  "Mu-Case should match simple patterns"

  (should (equal 'match (mu-case '(a)
                          (_ 'match))))

  (should (equal '(a) (mu-case '(a)
                        (lst lst))))

  (should (equal '(a) (mu-case '(a)
                        ([x y] (list x y))
                        ([x] (list x)))))

  (should (equal 'empty (mu-case '()
                          ([x] x)
                          (otherwise 'empty))))

  (should (equal 'match (mu-case '(a b c)
                          (['a _ 'c] 'match))))

  (defmacro mu-case--clause-test (expr pat &rest body)
    (declare (indent 1))
    `(pcase ,expr
       ,(mu-case--clause (cons pat body))
       (otherwise
        'no-match)))

  (should (equal 'match (mu-case--clause-test '()
                          [] 'match)))

  (should (equal 'symbol (mu-case--clause-test '(a)
                           ['a] 'symbol)))

  (should (equal 'b (mu-case--clause-test '(a b)
                      [(or 'a 'b) (and x 'b)] x)))

  (should (equal 'match (mu-case--clause-test '(:key)
                          [:key] 'match)))

  (should (equal :key (mu-case--clause-test '(:key)
                        [x] x)))

  (should (equal '(a :over b) (mu-case--clause-test '(a (:over) b)
                                [x (or [rel] :to) y] (list x rel y)))))


(ert-deftest mu-test-standard-mu-case-patterns ()
  "Mu-Case should allow standard pcase patterns"

  (should (equal '(a over b none) (mu-case '(a over b)
                                    ([x
                                      (and (pred symbolp)
                                           (or 'over 'to) rel)
                                      y]
                                     (list x rel y 'none)))))

  (should (equal '(1 2 3) (mu-case '(1 2)
                            ([(and (pred numberp)
                                   (app 1- 0)
                                   x)
                              (and num
                                   (let y 3))]
                             (list x num y))))))


(ert-deftest mu-test-mu-case-rest-patterns ()
  "Mu-Case should allow matching the rest of a list"

  (should (equal '(b c) (mu-case '(a b c)
                          (['a &rest tail] tail))))

  (should (equal 'c (mu-case '(a b c)
                      (['a &rest ['b last]] last))))

  (should (equal 'c (mu-case '(a b c)
                      (['a &rest (or [] ['b last])] last))))

  (should (equal 'match (mu-case '(a)
                          (['a &rest (or [] ['b last])] 'match))))

  (should (equal '(a b h) (mu-case '(a :over b :in h)
                            ([x (or :over :to) y &rest (or [:in z] [])]
                             (list x y (or z 'none)))))))


(ert-deftest mu-test-mu-case-nested-list-patterns ()
  "Mu-Case should allow nested list patterns"

  (should (equal '(2 3) (mu-case '(1 (2 3))
                          ([_ [a b]] (list a b)))))

  (should (equal '(1 2 3 4) (mu-case '((1 . 2) (3 . 4))
                              ([[a &rest b]
                                [(and (pred numberp) c)
                                 &rest
                                 (and (pred numberp) d)]]
                               (list a b c d))))))


(ert-deftest mu-test-mu-case-vector-patterns ()
  "Mu-Case should simple vector patterns"

  (should (equal '(1 2)
                 (mu-case [1 2]
                   (`[b c] (list b c)))))

  (should (equal '(b c)
                 (mu-case (list 'a [b c])
                   ([a `[b c]] (list b c))))))


(ert-deftest mu-test-mu-case-errors ()
  "Mu-Case should signal malformed patterns"
  (should
   (mu--error-match "in mu-case malformed &rest" (mu-case '(a b c)
                                                   (['a &rest foo bar] 'oops)))))


(ert-deftest mu-test-mu-let ()
  "Mu-let should work"
  (should (equal '(1 2 3 4) (mu-let (([a b c] '(1 2 3))
                                     ([_ d] '(0 4)))
                              (list a b c d))))

  (should (mu--error-match "in mu-let malformed" (mu-let (([_])) 'foo))))


(ert-deftest mu-test-ht-pattern ()
  "Mu-case should pattern match on hash-tables and alists"

  ;; match empty hash-table
  (should (equal 'match (mu-case (ht)
                          ((ht) 'match))))

  ;; match hash-table
  (should (equal 1 (mu-case (ht (:a 1))
                     ((ht :a) a))))

  ;; match alist
  (should (equal 1 (mu-case '((:a . 1))
                     ((ht :a) a))))

  ;; match whatever key-types hash-tables and alists allow
  (should (equal 1 (mu-case (ht ("foo" 1))
                     ((ht ("foo" a)) a))))

  ;; match tables inside a list
  (should (equal '(1 2) (mu-case (list (ht (:a 1)) '((:b . 2)))
                          ([(ht :a) (ht b)] (list a b)))))

  ;; allow all ht key pattern styles: :key, key, (:key id) ('key id)
  (should (equal '(1 2 3 4) (mu-case (ht (:a 1) ('b 2) (:c 3) ('d 4))
                              ((ht :a b 'c ('d D)) (list a b c D)))))

  (should (mu--error-match "in mu-case malformed ht pattern"
                              (mu-case (ht (:a 1))
                                ((ht "a"))))))


;;** - mu-fun -------------------------------------------------- *;;


(ert-deftest mu-test-defun ()
  "Mu-fun should define a function"
  (mu-test (foo-fun foo-macro)
    (should
     (equal '((:a :b 1 2)
              (:a :b 1)
              (:a :b)
              (:a nil))

            (progn
              (mu-defun foo-fun (&optional a b &rest args)
                :doc "string"
                :sig (a b c d)
                :interactive t
                ([x y]     (list a b x y))
                ([x]       (list a b x))
                (otherwise (list a b)))

              (list
               (foo-fun :a :b 1 2)
               (foo-fun :a :b 1)
               (foo-fun :a :b 1 2 3)
               (foo-fun :a)))))

    (should
     (equal '((:a :b 1 2)
              (:a :b 1)
              (:a :b))

            (progn
              (mu-defmacro foo-macro (a b &rest args)
                :doc "string"
                :sig (a x :in other)
                :sigs t
                :declare ((indent defun))
                ([x y]     `(list ,a ,b ,x ,y))
                ([x]       `(list ,a ,b ,x))
                (otherwise `(list ,a ,b)))

              (list
               (foo-macro :a :b 1 2)
               (foo-macro :a :b 1)
               (foo-macro :a :b 1 2 3)))))))


;;* Perf ---------------------------------------------------------- *;;


(defmacro mu-test-time (&rest body)
  (declare (indent defun))
  `(let ((start (float-time)))
     ,@body
     (- (float-time) start)))

;; TODO Test mu-defmethod isa vs struct inheritance?

;; NOTE Rather ugly way to measure performance, but does the trick. We stack up
;; mu-methods against built-in generic dispatch. Our generic dispatches on the
;; type of its first argument, so to compare apples to apples we use #'type-of as
;; our mu-dispatch. For mu-methods we run 10'000 repeats, each repeat
;; performs 7 dispatches for a total of 70'000 dispatches. We do the same for
;; generic dispatch.
;;
;; Without caching mu-methods are ~100x slower:
;;
;; (mu-test-perf)
;; =>
;; ((:mu-defmethod (:total . 2.364870071411133)
;;                (:average . 3.3783858163016185e-05))
;;  (:defmethod   (:total . 0.021378040313720703)
;;                (:average . 3.0540057591029576e-07)))


(defun mu-test-perf ()
  (mu-defmulti foo-test #'type-of)
  (mu-defmethod foo-test (x) :when 'foo-struct-1 1)
  (mu-defmethod foo-test (x) :when 'foo-struct-2 2)
  (mu-defmethod foo-test (x) :when 'foo-struct-3 3)
  (mu-defmethod foo-test (x) :when 'foo-struct-4 4)
  (mu-defmethod foo-test (x) :when 'foo-struct-5 5)
  (mu-defmethod foo-test (x) :when 'foo-struct-6 6)
  (mu-defmethod foo-test (x) :when :default 0)

  (cl-defstruct foo-struct-0)
  (cl-defstruct foo-struct-1)
  (cl-defstruct foo-struct-2)
  (cl-defstruct foo-struct-3)
  (cl-defstruct foo-struct-4)
  (cl-defstruct foo-struct-5)
  (cl-defstruct foo-struct-6)

  (defgeneric foo-struct-test (s) 0)
  (defmethod foo-struct-test ((s foo-struct-1)) 1)
  (defmethod foo-struct-test ((s foo-struct-2)) 2)
  (defmethod foo-struct-test ((s foo-struct-3)) 3)
  (defmethod foo-struct-test ((s foo-struct-4)) 4)
  (defmethod foo-struct-test ((s foo-struct-5)) 5)
  (defmethod foo-struct-test ((s foo-struct-6)) 6)

  (let ((s0 (make-foo-struct-0))
        (s1 (make-foo-struct-1))
        (s2 (make-foo-struct-2))
        (s3 (make-foo-struct-3))
        (s4 (make-foo-struct-4))
        (s5 (make-foo-struct-5))
        (s6 (make-foo-struct-6)))
    (ht->alist
     (ht
      (:defmethod
       (let* ((total (mu-test-time
                       (cl-loop repeat 10000
                                do (foo-struct-test s0)
                                do (foo-struct-test s1)
                                do (foo-struct-test s2)
                                do (foo-struct-test s3)
                                do (foo-struct-test s4)
                                do (foo-struct-test s5)
                                do (foo-struct-test s6))))
              (average (/ total 7 10000)))
         (list (cons :total total)
               (cons :average average))))

      (:mu-method
       (let* ((total (mu-test-time
                       (cl-loop repeat 10000
                                do (foo-test s0)
                                do (foo-test s1)
                                do (foo-test s2)
                                do (foo-test s3)
                                do (foo-test s4)
                                do (foo-test s5)
                                do (foo-test s6))))
              (average (/ total 7 10000)))
         (list (cons :total total) (cons :average average))))))))


;;* Playground ---------------------------------------------------- *;;


;; TODO Alternative syntax for most of the above test. Idea is for `expect' have
;; three arguments or fewer with test-predicate being in infix position:
;;
;; 0  (expect  expected-value predicate expr &optional :after stateful-expr)
;; 1  (expect  (predicate expr))
;; 2  (expect  predicate expr)
;; 3  (expectr expr predicate expected-value &optional :after stateful-expr)
;;
;; rewrite into
;;
;; 0  (should (progn stateful-expr (predicate expected-value test-body)))
;; 1  (should (predicate expr))
;; 2  (should (predicate expr))
;; 3  (should (progn stateful-expr (predicate expected-value test-body)))
;;
;; Does it make test structure more obvious?
;; Less noisy?
;; Easier for the eye to pick out the essence of the test?
;; Despite the significant right drift and the need for wide screen?

(comment


 (ert-deftest mu-test-rel ()
   "Creating `mu-isa?' hierachy should work"
   (mu-test ((set= mu--set-equal?))
     (expect '(:rect :shape)         set= (ht-keys (mu-rel :rect isa :shape)))
     (expect '(:rect :shape :square) set= (ht-keys (mu-rel :square isa :rect)))

     (expect :shape  member (mu-global-hierarchy :rect :parents))
     (expect :rect   member (mu-global-hierarchy :square :parents))
     (expect :square member (mu-global-hierarchy :rect :children))))


 (ert-deftest mu-test-relationships ()
   "Retrieving parents, ancestors, descendants should work"
   (mu-test ((set= mu--set-equal?))
     (expect '(:shape)                      set= (mu-parents :rect) :after (mu-rel :rect isa :shape))
     (expect '(:rect)                       set= (mu-parents :square) :after (mu-rel :square isa :rect))
     (expect '(:parallelogram :rect :shape) set= (mu-ancestors :square) :after (mu-rel :square isa :parallelogram))
     (expect '(:rect :square)               set= (mu-descendants :shape))))


 (ert-deftest mu-test-isa-hierarchy ()
   (mu-test ()
     (mu-rel :rect isa :shape)
     (mu-rel :square isa :rect)
     (expect '(:generation . 0)                     equal (mu-isa? 42 42))
     (expect '(:generation . 1)                     equal (mu-isa? :rect :shape))
     (expect '(:generation . 2)                     equal (mu-isa? :square :shape))
     (expect '((:generation . 1) (:generation . 1)) equal (mu-isa? [:square :rect] [:rect :shape]))
     (expect '((:generation . 1) (:generation . 0)) equal (mu-isa? [:square :shape] [:rect :shape]))
     (expect (null (mu-isa? [:square :rect] [:shape :square])))
     (expect (null (mu-isa? [:square] :rect)))
     (expect (null (mu-isa? [:square] [])))))


 (ert-deftest mu-test-multi ()
   "Defining new multi dispatcher should work"
   (mu-test ((set= mu--set-equal?) foo)
     (expect (null (mu-methods 'foo)) :after (mu-defmulti foo #'identity))
     (expect '(:default)                set=         (ht-keys (mu-methods 'foo)))
     (expect (functionp 'foo))
     (expect (functionp (mu-methods 'foo :default)))))


 (ert-deftest mu-test-mu-defmethod ()
   "Installing and removing `mu-method's should work"
   (mu-test ((set= mu--set-equal?) foo)
     (mu-defmulti foo #'identity)
     (expect '(:a :default)    set= (ht-keys (mu-methods 'foo)) :after (mu-defmethod foo (x) :when :a :a))
     (expect '(:a :b :default) set= (ht-keys (mu-methods 'foo)) :after (mu-defmethod foo (x) :when :b :b))

     ;; one method for every match
     (expect '(:a) set= (mapcar #'car (mu-methods :for 'foo :matching :a)))
     (expect '(:b) set= (mapcar #'car (mu-methods :for 'foo :matching :b)))

     ;; :default method when no method installed
     (expect '(:default) set= (mapcar #'car (mu-methods :for 'foo :matching :c)))
     ;; but no longer :default when installed
     (expect '(:c)       set= (mapcar #'car (mu-methods :for 'foo :matching :c)) :after (mu-defmethod foo (x) :when :c :c))

     ;; methods must be functions
     (expect #'functionp cl-every (ht-values (mu-methods 'foo)))

     ;; removing a mu-defmethod should work
     (mu-methods-remove 'foo :a)
     (should (mu--set-equal? '(:default) (mapcar #'car (mu-methods :for 'foo :matching :a))))))


 (ert-deftest mu-test-equality-dispatch ()
   "Basic equality based dispatch should work"
   (mu-test (foo)
     (mu-defmulti foo #'identity)
     (expect :a       equal (foo :a) :after (mu-defmethod foo (x) :when :a :a))
     (expect :b       equal (foo :b) :after (mu-defmethod foo (x) :when :b :b))

     ;; :default when method missing
     (expect :default equal (foo :c) :after (mu-defmethod foo (x) :when :default :default))

     ;; no :default when installed
     (expect :c       equal (too :c) :after (mu-defmethod foo (x) :when :c :c))

     ;; back to :default when removed
     (expect :default equal (foo :c) :after (mu-methods-remove 'foo :c))))


 (ert-deftest mu-test-isa-dispatch ()
   "Full isa dispatch should work"
   (mu-test (foo)
     ;; Example from the mu-defmethod docs.
     (mu-rel 'vector :isa :collection)
     (mu-rel 'hash-table :isa :collection)
     (mu-defmulti foo #'type-of)
     (mu-defmethod foo (c) :when :collection :a-collection)
     (mu-defmethod foo (s) :when 'string :a-string)

     (expect :a-collection equal (foo []))
     (expect :a-collection equal (foo (ht)))
     (expect :a-string     equal (foo "bar"))))


 (ert-deftest mu-test-errors ()
   "Error conditions should be signaled and possible to catch"
   (mu-test (foo bar)

     (expect "mu-error" equal (get 'mu-error 'error-message))
     (expect-error 'mu-error :after (mu-error "foo %s" 'bar))

     (mu-rel :rect isa :shape)
     (mu-rel :square isa :rect)
     (mu-rel :square isa :parallelogram)
     (mu-defmulti foo #'identity)
     (mu-defmethod foo (a) :when :square :square)
     (mu-defmethod foo (a) :when :shape :shape)

     ;; signal ambiguous methods
     (expect "multiple methods" mu--error-match (foo :square))

     ;; preinstalled :default method should signal method missing
     (expect "no mu-methods match" mu--error-match (foo :triangle))

     ;; catch cycle relationships
     (expect "cycle relationship" mu--error-match (mu-rel :shape isa :square))

     ;; catch malformed arglist in `multi' call
     (expect "malformed arglist" mu--error-match (mu-defmulti bar :val [a b]))

     ;; catch malformed arglist in `mu-method' call
     (expect "malformed arglist" mu--error-match (mu-defmethod bar :val [a b]))))
 ;; comment
 )
