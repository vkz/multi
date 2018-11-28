

;; Disable lexical-binding check for now
(setq multi-lexical-binding nil)

;; TODO I leave lexical binding off, cause I suspect my multi-test may on occasion
;; not work, cause it captures stuff lexically. I need to investigate cause I'd
;; rather have lexical scope here, too.


(require 'ert)
(require 'cl-lib)
(require 'multi)

;; TODO Cache

;; TODO Would implementing expect macro as per examples below be worth it?


;;* Prelude ------------------------------------------------------- *;;


(defun multi--symbol-function (s)
  "Like `symbol-function' but returns :unbound when S is
unbound."
  (declare
   (gv-setter (lambda (val)
                `(case ,val
                   ((:unbind :unbound) (fmakunbound ,s))
                   (otherwise          (fset ,s ,val))))))
  (or (symbol-function s) :unbound))


(defmacro multi-test (multis &rest body)
  "Set `multi-global-hierarchy' and `multi-methods' to empty
tables and unbind functions in the MULTIS list for the extent of
BODY, allowing it to bind them as needed. Restore everything
after BODY."
  (declare (indent defun))
  (let ((multis (mapcar (fn (m) `((multi--symbol-function ',m) :unbind)) multis)))
    `(cl-letf ((multi-global-hierarchy (make-multi-hierarchy))
               ,@multis)
       ,@body)))


(example
 (list
  (multi--symbol-function 'bar)
  (multi-test (bar)
    (multi bar [a] a)
    (multi--symbol-function 'bar))
  (multi--symbol-function 'bar))
 ;; example
 )


(cl-defmacro multi--error-match (prefix &rest body)
  "Try catching `multi-error' thrown by BODY and test if its
message prefix matches PREFIX"
  `(string-prefix-p
    ,prefix
    (condition-case err
        (progn ,@body)
        (multi-error (cadr err)))
    'ignore-case))


(example
 (should (multi--error-match "foo" (multi-error "foo %s" 'bar)))
 ;; example
 )


(defun multi--set-equal? (s1 s2)
  "Plenty good set-equality for testing"
  (unless (and (listp s1) (listp s2))
    (multi-error
     "in multi--set-equal? expected list arguments, but got %s and %s"
     s1 s2))
  (and (null (cl-set-difference s1 s2 :test #'equal))
       (null (cl-set-difference s2 s1 :test #'equal))))

(example
 (multi--set-equal? (list 1 2 3) (list 3 2 1))
 (multi--set-equal? (list 1 2 3) (list 3 2))
 (multi--set-equal? '() (list 1 2 3))
 ;; example
 )


(defmacro multi--install-shape-rels ()
  ;; :dot  ->  :square  ->  :rect   *-> :shape
  ;;            |                   ^
  ;;            |                   |
  ;;            *->  :parallelogram *-> :multiangle
  `(progn
     (multi-rel :dot isa :square)
     (multi-rel :rect isa :shape)
     (multi-rel :square isa :rect)
     (multi-rel :square isa :parallelogram)
     (multi-rel :parallelogram isa :multiangle)
     (multi-rel :parallelogram isa :shape)))


;;* Tests --------------------------------------------------------- *;;


;;** - multi-methods ----------------------------------------------- *;;


(ert-deftest multi-test-implementation-details ()
  "Low-level implementation details should work until implementation changes"
  (multi-test (foo)

    ;; store multi-methods table on the symbol
    (multi foo #'identity)
    (should (ht? (multi-methods 'foo)))

    ;; store dispatch and default functions on the symbol
    (should (functionp (get 'foo :multi-dispatch)))
    (should (functionp (get 'foo :multi-default)))

    ;; lookup a multi-method by key
    (multi-method foo (x) :when :rect :rect)
    (should (functionp (multi-methods 'foo :rect)))
    (should (equal :rect (funcall (multi-methods 'foo :rect) :rect)))

    ;; multi-methods should be setf-able when called with keys
    (setf (multi-methods 'foo :rect) (fn (x) :shape))
    (should (equal :shape (funcall (multi-methods 'foo :rect) :rect)))

    ;; multi-methods should be setf-able when called without keys
    (setf (multi-methods 'foo) (ht))
    (should (ht-empty? (multi-methods 'foo)))

    ;; hierarchy is a struct that stores relationships in a table
    (let ((h (make-multi-hierarchy)))
      (should (multi-hierarchy-p h))
      (should (symbolp (multi-hierarchy-id h)))

      ;; nested table values must be settable
      (setf (multi-hierarchy h :rect :parents) '(:shape))
      (should (multi--set-equal? '(:shape) (multi-hierarchy h :rect :parents)))

      ;; table is a hash-table
      (should (ht? (multi-hierarchy h))))

    ;; same should be true of the `multi-global-hierarchy'
    (setf (multi-global-hierarchy :rect :parents) '(:shape))
    (should (multi--set-equal? '(:shape) (multi-global-hierarchy :rect :parents)))
    (should (ht? (multi-global-hierarchy)))

    ;; TODO dispatch cache
    ;; (should (ht? (get 'foo :multi-cache)))
    ))


(ert-deftest multi-test-rel ()
  "Creating `multi-isa?' hierachy should work"
  (multi-test ()
    (should (multi--set-equal? '(:rect :shape) (ht-keys (multi-hierarchy (multi-rel :rect isa :shape)))))
    (should (multi--set-equal? '(:rect :shape :square) (ht-keys (multi-hierarchy (multi-rel :square isa :rect)))))
    (should (member :shape (multi-global-hierarchy :rect :parents)))
    (should (member :rect (multi-global-hierarchy :square :parents)))
    (should (member :square (multi-global-hierarchy :rect :children)))))


(ert-deftest multi-test-ancestors-descendants ()
  "Retrieving parents, ancestors, descendants should work"
  (multi-test ()
    (multi--install-shape-rels)

    ;; always computing multi-ancestors and multi-descendants should work
    (should (multi--set-equal?
             (list :parallelogram :rect :shape :multiangle)
             (multi-ancestors :square multi-global-hierarchy 'compute)))
    (should (multi--set-equal?
             (list :parallelogram :rect :square :dot :square)
             (multi-descendants :shape multi-global-hierarchy 'compute)))

    ;; accumulated :ancestors and :descendants must match recomputed
    (should (multi--set-equal?
             (multi-ancestors :parallelogram)
             (multi-ancestors :parallelogram multi-global-hierarchy 'compute)))
    (should (multi--set-equal?
             (multi-descendants :parallelogram)
             (multi-descendants :parallelogram multi-global-hierarchy 'compute)))

    (should (multi--set-equal?
             (multi-ancestors :multiangle)
             (multi-ancestors :multiangle multi-global-hierarchy 'compute)))
    (should (multi--set-equal?
             (multi-descendants :multiangle)
             (multi-descendants :multiangle multi-global-hierarchy 'compute)))

    (should (multi--set-equal?
             (multi-ancestors :shape)
             (multi-ancestors :shape multi-global-hierarchy 'compute)))
    (should (multi--set-equal?
             (multi-descendants :shape)
             (multi-descendants :shape multi-global-hierarchy 'compute)))

    (should (multi--set-equal?
             (multi-ancestors :square)
             (multi-ancestors :square multi-global-hierarchy 'compute)))
    (should (multi--set-equal?
             (multi-descendants :square)
             (multi-descendants :square multi-global-hierarchy 'compute)))))


(ert-deftest multi-test-isa-hierarchy ()
  "Checking isa relationship should work"
  (multi-test ()
    (multi--install-shape-rels)

    ;; atomic rels should work
    (should (multi-isa? 42 42))
    (should (multi-isa? :rect :shape))
    (should (multi-isa? :square :shape))
    ;; sequential rels should work
    (should (multi-isa? [:square :rect] [:rect :shape]))
    (should (multi-isa? [:square :shape] [:rect :shape]))
    (should (multi-isa? [[:dot :parallelogram] :square] [[:shape :multiangle] :rect]))
    ;; unrelated entities should fail
    (should (null (multi-isa? [:square :rect] [:shape :square])))
    (should (null (multi-isa? [:square] :rect)))
    (should (null (multi-isa? [:square] [])))

    ;; atomic rels should report correct generation
    (should (equal '(:generation . 0) (multi-isa/generations? 42 42)))
    (should (equal '(:generation . 1) (multi-isa/generations? :rect :shape)))
    (should (equal '(:generation . 2) (multi-isa/generations? :square :shape)))
    ;; sequential rels should report correct generatinos
    (should (equal '((:generation . 1) (:generation . 1)) (multi-isa/generations? [:square :rect] [:rect :shape])))
    (should (equal '((:generation . 1) (:generation . 0)) (multi-isa/generations? [:square :shape] [:rect :shape])))
    (should (equal '(((:generation . 3)
                      (:generation . 1))
                     (:generation . 1))
                   (multi-isa/generations? [[:dot :parallelogram] :square] [[:shape :multiangle] :rect])))
    ;; unrelated entities should fail returning no generations
    (should (null (multi-isa/generations? [:square :rect] [:shape :square])))
    (should (null (multi-isa/generations? [:square] :rect)))
    (should (null (multi-isa/generations? [:square] [])))))


(ert-deftest multi-test-multi ()
  "Installing new `multi' dispatch function should work"
  (multi-test (foo)

    (multi foo #'identity)

    (should (functionp 'foo))
    (should (multi-methods 'foo))
    (should (null (ht-keys (multi-methods 'foo))))
    (should (functionp (get 'foo :multi-default)))))


(ert-deftest multi-test-multi-method ()
  "Installing and removing `multi-method's should work"
  (multi-test (foo)

    (multi foo #'identity)
    (should (ht? (multi-methods 'foo)))

    (multi-method foo (x) :when :a :a)
    (should (multi--set-equal? '(:a) (ht-keys (multi-methods 'foo))))

    (multi-method foo (x) :when :b :b)
    (should (multi--set-equal? '(:a :b) (ht-keys (multi-methods 'foo))))

    ;; one method for every match
    (should (multi--set-equal? '(:a) (ht-keys (multi-methods :for 'foo :matching :a))))
    (should (multi--set-equal? '(:b) (ht-keys (multi-methods :for 'foo :matching :b))))

    ;; :default method when no match installed
    (should (multi--set-equal? '(:default) (ht-keys (multi-methods :for 'foo :matching :c))))

    ;; but no longer :default when installed
    (multi-method foo (x) :when :c :c)
    (should (multi--set-equal? '(:c) (ht-keys (multi-methods :for 'foo :matching :c))))

    ;; methods must be functions
    (should (cl-every #'functionp (ht-values (multi-methods 'foo))))

    ;; removing a method should work
    (multi-methods-remove foo :a)
    (should (multi--set-equal? '(:default) (ht-keys (multi-methods :for 'foo :matching :a))))))


(ert-deftest multi-test-equality-dispatch ()
  "Basic equality based dispatch should work"
  (multi-test (foo)

    (multi foo (fn (&rest args) (apply #'vector args)))
    (multi-method foo (&rest x) :when [:a] :a)
    (multi-method foo (&rest x) :when [:b] :b)
    (multi-method foo (&rest x) :when [:a :a] :a)
    (multi-method foo (&rest x) :when [:b :b] :b)

    (should (equal :a (foo :a)))
    (should (equal :b (foo :b)))
    (should (equal :a (foo :a :a)))
    (should (equal :b (foo :b :b)))))


(ert-deftest multi-test-isa-dispatch ()
  "Full isa dispatch should work"
  (multi-test (foo)

    ;; Example from the multi-method docs.
    (multi-rel 'vector :isa :collection)
    (multi-rel 'hash-table :isa :collection)
    (multi foo #'type-of)
    (multi-method foo (c) :when :collection :a-collection)
    (multi-method foo (s) :when 'string :a-string)

    (should (equal :a-collection (foo [])))
    (should (equal :a-collection (foo (ht))))
    (should (equal :a-string (foo "bar")))))


(ert-deftest multi-test-ambiguous-methods ()
  "Dispatch ambiguity should be caught or preferred away"
  (multi-test (bar)

    ;; Example from the multi-method docs.
    (multi-rel :rect isa :shape)
    (multi bar #'vector)
    (multi-method bar (x y) :when [:rect :shape] :rect-shape)
    (multi-method bar (x y) :when [:shape :rect] :shape-rect)

    ;; since no val is preferred, we start with an empty prefers table
    (should (ht-empty? (multi-prefers bar multi-global-hierarchy)))

    ;; and therefore report error when unable to choose a method
    (should (multi--error-match "multiple methods" (bar :rect :rect)))

    ;; we should be able to register a prefer
    (multi-prefer 'bar [:rect :shape] :over [:shape :rect])
    (should (multi--set-equal?
             '([:shape :rect])
             (multi-prefers bar multi-global-hierarchy [:rect :shape])))

    ;; we should be able to register more than one prefer for the same value
    (multi-prefer 'bar [:rect :shape] :over [:parallelogram :rect])
    (should (multi--set-equal?
             '([:shape :rect] [:parallelogram :rect])
             (multi-prefers bar multi-global-hierarchy [:rect :shape])))

    ;; and the registered prefers should resolve the ambiguity
    (should (equal :rect-shape (bar :rect :rect)))

    ;; we should be able to remove a prefer
    (multi-prefers-remove bar [:rect :shape] :over [:shape :rect])
    (should (multi--set-equal?
             '([:parallelogram :rect])
             (multi-prefers bar multi-global-hierarchy [:rect :shape])))

    ;; and go back to ambiguity
    (should (multi--error-match "multiple methods" (bar :rect :rect)))

    ;; we should be able to remove all prefers for a value
    (multi-prefers-remove bar [:rect :shape])
    (should-not (multi-prefers bar multi-global-hierarchy [:rect :shape]))

    ;; we should be able to remove all registered prefers
    (multi-prefers-remove bar :in multi-global-hierarchy)
    (should (ht-empty? (multi-prefers bar multi-global-hierarchy)))

    ;; inconsintent preferences shouldn't make it into multi-prefers
    (multi-prefer 'bar [:rect :shape] :over [:shape :rect])
    (multi-prefer 'bar [:shape :rect] :over [:parallelogram :rect])
    (multi--error-match "in multi-prefer cyclic preference "
                        (multi-prefer 'bar [:parallelogram :rect] :over [:rect :shape]))

    ;; TODO Maybe test the above with a custom hierarchy
    ))


(ert-deftest multi-test-default-method ()
  "Default method should work"
  (multi-test (foo)

    (multi foo #'identity)
    (multi-method foo (x) :when :a :a)

    ;; pre-installed :default when method missing
    (should (multi--error-match "no multi-methods match" (foo :c)))

    ;; :default when method missing and :default installed
    (multi-method foo (x) :when :default :default)
    (should (equal :default (foo :c)))

    ;; no longer :default when installed
    (multi-method foo (x) :when :c :c)
    (should (equal :c (foo :c)))

    ;; removing custom :default should restore pre-installed :default
    (multi-methods-remove 'foo :default)
    (should (multi--error-match "no multi-methods match" (foo :d)))))


(ert-deftest multi-test-errors ()
  "Error conditions should be signaled and possible to catch"
  (multi-test (foo bar)

    (should (equal "multi-error" (get 'multi-error 'error-message)))
    (should-error (multi-error "foo %s" 'bar) :type 'multi-error)

    (multi-rel :rect isa :shape)
    (multi-rel :square isa :rect)
    (multi-rel :square isa :parallelogram)

    ;; should signal attempt to relate structured data
    (should (multi--error-match "in multi-rel no meaningful semantics"
                                (multi-rel [:a :b] isa [:c :d])))
    (should (multi--error-match "in multi-rel no meaningful semantics"
                                (multi-rel "foo" isa "bar")))
    (should (multi--error-match "in multi-rel no meaningful semantics"
                                (multi-rel (make-multi-hierarchy) :isa (make-multi-hierarchy))))

    (multi foo #'identity)
    (multi-method foo (a) :when :square :square)
    (multi-method foo (a) :when :shape :shape)

    ;; catch malformed arglist in `multi-rel' call
    (should (multi--error-match "in multi-rel malformed arglist" (multi-rel :foo :bar)))

    ;; signal ambiguous methods
    (should (multi--error-match "multiple methods" (foo :square)))

    ;; preinstalled :default method should signal method missing
    (should (multi--error-match "no multi-methods match" (foo :triangle)))

    ;; catch cycle relationships
    (should (multi--error-match "in multi-rel cyclic relationship" (multi-rel :shape isa :square)))
    ;; full cycle path should be reported
    (should (equal '(:square :rect :shape) (multi--cycle? :shape :square)))

    ;; catch malformed arglist in `multi' call
    (should (multi--error-match "in multi malformed arglist" (multi bar :val [a b])))

    ;; catch malformed arglist in `multi-method' call
    (should (multi--error-match "in multi-method malformed arglist" (multi-method bar :val [a b])))))


(ert-deftest multi-test-custom-hierarchy ()
  "Multi-Methods should work with custom hierarchies"
  ;; TODO Current implementation bakes the hierarchy at (multi foo ...)
  ;; definition, so every (foo ...) invocation will use the same hierarchy. In
  ;; this respect we follow Clojure. My hunch, however, is this introduces
  ;; unnecessary coupling between multi-methods and hierarchies. IMO hierarchies
  ;; are in fact independent, so why marry the two linked but orthogonal concepts?
  ;; Understandably, the case could be made that decoupling them doesn't add
  ;; expressive power that would actually be used. That's certainly a good reason
  ;; to follow Clojure.

  ;; NOTE see note on lexical vs dynamic scope in `multi.el'

  (eval
   ;; NOTICE that we must `quote' the form for this to work!!!
   '(multi-test (bar)
      (let ((hierarchy (make-multi-hierarchy)))
        ;; override :rect rel in custom hierarchy
        (multi-rel :rect isa :parallelogram in hierarchy)
        (multi-rel :square isa :rect in hierarchy)
        ;; define multi-dispatch over the custom hierarchy
        (multi bar #'identity :in hierarchy)
        (multi-method bar (a) :when :parallelogram :parallelogram)
        (let ((hierarchy (make-multi-hierarchy)))
          (multi-rel :rect isa :shape in hierarchy)
          (multi-rel :square isa :rect in hierarchy)
          ;; Method calls should still use custom hierarchy, so that :rect and
          ;; :square are :parallelograms
          (should (equal :parallelogram (bar :rect)))
          (should (equal :parallelogram (bar :square)))))
      ;; even outside of both `let's we should get the same result
      (should (equal :parallelogram (bar :rect)))
      (should (equal :parallelogram (bar :square))))
   'lexical-scope))


;;** - multicase -------------------------------------------------- *;;


(ert-deftest multi-test-simple-multi-case-patterns ()
  "Multi-Case should match simple patterns"

  (should (equal 'match (multi-case '(a)
                          (_ 'match))))

  (should (equal '(a) (multi-case '(a)
                        (lst lst))))

  (should (equal '(a) (multi-case '(a)
                        ([x y] (list x y))
                        ([x] (list x)))))

  (should (equal 'empty (multi-case '()
                          ([x] x)
                          (otherwise 'empty))))

  (should (equal 'match (multi-case '(a b c)
                          (['a _ 'c] 'match))))

  (defmacro multi-case--clause-test (expr pat &rest body)
    (declare (indent 1))
    `(pcase ,expr
       ,(multi-case--clause (cons pat body))
       (otherwise
        'no-match)))

  (should (equal 'match (multi-case--clause-test '()
                          [] 'match)))

  (should (equal 'symbol (multi-case--clause-test '(a)
                           ['a] 'symbol)))

  (should (equal 'b (multi-case--clause-test '(a b)
                      [(or 'a 'b) (and x 'b)] x)))

  (should (equal 'match (multi-case--clause-test '(:key)
                          [:key] 'match)))

  (should (equal :key (multi-case--clause-test '(:key)
                        [x] x)))

  (should (equal '(a :over b) (multi-case--clause-test '(a (:over) b)
                                [x (or [rel] :to) y] (list x rel y)))))


(ert-deftest multi-test-standard-multi-case-patterns ()
  "Multi-Case should allow standard pcase patterns"

  (should (equal '(a over b none) (multi-case '(a over b)
                                    ([x
                                      (and (pred symbolp)
                                           (or 'over 'to) rel)
                                      y]
                                     (list x rel y 'none)))))

  (should (equal '(1 2 3) (multi-case '(1 2)
                            ([(and (pred numberp)
                                   (app 1- 0)
                                   x)
                              (and num
                                   (let y 3))]
                             (list x num y))))))


(ert-deftest multi-test-multi-case-rest-patterns ()
  "Multi-Case should allow matching the rest of a list"

  (should (equal '(b c) (multi-case '(a b c)
                          (['a &rest tail] tail))))

  (should (equal 'c (multi-case '(a b c)
                      (['a &rest ['b last]] last))))

  (should (equal 'c (multi-case '(a b c)
                      (['a &rest (or [] ['b last])] last))))

  (should (equal 'match (multi-case '(a)
                          (['a &rest (or [] ['b last])] 'match))))

  (should (equal '(a b h) (multi-case '(a :over b :in h)
                            ([x (or :over :to) y &rest (or [:in z] [])]
                             (list x y (or z 'none)))))))


(ert-deftest multi-test-multi-case-nested-list-patterns ()
  "Multi-Case should allow nested list patterns"

  (should (equal '(2 3) (multi-case '(1 (2 3))
                          ([_ [a b]] (list a b)))))

  (should (equal '(1 2 3 4) (multi-case '((1 . 2) (3 . 4))
                              ([[a &rest b]
                                [(and (pred numberp) c)
                                 &rest
                                 (and (pred numberp) d)]]
                               (list a b c d))))))


(ert-deftest multi-test-multi-case-vector-patterns ()
  "Multi-Case should simple vector patterns"

  (should (equal '(1 2)
                 (multi-case [1 2]
                   (`[b c] (list b c)))))

  (should (equal '(b c)
                 (multi-case (list 'a [b c])
                   ([a `[b c]] (list b c))))))

(ert-deftest multi-test-multi-case-errors ()
  "Multi-Case should signal malformed patterns"
  (should
   (multi--error-match "in multi-case malformed &rest" (multi-case '(a b c)
                                                        (['a &rest foo bar] 'oops)))))


;;* Perf ---------------------------------------------------------- *;;


(defmacro multi-test-time (&rest body)
  (declare (indent defun))
  `(let ((start (float-time)))
     ,@body
     (- (float-time) start)))

;; TODO Test multi-method isa vs struct inheritance?

;; NOTE Rather ugly way to measure performance, but does the trick. We stack up
;; multi-methods against built-in generic dispatch. Our generic dispatches on the
;; type of its first argument, so to compare apples to apples we use #'type-of as
;; our multi-dispatch. For multi-methods we run 10'000 repeats, each repeat
;; performs 7 dispatches for a total of 70'000 dispatches. We do the same for
;; generic dispatch.
;;
;; Without caching multi-methods are ~100x slower:
;;
;; (multi-test-perf)
;; =>
;; ((:multi-method (:total . 2.364870071411133)
;;                (:average . 3.3783858163016185e-05))
;;  (:defmethod   (:total . 0.021378040313720703)
;;                (:average . 3.0540057591029576e-07)))


(defun multi-test-perf ()
  (multi foo-test #'type-of)
  (multi-method foo-test (x) :when 'foo-struct-1 1)
  (multi-method foo-test (x) :when 'foo-struct-2 2)
  (multi-method foo-test (x) :when 'foo-struct-3 3)
  (multi-method foo-test (x) :when 'foo-struct-4 4)
  (multi-method foo-test (x) :when 'foo-struct-5 5)
  (multi-method foo-test (x) :when 'foo-struct-6 6)
  (multi-method foo-test (x) :when :default 0)

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
       (let* ((total (multi-test-time
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

      (:multi-method
       (let* ((total (multi-test-time
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


 (ert-deftest multi-test-rel ()
   "Creating `multi-isa?' hierachy should work"
   (multi-test ((set= multi--set-equal?))
     (expect '(:rect :shape)         set= (ht-keys (multi-rel :rect isa :shape)))
     (expect '(:rect :shape :square) set= (ht-keys (multi-rel :square isa :rect)))

     (expect :shape  member (multi-global-hierarchy :rect :parents))
     (expect :rect   member (multi-global-hierarchy :square :parents))
     (expect :square member (multi-global-hierarchy :rect :children))))


 (ert-deftest multi-test-relationships ()
   "Retrieving parents, ancestors, descendants should work"
   (multi-test ((set= multi--set-equal?))
     (expect '(:shape)                      set= (multi-parents :rect) :after (multi-rel :rect isa :shape))
     (expect '(:rect)                       set= (multi-parents :square) :after (multi-rel :square isa :rect))
     (expect '(:parallelogram :rect :shape) set= (multi-ancestors :square) :after (multi-rel :square isa :parallelogram))
     (expect '(:rect :square)               set= (multi-descendants :shape))))


 (ert-deftest multi-test-isa-hierarchy ()
   (multi-test ()
     (multi-rel :rect isa :shape)
     (multi-rel :square isa :rect)
     (expect '(:generation . 0)                     equal (multi-isa? 42 42))
     (expect '(:generation . 1)                     equal (multi-isa? :rect :shape))
     (expect '(:generation . 2)                     equal (multi-isa? :square :shape))
     (expect '((:generation . 1) (:generation . 1)) equal (multi-isa? [:square :rect] [:rect :shape]))
     (expect '((:generation . 1) (:generation . 0)) equal (multi-isa? [:square :shape] [:rect :shape]))
     (expect (null (multi-isa? [:square :rect] [:shape :square])))
     (expect (null (multi-isa? [:square] :rect)))
     (expect (null (multi-isa? [:square] [])))))


 (ert-deftest multi-test-multi ()
   "Defining new multi dispatcher should work"
   (multi-test ((set= multi--set-equal?) foo)
     (expect (null (multi-methods 'foo)) :after (multi foo #'identity))
     (expect '(:default)                set=         (ht-keys (multi-methods 'foo)))
     (expect (functionp 'foo))
     (expect (functionp (multi-methods 'foo :default)))))


 (ert-deftest multi-test-multi-method ()
   "Installing and removing `multi-method's should work"
   (multi-test ((set= multi--set-equal?) foo)
     (multi foo #'identity)
     (expect '(:a :default)    set= (ht-keys (multi-methods 'foo)) :after (multi-method foo (x) :when :a :a))
     (expect '(:a :b :default) set= (ht-keys (multi-methods 'foo)) :after (multi-method foo (x) :when :b :b))

     ;; one method for every match
     (expect '(:a) set= (mapcar #'car (multi-methods :for 'foo :matching :a)))
     (expect '(:b) set= (mapcar #'car (multi-methods :for 'foo :matching :b)))

     ;; :default method when no method installed
     (expect '(:default) set= (mapcar #'car (multi-methods :for 'foo :matching :c)))
     ;; but no longer :default when installed
     (expect '(:c)       set= (mapcar #'car (multi-methods :for 'foo :matching :c)) :after (multi-method foo (x) :when :c :c))

     ;; methods must be functions
     (expect #'functionp cl-every (ht-values (multi-methods 'foo)))

     ;; removing a multi-method should work
     (multi-methods-remove 'foo :a)
     (should (multi--set-equal? '(:default) (mapcar #'car (multi-methods :for 'foo :matching :a))))))


 (ert-deftest multi-test-equality-dispatch ()
   "Basic equality based dispatch should work"
   (multi-test (foo)
     (multi foo #'identity)
     (expect :a       equal (foo :a) :after (multi-method foo (x) :when :a :a))
     (expect :b       equal (foo :b) :after (multi-method foo (x) :when :b :b))

     ;; :default when method missing
     (expect :default equal (foo :c) :after (multi-method foo (x) :when :default :default))

     ;; no :default when installed
     (expect :c       equal (too :c) :after (multi-method foo (x) :when :c :c))

     ;; back to :default when removed
     (expect :default equal (foo :c) :after (multi-methods-remove 'foo :c))))


 (ert-deftest multi-test-isa-dispatch ()
   "Full isa dispatch should work"
   (multi-test (foo)
     ;; Example from the multi-method docs.
     (multi-rel 'vector :isa :collection)
     (multi-rel 'hash-table :isa :collection)
     (multi foo #'type-of)
     (multi-method foo (c) :when :collection :a-collection)
     (multi-method foo (s) :when 'string :a-string)

     (expect :a-collection equal (foo []))
     (expect :a-collection equal (foo (ht)))
     (expect :a-string     equal (foo "bar"))))


 (ert-deftest multi-test-errors ()
   "Error conditions should be signaled and possible to catch"
   (multi-test (foo bar)

     (expect "multi-error" equal (get 'multi-error 'error-message))
     (expect-error 'multi-error :after (multi-error "foo %s" 'bar))

     (multi-rel :rect isa :shape)
     (multi-rel :square isa :rect)
     (multi-rel :square isa :parallelogram)
     (multi foo #'identity)
     (multi-method foo (a) :when :square :square)
     (multi-method foo (a) :when :shape :shape)

     ;; signal ambiguous methods
     (expect "multiple methods" multi--error-match (foo :square))

     ;; preinstalled :default method should signal method missing
     (expect "no multi-methods match" multi--error-match (foo :triangle))

     ;; catch cycle relationships
     (expect "cycle relationship" multi--error-match (multi-rel :shape isa :square))

     ;; catch malformed arglist in `multi' call
     (expect "malformed arglist" multi--error-match (multi bar :val [a b]))

     ;; catch malformed arglist in `multi-method' call
     (expect "malformed arglist" multi--error-match (multi-method bar :val [a b]))))
 ;; comment
 )
