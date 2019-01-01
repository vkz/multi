;; -*- lexical-binding: t; -*-


(load-file "test-prelude.el")


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


;;* internals ---------------------------------------------------- *;;


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
    (setf (mu-methods 'foo :rect) (lambda (x) :shape))
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


;;* hierarchies -------------------------------------------------- *;;


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


;;* defmethod ---------------------------------------------------- *;;


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


(ert-deftest mu-test-multi-methods ()
  "multi-methods should work"
  (mu-test (foo)

    ;; regular function dispatch
    (mu-defmulti foo #'vector)

    (mu-defmethod foo (a b) :when [:a :b] [:a :b])
    (mu-defmethod foo (a b) :when [:c :d] [:c :d])

    (should (equal [:a :b] (foo :a :b)))
    (should (equal [:c :d] (foo :c :d)))
    (should (mu--error-match "no mu-methods match" (foo :a :d)))


    ;; regular defun dispatch
    (mu-defmulti foo (&rest args)
      "docstring"
      :in mu-global-hierarchy
      (apply #'vector args))

    (mu-defmethod foo (a b) :when [:a :b] [:a :b])
    (mu-defmethod foo (a b) :when [:c :d] [:c :d])

    (should (equal [:a :b] (foo :a :b)))
    (should (equal [:c :d] (foo :c :d)))


    ;; single-head dispatch, simple methods
    (mu-defmulti foo [_ [arg]]
      "docstring"
      :in mu-global-hierarchy
      arg)

    (mu-defmethod foo (a b) :when 1 1)
    (mu-defmethod foo (a b) :when 2 2)

    (should (eq 1 (foo 0 [1])))
    (should (eq 2 (foo 0 [2])))
    (should (mu--error-match "no mu-methods match" (foo 0 [3])))


    ;; mu-lambda dispatch, destructuring methods
    (mu-defmulti foo (mu [_ [arg]] arg)
      "docstring"
      :in mu-global-hierarchy)

    (mu-defmethod foo [[a] _] :when 1 (list a))
    (mu-defmethod foo (mu [[a b] _] (list a b)) :when 2)

    (should (equal '(a) (foo [a] [1])))
    (should (equal '(a b) (foo [a b] [2])))


    ;; mu-lambda dispatch, multi-head method
    (mu-defmulti foo (mu [_ [arg]] arg)
      "docstring"
      :in mu-global-hierarchy)

    (mu-defmethod foo [[a] _] :when 1 (list a))
    (mu-defmethod foo (a b) :when 2
      ([[a] _] (list a))
      ([[a b] _] (list a b)))

    (should (equal '(a) (foo [a] [1])))
    (should (equal '(a b) (foo [a b] [2])))


    ;; multi-head dispatch, simple methods
    (mu-defmulti foo (&rest args)
      "docstring"
      :in mu-global-hierarchy
      ([a] a)
      ([a b] b))

    (mu-defmethod foo (&rest args) :when 1 1)
    (mu-defmethod foo (&rest args) :when 2 2)

    (should (eq 1 (foo 1)))
    (should (eq 2 (foo 1 2)))

    ;; example
    ))


;;* dispatch ----------------------------------------------------- *;;


(ert-deftest mu-test-equality-dispatch ()
  "Basic equality based dispatch should work"
  (mu-test (foo)

    (mu-defmulti foo (lambda (&rest args) (apply #'vector args)))
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


;;* prefers ------------------------------------------------------ *;;


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


;;* errors ------------------------------------------------------- *;;


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
    (should (equal '(:square :rect :shape) (mu--cycle? :shape :square)))))


;;* custom-hierarchies ------------------------------------------- *;;


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


;;* perf --------------------------------------------------------- *;;


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

