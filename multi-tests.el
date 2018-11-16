;; -*- lexical-binding: t; -*-


(require 'ert)
(require 'cl-lib)
(require 'multi)


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
    `(cl-letf ((multi-global-hierarchy (ht))
               (multi-methods          (ht))
               ,@multis)
       ,@body
       ;; restore globals
       )))


(example
 (list
  (multi--symbol-function 'bar)
  (multi-test (bar)
    (multi bar [a] a)
    (multimethod bar (a) :when :rect 'rect)
    (multimethod bar (a) :when :shape 'shape)
    (multi--symbol-function 'bar))
  (multi--symbol-function 'bar))
 ;; example
 )


(cl-defmacro multi--error-match (prefix &rest body)
  "Try catching multi-error thrown by BODY and test if its
message prefix matches PREFIX"
  `(string-prefix-p
    ,prefix
    (condition-case err
        ,@body
      (multi-error (cadr err)))
    'ignore-case))


(example
 (should (multi--error-match "foo" (multi-error "foo %s" 'bar)))
 ;; example
 )


;;* Tests --------------------------------------------------------- *;;


(ert-deftest multi-test-hierarchy ()
  "Creating `multi-isa?' hierachy should work"
  (multi-test ()
    (multi-rel :rect isa :shape)
    (multi-rel :square isa :rect)
    (should (equal '(:generation . 0) (multi-isa? 42 42)))
    (should (equal '(:generation . 1) (multi-isa? :rect :shape)))
    (should (equal '(:generation . 2) (multi-isa? :square :shape)))
    (should (equal '((:generation . 1) (:generation . 1)) (multi-isa? [:square :rect] [:rect :shape])))
    (should (equal '((:generation . 1) (:generation . 0)) (multi-isa? [:square :shape] [:rect :shape])))
    (should (null (multi-isa? [:square :shape] [:rect :shape])))
    (should (null (multi-isa? [:square :rect] [:shape :square])))
    (should (null (multi-isa? [:square] :rect)))
    (should (null (multi-isa? [:square] [])))))


;; TODO replace `cl-set-exclusive-or' with set equality test
(ert-deftest multi-test-core-api-functions
    "Core API functions should work"
  (multi-test (simple2)
    (multi simple2 #'identity)
    (multimethod simple2 [x] :when :a :a)
    (multimethod simple2 [x] :when :b :b)
    ;; dispatch is a function
    (should (functionp 'simple2))
    ;; two methods should've been installed
    (should (null (cl-set-exclusive-or '(:a :b) (ht-keys (ht-get multi-methods 'simple2)))))
    ;; each for the correct value
    (should (equal :a (caar (multi-methods :for 'simple2 :matching :a))))
    (should (equal :b (caar (multi-methods :for 'simple2 :matching :b))))
    (should (equal :default (caar (multi-methods :for 'simple2 :matching :c))))
    ;; adding another method
    (multimethod simple2 [x] :when :c :c)
    ;; should be reflected in the table
    (should (null (cl-set-exclusive-or '(:a :b :c) (ht-keys (ht-get multi-methods 'simple2)))))
    ;; methods must be functions
    (should (functionp (cdar (multi-methods :for 'simple2 :matching :a))))
    (should (functionp (cdar (multi-methods :for 'simple2 :matching :b))))
    (should (functionp (cdar (multi-methods :for 'simple2 :matching :c))))
    ;; retrieving relationships should work
    (multi-rel :rect isa :shape)
    (multi-rel :square isa :rect)
    (multi-rel :square isa :parallelogram)
    ;; TODO use set equality test instead
    (should (equal '(:shape) (multi-parents :rect)))
    (should (equal '(:parallelogram :rect) (multi-parents :square)))
    (should (equal '(:parallelogram :rect :shape) (multi-ancestors :square)))
    (should (equal '(:rect :square) (multi-descendants :shape)))))


(ert-deftest multi-test-basic-multimethods ()
  "Basic equality based dispatch should work"
  (multi-test (too-simple)
    (multi too-simple #'identity)
    (multimethod too-simple [x] :when :a :a)
    (multimethod too-simple [x] :when :b :b)
    (multimethod too-simple [x] :when :default :default)
    (should (equal :a (too-simple :a)))
    (should (equal :b (too-simple :b)))
    (should (equal :default (too-simple :c)))
    (comment
     ;; TODO Removing a method works
     (remove-method too-simple :a)
     (should (equal :default (too-simple :a)))
     ;; comment
     )
    ;; Adding another method works
    (multimethod too-simple :d [x] :d)
    (should (equal :d (too-simple :d)))))


(ert-deftest multi-test-isa-dispatch
    "`multi-isa?' dispatch should work"
  (multi-test (foo)
    ;; Example from the multimethod docs.
    (multi-rel 'vector :isa :collection)
    (multi-rel 'hash-table :isa :collection)
    (multi foo #'type-of)
    (multimethod foo :collection [c] :a-collection)
    (multimethod foo 'string [s] :a-string)
    (should (equal :a-collection (foo [])))
    (should (equal :a-collection (foo (ht))))
    (should (equal :a-string (foo "bar")))))


(ert-deftest multi-test-ambiguous-methods
    "Dispatch ambiguity should be caught or preferred away"
  (multi-test (bar)
    ;; Example from the multimethod docs.
    (multi-rel :rect isa :shape)
    ;; TODO Whould this work?
    ;; (multi bar #'vector)
    (multi bar (fn (x y) (vector x y)))
    (multimethod bar [:rect :shape] (x y) :rect-shape)
    (multimethod bar [:shape :rect] (x y) :shape-rect)
    (should-error (bar :rect :rect) :type 'multi-error)
    (comment
     ;; TODO `multi-prefer'

     ;; The prefers method returns empty table w/ no prefs
     (should (null (multi-prefers bar)))

     ;; Adding a preference to resolve it dispatches correctly
     (multi-prefer bar [:rect :shape] :to [:shape :rect])
     ;; or
     ;; (multi-prefer bar [:rect :shape] :over [:shape :rect])
     (should (equal :rect-shape (bar :rect :rect)))

     ;; The prefers method now returns the correct table
     (should (equal (ht ([:rect :shape] '([:shape :rect]))) (multi-prefers bar)))
     ;; comment
     )))


(ert-deftest multi-test-errors
    "Error conditions should be signaled and possible to catch"
  (multi-test (foo bar)
    (should (equal "multi-error" (get 'multi-error 'error-message)))
    (should-error (multi-error "foo %s" 'bar) :type 'multi-error)
    (multi-rel :rect isa :shape)
    (multi-rel :square isa :rect)
    (multi-rel :square isa :parallelogram)
    (multi foo #'identity)
    (multimethod foo (a) :when :square :square)
    (multimethod foo (a) :when :shape :shape)
    ;; signal ambiguous methods
    (should (multi--error-match "multiple methods" (foo :square)))
    ;; preinstalled :default method should signal method missing
    (should (multi--error-match "no multimethods match" (foo :triangle)))
    ;; catch cycle relationships
    (should (multi--error-match "cycle relationship" (multi-rel :shape isa :square)))
    ;; catch malformed arglist in `multi' call
    (should (multi--error-match "malformed arglist" (multi bar :val [a b])))
    ;; catch malformed arglist in `multimethod' call
    (should (multi--error-match "malformed arglist" (multimethod bar :val [a b])))))


(ert-deftest multi-test-default-methods
    "Default methods should work"
  (multi-test (foo)
    (multi-rel :rect isa :shape)
    (multi foo #'identity)
    (multimethod foo (a) :when :rect :rect)
    ;; preinstalled :default method should signal method missing
    (should (multi--error-match "no multimethods match" (foo :triangle)))
    ;; custom :default method should work
    (multimethod foo (a) :when :default :shape)
    (should (equal :rect (foo :square)))))


(comment
 (should ..)
 (should-not ..)
 (should-error .. :type 'multi-error)

 (ert-deftest multi-prefer-method ()
   "`multi-prefer-method' should resolve ambiguitise"
   :expected-result :failed
   ...)

 ;; skip test on condition
 (ert-deftest test-dbus ()
   ""
   (skip-unless (featurep 'dbusbind))
   ...)
 ;; comment
 )
