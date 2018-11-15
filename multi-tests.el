;; -*- lexical-binding: t; -*-


(require 'ert)
(require 'cl-lib)
(require 'multi)


;;* Prelude


(defun multi--symbol-function (s)
  "Like `symbol-function' but returns :unbound when S is
unbound."
  (declare
   (gv-setter (lambda (val)
                `(case ,val
                   ((:unbind :unbound) (fmakunbound ,s))
                   (otherwise (fset ,s ,val))))))
  (or (symbol-function s) :unbound))


(defmacro multi-test (multis &rest body)
  "Set `multi-global-hierarchy' and `multi-methods' to empty
tables and unbind functions in the MULTIS list for the extent of
BODY, allowing it to bind them as needed. Restore everything
after BODY."
  (declare (indent defun))
  (let ((multis (mapcar (fn (m) `((multi--symbol-function ',m) :unbind)) multis)))
    `(cl-letf ((multi-global-hierarchy (ht))
               (multi-methods (ht))
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


;;* Tests


;; TODO I don't like Clojure tests. Should do better.


(ert-deftest multi-test-basic-multimethods ()
  "Check basic dispatch"
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


(ert-deftest multi-test-isa
    "Check isa? dispatch"
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
    "Amubiguous method match dispatch signals an error"
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


(ert-deftest multi-test-core
    "Core function methods works"
  (multi-test (simple2)
    (multi simple2 #'identity)
    (multimethod simple2 [x] :when :a :a)
    (multimethod simple2 [x] :when :b :b) (multi-methods :for 'simple2)
    (should (null (cl-set-exclusive-or '(:a :b) (ht-keys (ht-get multi-methods 'simple2)))))
    (should (equal :a (car (multi-methods :for 'simple2 :matching :a))))
    (multimethod simple2 [x] :when :c :c)
    (should (null (cl-set-exclusive-or '(:a :b :c) (ht-keys (ht-get multi-methods 'simple2)))))
    (comment
     ;; TODO remove-method
     (multi-remove-method 'simple2 :a)
     (should (null (cl-set-exclusive-or '(:b :c) (ht-keys (ht-get multi-methods 'simple2)))))
     ;; comment
     )))


(ert-deftest multi-test-getting-a-method
    "Retrieving a method works"
  (multi-test (simple3)
    (multi simple3 #'identity)
    (multimethod simple3 [x] :when :a :a)
    (multimethod simple3 [x] :when :b :b)
    (should (functionp (cdar (multi-methods :for 'simple3 :matching :a))))
    (should (equal :a (caar (multi-methods :for 'simple3 :matching :a))))
    (should (functionp (cdar (multi-methods :for 'simple3 :matching :b))))
    (should (equal :b (caar (multi-methods :for 'simple3 :matching :b))))
    (should (equal :default (caar (multi-methods :for 'simple3 :matching :b))))))


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
