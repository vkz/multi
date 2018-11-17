;; NOTE I leave lexical binding off, cause I suspect my multi-test may on occasion
;; not work, cause it captures stuff lexically.


(require 'ert)
(require 'cl-lib)
(require 'multi)


;; TODO Perf

;; TODO Cache

;; TODO Would implementing expect macro as per examples below be worth it?

;; TODO ht-* functionality below exposes implementation, introduce relevant API
;; and replace all that hash-table junk. I should be able to swap underlying data
;; structures at will without breaking tests.


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
  (and (null (cl-set-difference s1 s2))
       (null (cl-set-difference s2 s1))))


(example
 (multi--set-equal? (list 1 2 3) (list 3 2 1))
 (multi--set-equal? (list 1 2 3) (list 3 2))
 (multi--set-equal? '() (list 1 2 3))
 ;; example
 )


;;* Tests --------------------------------------------------------- *;;


(ert-deftest multi-test-rel ()
  "Creating `multi-isa?' hierachy should work"
  (multi-test ()
    (should (multi--set-equal? '(:rect :shape) (ht-keys (multi-rel :rect isa :shape))))
    (should (multi--set-equal? '(:rect :shape :square) (ht-keys (multi-rel :square isa :rect))))
    (should (member :shape (ht-get* (multi-global-hierarchy) :rect :parents)))
    (should (member :rect (ht-get* (multi-global-hierarchy) :square :parents)))
    (should (member :square (ht-get* (multi-global-hierarchy) :rect :children)))))


(ert-deftest multi-test-ancestors-descendants ()
  "Retrieving parents, ancestors, descendants should work"
  (multi-test ()

    (multi-rel :rect isa :shape)
    (multi-rel :square isa :rect)
    (multi-rel :square isa :parallelogram)

    (should (multi--set-equal? '(:shape) (multi-parents :rect)))
    (should (multi--set-equal? '(:parallelogram :rect) (multi-parents :square)))
    (should (multi--set-equal? '(:parallelogram :rect :shape) (multi-ancestors :square)))
    (should (multi--set-equal? '(:rect :square) (multi-descendants :shape)))))


(ert-deftest multi-test-isa-hierarchy ()
  "Checking isa relationship should work"
  (multi-test ()

    (multi-rel :rect isa :shape)
    (multi-rel :square isa :rect)

    (should (equal '(:generation . 0) (multi-isa? 42 42)))
    (should (equal '(:generation . 1) (multi-isa? :rect :shape)))
    (should (equal '(:generation . 2) (multi-isa? :square :shape)))
    (should (equal '((:generation . 1) (:generation . 1)) (multi-isa? [:square :rect] [:rect :shape])))
    (should (equal '((:generation . 1) (:generation . 0)) (multi-isa? [:square :shape] [:rect :shape])))
    (should (null (multi-isa? [:square :rect] [:shape :square])))
    (should (null (multi-isa? [:square] :rect)))
    (should (null (multi-isa? [:square] [])))))


(ert-deftest multi-test-multi ()
  "Installing new `multi' dispatch function should work"
  (multi-test (foo)

    (multi foo #'identity)

    (should (ht-contains? multi-methods 'foo))
    (should (functionp 'foo))
    (should (multi--set-equal? '(:default) (ht-keys (ht-get multi-methods 'foo))))
    (should (functionp (ht-get* multi-methods 'foo :default)))))


(ert-deftest multi-test-multimethod ()
  "Installing and removing `multimethod's should work"
  (multi-test (foo)

    (multi foo #'identity)
    (multimethod foo (x) :when :a :a)

    (should (multi--set-equal? '(:a :default) (ht-keys (ht-get multi-methods 'foo))))

    (multimethod foo (x) :when :b :b)
    (should (multi--set-equal? '(:a :b :default) (ht-keys (ht-get multi-methods 'foo))))

    ;; one method for every match
    (should (multi--set-equal? '(:a) (mapcar #'car (multi-methods :for 'foo :matching :a))))
    (should (multi--set-equal? '(:b) (mapcar #'car (multi-methods :for 'foo :matching :b))))

    ;; :default method when no match installed
    (should (multi--set-equal? '(:default) (mapcar #'car (multi-methods :for 'foo :matching :c))))

    ;; but no longer :default when installed
    (multimethod foo (x) :when :c :c)
    (should (multi--set-equal? '(:c) (mapcar #'car (multi-methods :for 'foo :matching :c))))

    ;; methods must be functions
    (should (cl-every #'functionp (ht-values (ht-get multi-methods 'foo))))

    ;; TODO `multi-remove-method'
    ;; (multi-remove-method 'foo :a)
    ;; (should (multi--set-equal? '(:default) (mapcar #'car (multi-methods :for 'foo :matching :a))))
    ))


(ert-deftest multi-test-equality-dispatch ()
  "Basic equality based dispatch should work"
  (multi-test (foo)

    (multi foo #'identity)
    (multimethod foo (x) :when :a :a)
    (multimethod foo (x) :when :b :b)

    (should (equal :a (foo :a)))
    (should (equal :b (foo :b)))))


(ert-deftest multi-test-isa-dispatch ()
  "Full isa dispatch should work"
  (multi-test (foo)

    ;; Example from the multimethod docs.
    (multi-rel 'vector :isa :collection)
    (multi-rel 'hash-table :isa :collection)
    (multi foo #'type-of)
    (multimethod foo (c) :when :collection :a-collection)
    (multimethod foo (s) :when 'string :a-string)

    (should (equal :a-collection (foo [])))
    (should (equal :a-collection (foo (ht))))
    (should (equal :a-string (foo "bar")))))


(ert-deftest multi-test-ambiguous-methods ()
  "Dispatch ambiguity should be caught or preferred away"
  (multi-test (bar)

    ;; Example from the multimethod docs.
    (multi-rel :rect isa :shape)
    ;; TODO Whould this work?
    ;; (multi bar #'vector)
    (multi bar (fn (x y) (vector x y)))
    (multimethod bar (x y) :when [:rect :shape] :rect-shape)
    (multimethod bar (x y) :when [:shape :rect] :shape-rect)

    (should (multi--error-match "multiple methods" (bar :rect :rect)))

    ;; TODO `multi-prefer'
    ;; The prefers method returns empty table w/ no prefs
    ;; (should (null (multi-prefers bar)))
    ;; Adding a preference to resolve it dispatches correctly
    ;; (multi-prefer bar [:rect :shape] :to [:shape :rect])
    ;; or
    ;; (multi-prefer bar [:rect :shape] :over [:shape :rect])
    ;; (should (equal :rect-shape (bar :rect :rect)))
    ;; The prefers method now returns the correct table
    ;; (should (equal (ht ([:rect :shape] '([:shape :rect]))) (multi-prefers bar)))
    ))


(ert-deftest multi-test-default-method ()
  "Default method should work"
  (multi-test (foo)

    (multi foo #'identity)
    (multimethod foo (x) :when :a :a)

    ;; pre-installed :default when method missing
    (should (multi--error-match "no multimethods match" (foo :c)))

    ;; :default when method missing and :default installed
    (multimethod foo (x) :when :default :default)
    (should (equal :default (foo :c)))

    ;; no longer :default when installed
    (multimethod foo (x) :when :c :c)
    (should (equal :c (foo :c)))

    ;; TODO back to :default when removed
    ;; (multi-remove-method 'foo :c)
    ;; (should (equal :default (foo :c)))
    ))


(ert-deftest multi-test-errors ()
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
