;; NOTE I leave lexical binding off, cause I suspect my multi-test may on occasion
;; not work, cause it captures stuff lexically.


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

    (should (functionp 'foo))
    (should (get 'foo :multi-methods))
    (should (null (ht-keys (get 'foo :multi-methods))))
    (should (functionp (get 'foo :multi-default)))))


(ert-deftest multi-test-multimethod ()
  "Installing and removing `multimethod's should work"
  (multi-test (foo)

    (multi foo #'identity)
    (should (get 'foo :multi-methods))

    (multimethod foo (x) :when :a :a)
    (should (multi--set-equal? '(:a) (ht-keys (get 'foo :multi-methods))))

    (multimethod foo (x) :when :b :b)
    (should (multi--set-equal? '(:a :b) (ht-keys (get 'foo :multi-methods))))

    ;; one method for every match
    (should (multi--set-equal? '(:a) (mapcar #'car (multi-methods :for 'foo :matching :a))))
    (should (multi--set-equal? '(:b) (mapcar #'car (multi-methods :for 'foo :matching :b))))

    ;; :default method when no match installed
    (should (multi--set-equal? '(:default) (mapcar #'car (multi-methods :for 'foo :matching :c))))

    ;; but no longer :default when installed
    (multimethod foo (x) :when :c :c)
    (should (multi--set-equal? '(:c) (mapcar #'car (multi-methods :for 'foo :matching :c))))

    ;; methods must be functions
    (should (cl-every #'functionp (ht-values (get 'foo :multi-methods))))

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
    (multi bar #'vector)
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

    ;; TODO removing custom :default should restore pre-installed :default
    ;; (multi-remove-method 'foo :default)
    ;; (should (multi--error-match "no multimethods match" (foo :d)))

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


;;* Perf ---------------------------------------------------------- *;;


(defmacro multi-test-time (&rest body)
  (declare (indent defun))
  `(let ((start (float-time)))
     ,@body
     (- (float-time) start)))

;; TODO Test multimethod isa vs struct inheritance?

;; NOTE Rather ugly way to measure performance, but does the trick. We stack up
;; multimethods against built-in generic dispatch. Our generic dispatches on the
;; type of its first argument, so to compare apples to apples we use #'type-of as
;; our multi-dispatch. For multimethods we run 10'000 repeats, each repeat
;; performs 7 dispatches for a total of 70'000 dispatches. We do the same for
;; generic dispatch.
;;
;; Without caching multimethods are ~100x slower:
;;
;; (multi-test-perf)
;; =>
;; ((:multimethod (:total . 2.364870071411133)
;;                (:average . 3.3783858163016185e-05))
;;  (:defmethod   (:total . 0.021378040313720703)
;;                (:average . 3.0540057591029576e-07)))


(defun multi-test-perf ()
  (multi foo-test #'type-of)
  (multimethod foo-test (x) :when 'foo-struct-1 1)
  (multimethod foo-test (x) :when 'foo-struct-2 2)
  (multimethod foo-test (x) :when 'foo-struct-3 3)
  (multimethod foo-test (x) :when 'foo-struct-4 4)
  (multimethod foo-test (x) :when 'foo-struct-5 5)
  (multimethod foo-test (x) :when 'foo-struct-6 6)
  (multimethod foo-test (x) :when :default 0)

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

      (:multimethod
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

     (expect :shape  member (ht-get* (multi-global-hierarchy) :rect :parents))
     (expect :rect   member (ht-get* (multi-global-hierarchy) :square :parents))
     (expect :square member (ht-get* (multi-global-hierarchy) :rect :children))))


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
     (expect (null (get 'foo :multi-methods)) :after (multi foo #'identity))
     (expect '(:default)                set=         (ht-keys (get 'foo :multi-methods)))
     (expect (functionp 'foo))
     (expect (functionp (ht-get* (get 'foo :multi-methods) :default)))))


 (ert-deftest multi-test-multimethod ()
   "Installing and removing `multimethod's should work"
   (multi-test ((set= multi--set-equal?) foo)
     (multi foo #'identity)
     (expect '(:a :default)    set= (ht-keys (get 'foo :multi-methods)) :after (multimethod foo (x) :when :a :a))
     (expect '(:a :b :default) set= (ht-keys (get 'foo :multi-methods)) :after (multimethod foo (x) :when :b :b))

     ;; one method for every match
     (expect '(:a) set= (mapcar #'car (multi-methods :for 'foo :matching :a)))
     (expect '(:b) set= (mapcar #'car (multi-methods :for 'foo :matching :b)))

     ;; :default method when no method installed
     (expect '(:default) set= (mapcar #'car (multi-methods :for 'foo :matching :c)))
     ;; but no longer :default when installed
     (expect '(:c)       set= (mapcar #'car (multi-methods :for 'foo :matching :c)) :after (multimethod foo (x) :when :c :c))

     ;; methods must be functions
     (expect #'functionp cl-every (ht-values (get 'foo :multi-methods)))

     ;; TODO `multi-remove-method'
     ;; (multi-remove-method 'foo :a)
     ;; (should (multi--set-equal? '(:default) (mapcar #'car (multi-methods :for 'foo :matching :a))))
     ))


 (ert-deftest multi-test-equality-dispatch ()
   "Basic equality based dispatch should work"
   (multi-test (foo)
     (multi foo #'identity)
     (expect :a       equal (foo :a) :after (multimethod foo (x) :when :a :a))
     (expect :b       equal (foo :b) :after (multimethod foo (x) :when :b :b))

     ;; :default when method missing
     (expect :default equal (foo :c) :after (multimethod foo (x) :when :default :default))

     ;; no :default when installed
     (expect :c       equal (too :c) :after (multimethod foo (x) :when :c :c))

     ;; TODO back to :default when removed
     ;; (expect :default equal (foo :c) :after (multi-remove-method 'foo :c))
     ))


 (ert-deftest multi-test-isa-dispatch ()
   "Full isa dispatch should work"
   (multi-test (foo)
     ;; Example from the multimethod docs.
     (multi-rel 'vector :isa :collection)
     (multi-rel 'hash-table :isa :collection)
     (multi foo #'type-of)
     (multimethod foo (c) :when :collection :a-collection)
     (multimethod foo (s) :when 'string :a-string)

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
     (multimethod foo (a) :when :square :square)
     (multimethod foo (a) :when :shape :shape)

     ;; signal ambiguous methods
     (expect "multiple methods" multi--error-match (foo :square))

     ;; preinstalled :default method should signal method missing
     (expect "no multimethods match" multi--error-match (foo :triangle))

     ;; catch cycle relationships
     (expect "cycle relationship" multi--error-match (multi-rel :shape isa :square))

     ;; catch malformed arglist in `multi' call
     (expect "malformed arglist" multi--error-match (multi bar :val [a b]))

     ;; catch malformed arglist in `multimethod' call
     (expect "malformed arglist" multi--error-match (multimethod bar :val [a b]))))
 ;; comment
 )