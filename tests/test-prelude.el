;; -*- lexical-binding: t; -*-


;; Disable lexical-binding check for now
(setq mu-lexical-binding nil)

;; TODO I leave lexical binding off, cause I suspect my mu-test may on occasion
;; not work, cause it captures stuff lexically. I need to investigate cause I'd
;; rather have lexical scope here, too.


(require 'ert)
(require 'cl-lib)

(defun multi--load-el-if-newer (file)
  (let ((el file)
        (elc (concat (file-name-sans-extension file) ".elc")))
    (load-file (if (file-newer-than-file-p el elc) el elc))))

(multi--load-el-if-newer "../multi-prelude.el")
(multi--load-el-if-newer "../multi-patterns.el")
(multi--load-el-if-newer "../multi-methods.el")

;; TODO Would implementing expect macro as per examples below be worth it?

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
  (let ((multis (mapcar (lambda (m) `((mu--symbol-function ',m) :unbind)) multis)))
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


(mu-defun --by [field :on compare]
  "Create a `sort' compatible comparator that COMPAREs object
substructures extracted with FIELD."
  (lambda (&rest args)
    (apply compare (mapcar field args))))


;;* Perf --------------------------------------------------------- *;;


(defmacro mu-test-time (&rest body)
  (declare (indent defun))
  `(let ((start (float-time)))
     ,@body
     (- (float-time) start)))


;;* Playground --------------------------------------------------- *;;


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

 (ert-deftest mu-test-list-patterns ()
   "list patterns `lst' and `l' should work"

   (expect   'match     equal    (mu-case '(a)
                                   (_ 'match)))

   (expect   '(a)       equal    (mu-case '(a)
                                   (lst lst)))

   (expect   '(a)       equal    (mu-case '(a)
                                   ((l x y) (list x y))
                                   ((l x) (list x))))

   (expect   'empty     equal    (mu-case '()
                                   ((l x) x)
                                   (otherwise 'empty)))

   (expect   'match     equal    (mu-case '(a b c)
                                   ((l 'a _ 'c) 'match)))

   (expect   '(2 3)     equal    (mu-case '(1 (2 3))
                                   ((l _ (l a b)) (list a b))))

   (expect   'match     equal    (mu-case '((1 2))
                                   ((l (l 1 &rest (l (pred numberp)))) 'match)))

   (expect   '(1 2 3 4) equal    (mu-case '((1 2) (3 4))
                                   ((l (l (and (pred numberp) a) &rest (l b))
                                       (l (and (pred numberp) c) &rest (l d)))
                                    (list a b c d)))))

 (mu-test ()
   ;; TODO single expect should allow multiple tests in a `setq' like manner

   (expect '(1 2)       equal    (foo 1 2)
           '(1 2 3)     equal    (foo 1 2 3)

           :after (mu-defun foo (&rest args)
                    ([a b c] (list a b c))
                    ([a b] (list a b))))

   (expect  '(1 2 3 4)  equal    (foo 1 '(2 3) 4)
            '(1 2 3)    equal    (foo 1 '(2) 3)

            :after (mu-defun foo (&rest args)
                     ([a [b c] d] (list a b c d))
                     ([a [b] c] (list a b c)))))


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
