;; -*- lexical-binding: t; -*-


(load-file "test-prelude.el")


(ert-deftest mu-test-table-protocol ()
  (mu-test ()
    (mu-defstruct foo-struct props)
    (cl-defstruct bar-struct props)

    ;; methods should appear on protocol struct
    (should (mu-protocol-methods mu-table-protocol))

    ;; every protocol method is a generic
    (should (cl-every #'cl-generic-p (ht-keys (mu-protocol-methods mu-table-protocol))))

    (should (mu-implements? nil               mu-table-protocol))
    (should (mu-implements? t                 mu-table-protocol))
    (should (mu-implements? '(foo)            mu-table-protocol))
    (should (mu-implements? [1]               mu-table-protocol))
    (should (mu-implements? (ht) mu-table-protocol))
    (should (mu-implements? (make-mu-struct) mu-table-protocol))
    ;; any mu-defstruct inherits table protocol from mu-struct
    (should (mu-implements? (make-foo-struct) mu-table-protocol))
    ;; any cl-defstruct inherits table protocol from cl-structure-object
    (should (mu-implements? (make-bar-struct) mu-table-protocol))))


(ert-deftest mu-test-callable-protocol ()
  (mu-test ()
    (mu-defstruct foo-struct props)
    (cl-defstruct bar-struct props)

    (setq foo (make-foo-struct :props (ht (:a (ht (:b 1))))))
    (setq bar (make-bar-struct :props (ht (:a (ht (:b 1))))))

    ;; struct as function with default :call

    ;; mu-struct
    (should (eq 1 (mu.call foo :props :a :b)))
    (should (eq 1 (mu.apply foo :props '(:a :b))))
    ;; cl-struct
    (should (eq 1 (mu.call bar :props :a :b)))
    (should (eq 1 (mu.apply bar :props '(:a :b))))

    ;; mu-struct: set call
    (setf (mu. foo :call) (lambda (struct &rest args) 42))
    (should (eq 42 (mu.call foo :bar)))

    ;; cl-struct with no call slot: set call
    (should (mu--error-match
             "cl-struct of type bar-struct has no slot "
             (setf (mu. bar :call) (lambda (struct &rest args) 42))))
    (should-not (mu.call bar :foo))
    (should (mu.call bar :props :a :b))

    ;; cl-struct with call slot: set call
    (cl-defstruct bar-call-struct
      (call (lambda (self &rest args) 42))
      props)
    (should (eq 42 (mu.call (make-bar-call-struct :props :bar) :props)))

    ;; cl-struct with no call slot: set call
    (cl-defstruct bar-nocall-struct props)
    (mu-defcallable bar-nocall-struct (lambda (self &rest args) 42))
    (should (eq 42 (mu.call (make-bar-nocall-struct :props :bar) :props)))

    ;; hash-table as function
    (setq foo (ht (:a (ht (:b 1)))))
    (setq bar (ht (:call #'mu.)
                  (:a (ht (:b 1)))
                  (:foo foo)))
    (should (eq 1 (mu.call foo :a :b)))
    (should (eq 1 (mu.apply foo '(:a :b))))
    (should (eq 1 (mu.call bar :a :b)))
    (should (eq 1 (mu.apply bar :foo '(:a :b))))

    ;; normal function
    (should (equal '(:a :b) (mu.call #'list :a :b)))
    (should (equal '(:a :b) (mu.apply #'list :a '(:b))))))


(ert-deftest mu-test-defstruct ()
  ""
  (mu-test ()
    (mu-defstruct foo-struct props)

    ;; its a legitimate struct
    (should (recordp (make-foo-struct)))

    ;; struct predicate is defined
    (should (foo-struct? (make-foo-struct)))

    ;; it is a mu-struct
    (should (mu-struct? (make-foo-struct)))

    ;; struct slots have been stored
    (should (ht-get (get 'foo-struct :mu-slots) 'props))

    ;; getter is defined
    (should (functionp #'foo-struct))

    ;; setter is defined
    (should (functionp (function-get 'foo-struct 'gv-expander)))

    ;; mu.slots and mu.keys should work and return the same set
    (should (mu--set-equal? '(props -keys cl-tag-slot) (mu.slots (make-foo-struct))))
    (should (mu--set-equal? '(props -keys cl-tag-slot) (mu.keys  (make-foo-struct))))

    ;; with a missing key mu.keys should return it in addition to slots
    (let ((foo (make-foo-struct)))
      (setf (ht-get (mu-struct--keys foo) :missing) :key)
      (should (equal '(:missing)
                     (cl-set-difference (mu.keys foo)
                                        (mu.slots foo)))))))


(ert-deftest mu-test-struct-getters ()
  ""
  (mu-test ()
    ;; mu-struct
    (mu-defstruct foo-struct props)
    ;; cl-struct
    (cl-defstruct bar-struct props)

    ;; mu-struct getter should work
    (should (eq 1 (foo-struct (make-bar-struct :props 1) :props)))

    ;; mu-struct getter should work for nested structures
    (should (eq 1 (foo-struct (make-foo-struct :props (ht (:a (ht (:b 1))))) :props :a :b)))

    ;; looking up missing keys should work, not throw
    (should-not (foo-struct (make-foo-struct) :props :a :b))

    ;; getters should work for a mix of nested hash-tables and structs
    (should (eq 1 (let* ((foo (make-foo-struct :props (ht (:b 1))))
                         (bar (make-bar-struct :props (ht (:a foo)))))
                    (foo-struct foo 'props :b))))))


(ert-deftest mu-test-struct-setters ()
  ""
  (mu-test ()

    (mu-defstruct bazzer props)

    ;; set a slot
    (should (eq 2 (let ((baz (make-bazzer)))
                    (setf (bazzer baz :props) 2)
                    (bazzer baz :props))))

    ;; replace nested value in a slot
    (should (eq 2 (let ((baz (make-bazzer :props (ht (:a (ht (:b 1)))))))
                    (setf (bazzer baz :props :a :b) 2)
                    (bazzer baz :props :a :b))))


    ;; set nested key in a slot
    (should (eq 2 (let ((baz (make-bazzer :props (ht))))
                    (setf (bazzer baz :props :a) 2)
                    (bazzer baz :props :a))))

    ;; create nested keys in a slot
    (should (eq 2 (let ((baz (make-bazzer)))
                    (setf (bazzer baz :props :a :b) 2)
                    (bazzer baz :props :a :b))))

    ;; create nested keys not in a slot
    (should (eq 2 (let ((baz (make-bazzer)))
                    (setf (bazzer baz :missing :a :b) 2)
                    (bazzer baz :missing :a :b))))

    ;; attempting to set keys in non-associative should throw
    (should (mu--error-match "protocol mu-table-protocol does not extend"
                             (let ((baz (make-bazzer :props 2)))
                               (setf (bazzer baz :props :a) 2)
                               (bazzer baz :props))))))


(ert-deftest mu-test-mu. ()
  ""
  (mu-test ()
    ;; mu-struct
    (mu-defstruct bazzer props)
    ;; mu-struct
    (mu-defstruct foo-struct props)
    ;; cl-struct
    (cl-defstruct bar-struct props)

    ;; basic and nested lookup should work for hash-tables and structs
    (should (eq 1 (mu. (ht (:a 1)) :a)))
    (should (eq 1 (mu. (ht (:a (ht (:b 1)))) :a :b)))
    (should (eq 1 (mu. (make-foo-struct :props (ht (:a (ht (:b 1))))) :props :a :b)))
    (should (eq 1 (mu. (make-bar-struct :props (ht (:a (ht (:b 1))))) :props :a :b)))

    ;; looking up missing keys should work, not throw
    (should-not (mu. (ht (:a (ht (:b 1)))) :a :c))

    ;; mix of nested hash-tables and structs should work
    (should (eq 1 (let* ((foo (make-foo-struct :props (ht (:b 1))))
                         (bar (make-bar-struct :props (ht (:a foo)))))
                    (mu. bar :props :a :props :b))))


    ;; set a slot
    (should (eq 2 (let ((baz (make-bazzer)))
                    (setf (mu. baz :props) 2)
                    (mu. baz :props))))

    ;; nest keys in a slot
    (should (equal '(2 2) (let ((baz (make-bazzer)))
                            ;; add new nested key
                            (setf (mu. baz :props :a :b) 1)
                            ;; replace nested key
                            (setf (mu. baz :props :a :b) 2)
                            ;; add nested key to sub-maps
                            (setf (mu. baz :props :a :c :d) 2)
                            ;; result
                            (list (mu. baz :props :a :b)
                                  (mu. baz :props :a :c :d)))))


    ;; nest keys not in a slot
    (should (eq 2 (let ((baz (make-bazzer)))
                    (setf (mu. baz :missing :a :b) 2)
                    (mu. baz :missing :a :b))))


    ;; attempting to set keys in non-associative should throw
    (should (mu--error-match "protocol mu-table-protocol does not extend"
                             (let ((baz (make-bazzer :props 2)))
                               (setf (bazzer baz :props :a) 2)
                               (bazzer baz :props))))))
