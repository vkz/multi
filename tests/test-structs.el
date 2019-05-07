;; -*- lexical-binding: t; -*-

;; Copyright (C) 2018, 2019 by Vlad Kozin

(load-file "test-prelude.el")


(ert-deftest mu-test-table-protocol ()
  (mu-test ()
    (mu-defstruct foo-struct (name :foo) props)
    (cl-defstruct (bar-struct (:constructor bar-struct-create)) props)

    ;; methods should appear on protocol struct
    (should (mu-protocol-methods mu-table-protocol))

    ;; every protocol method is a generic
    (should (cl-every #'cl-generic-p (ht-keys (mu-protocol-methods mu-table-protocol))))

    (should (mu-implements? nil               mu-table-protocol))
    (should (mu-implements? t                 mu-table-protocol))
    (should (mu-implements? '(foo)            mu-table-protocol))
    (should (mu-implements? [1]               mu-table-protocol))
    (should (mu-implements? (ht) mu-table-protocol))
    (should (mu-implements? (mu-struct-create) mu-table-protocol))
    ;; any mu-defstruct inherits table protocol from mu-struct
    (should (mu-implements? (foo-struct-create) mu-table-protocol))
    ;; any cl-defstruct inherits table protocol from cl-structure-object
    (should (mu-implements? (bar-struct-create) mu-table-protocol))))


(ert-deftest mu-test-callable-protocol ()
  (mu-test ()
    (mu-defstruct foo-struct (name :foo) props)
    (cl-defstruct (bar-struct (:constructor bar-struct-create)) props)

    (setq foo (foo-struct-create :props (ht (:a (ht (:b 1))))))
    (setq bar (bar-struct-create :props (ht (:a (ht (:b 1))))))

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
    (cl-defstruct (bar-call-struct (:constructor bar-call-struct-create))
      (call (lambda (self &rest args) 42))
      props)
    (should (eq 42 (mu.call (bar-call-struct-create :props :bar) :props)))

    ;; cl-struct with no call slot: set call
    (cl-defstruct (bar-nocall-struct (:constructor bar-nocall-struct-create)) props)
    (mu-defcallable bar-nocall-struct (lambda (self &rest args) 42))
    (should (eq 42 (mu.call (bar-nocall-struct-create :props :bar) :props)))

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


(ert-deftest mu-test-equatable-protocol ()
  ""
  (mu-test ()
    (mu-defstruct foo-struct (name :foo) props)
    (cl-defstruct (bar-struct (:constructor bar-struct-create)) props)

    (should (mu.equal '() '()))
    (should (mu.equal '(1) '(1)))
    (should (mu.equal (ht) (ht)))
    (should (mu.equal (ht (0 1)) (ht (0 1))))

    ;; mu-structs
    (should (mu.equal (foo-struct-create) (foo-struct-create)))
    ;; cl-structs
    (should (mu.equal (bar-struct-create) (bar-struct-create)))

    (should-not (mu.equal '(1) '()))
    (should-not (mu.equal '(1) (ht (0 1))))
    (should-not (mu.equal (ht) (ht (0 1))))
    (should-not (mu.equal (foo-struct-create :name :not-foo) (foo-struct-create)))
    (should-not (mu.equal (bar-struct-create) (foo-struct-create)))))


(ert-deftest mu-test-defstruct ()
  ""
  (mu-test ()
    (mu-defstruct foo-struct (name :foo) props)

    ;; its a legitimate struct
    (should (recordp (foo-struct-create)))

    ;; struct predicate is defined
    (should (foo-struct? (foo-struct-create)))

    ;; it is a mu-struct
    (should (mu-struct? (foo-struct-create)))

    ;; struct slots have been stored
    (should (ht-get (get 'foo-struct :mu-slots) 'props))

    ;; getter is defined
    (should (functionp #'foo-struct))

    ;; setter is defined
    (should (functionp (function-get 'foo-struct 'gv-expander)))

    ;; mu.slots and mu.keys should work and return the same set
    (should (mu--set-equal? '(name props) (mu.slots (foo-struct-create))))
    (should (mu--set-equal? '(name props) (mu.keys (foo-struct-create))))

    ;; with a missing key mu.keys should return it in addition to slots
    (let ((foo (foo-struct-create)))
      (setf (ht-get (mu-struct--keys foo) :missing) :key)
      (should (equal '(:missing)
                     (cl-set-difference (mu.keys foo)
                                        (mu.slots foo)))))

    ;; inheritance with overriding defaults
    (mu-defstruct (baz-struct
                   (:include foo-struct (name :baz)))
      val)

    ;; no default make-struct constructor
    (should-not (functionp #'make-baz-struct))

    ;; struct-create constructor instead
    (should (functionp #'baz-struct-create))

    ;; should satisfy type-predicates
    (should (baz-struct? (baz-struct-create)))
    (should (foo-struct? (baz-struct-create)))
    (should (mu-struct? (baz-struct-create)))

    ;; should store constructor for mu.new
    (should (get 'baz-struct :mu-constructor))

    ;; should override parent's defaults
    (should (eq :baz (baz-struct-name (baz-struct-create))))

    ;; mu.new constructor should work
    (should (eq 42 (baz-struct-val (mu.new baz-struct :val 42))))

    ;; signal when there're custom :constructors but no :new
    (should-error
     (mu-defstruct (baz-struct (:include foo-struct (name :baz))
                               (:constructor baz-struct--create))
       val)
     :type 'mu-error)

    ;; custom constructor and new
    (mu-defstruct (baz-custom-struct
                   (:include foo-struct (name :baz))
                   (:constructor baz-custom-struct--create)
                   (:new baz-custom-struct--create))
      val)

    (should-not (functionp #'baz-custom-struct-create))
    (should (functionp #'baz-custom-struct--create))

    ;; mu.new works with custom :new
    (should (eq 42 (baz-custom-struct-val (mu.new baz-custom-struct :val 42))))

    ;; implement protocols with :implements option; both `name' and `props' slot
    ;; ids must be bound to their respective slot values in method bodies
    (mu-defstruct foobar name props

      :implements mu-table-protocol
      (defmethod mu--get (foo key) (list name props))
      (defmethod mu--set (foo key val) (list name props))

      :implements mu-callable-protocol
      (defmacro mu--call (f args) (list name props)))

    (should (mu-implements? (foobar-create) mu-table-protocol))
    (should (mu-implements? (foobar-create) mu-callable-protocol))
    (should (equal '(:foo 42) (mu--get (foobar-create :name :foo :props 42) :key)))
    (should (equal '(:foo 42) (mu--set (foobar-create :name :foo :props 42) :key :val)))
    (should (equal '(:foo 42) (mu--call (foobar-create :name :foo :props 42) '(1 2 3))))))


(ert-deftest mu-test-struct-getters ()
  ""
  (mu-test ()
    ;; mu-struct
    (mu-defstruct foo-struct (name :foo) props)
    ;; cl-struct
    (cl-defstruct (bar-struct (:constructor bar-struct-create)) props)

    ;; mu-struct getter should work
    (should (eq 1 (foo-struct (bar-struct-create :props 1) :props)))

    ;; mu-struct getter should work for nested structures
    (should (eq 1 (foo-struct (foo-struct-create :props (ht (:a (ht (:b 1))))) :props :a :b)))

    ;; looking up missing keys should work, not throw
    (should-not (foo-struct (foo-struct-create) :props :a :b))

    ;; getters should work for a mix of nested hash-tables and structs
    (should (eq 1 (let* ((foo (foo-struct-create :props (ht (:b 1))))
                         (bar (bar-struct-create :props (ht (:a foo)))))
                    (foo-struct foo 'props :b))))))


(ert-deftest mu-test-struct-setters ()
  ""
  (mu-test ()

    (mu-defstruct bazzer props)

    ;; set a slot
    (should (eq 2 (let ((baz (bazzer-create)))
                    (setf (bazzer baz :props) 2)
                    (bazzer baz :props))))

    ;; replace nested value in a slot
    (should (eq 2 (let ((baz (bazzer-create :props (ht (:a (ht (:b 1)))))))
                    (setf (bazzer baz :props :a :b) 2)
                    (bazzer baz :props :a :b))))


    ;; set nested key in a slot
    (should (eq 2 (let ((baz (bazzer-create :props (ht))))
                    (setf (bazzer baz :props :a) 2)
                    (bazzer baz :props :a))))

    ;; create nested keys in a slot
    (should (eq 2 (let ((baz (bazzer-create)))
                    (setf (bazzer baz :props :a :b) 2)
                    (bazzer baz :props :a :b))))

    ;; create nested keys not in a slot
    (should (eq 2 (let ((baz (bazzer-create)))
                    (setf (bazzer baz :missing :a :b) 2)
                    (bazzer baz :missing :a :b))))

    ;; attempting to set keys in non-associative should throw
    (should (mu--error-match "protocol mu-table does not extend"
                             (let ((baz (bazzer-create :props 2)))
                               (setf (bazzer baz :props :a) 2)
                               (bazzer baz :props))))))


(ert-deftest mu-test-mu. ()
  ""
  (mu-test ()
    ;; mu-struct
    (mu-defstruct bazzer props)
    ;; mu-struct
    (mu-defstruct foo-struct (name :foo) props)
    ;; cl-struct
    (cl-defstruct (bar-struct (:constructor bar-struct-create)) props)

    ;; basic and nested lookup should work for hash-tables and structs
    (should (eq 1 (mu. (ht (:a 1)) :a)))
    (should (eq 1 (mu. (ht (:a (ht (:b 1)))) :a :b)))
    (should (eq 1 (mu. (foo-struct-create :props (ht (:a (ht (:b 1))))) :props :a :b)))
    (should (eq 1 (mu. (bar-struct-create :props (ht (:a (ht (:b 1))))) :props :a :b)))

    ;; looking up missing keys should work, not throw
    (should-not (mu. (ht (:a (ht (:b 1)))) :a :c))

    ;; mix of nested hash-tables and structs should work
    (should (eq 1 (let* ((foo (foo-struct-create :props (ht (:b 1))))
                         (bar (bar-struct-create :props (ht (:a foo)))))
                    (mu. bar :props :a :props :b))))


    ;; set a slot
    (should (eq 2 (let ((baz (bazzer-create)))
                    (setf (mu. baz :props) 2)
                    (mu. baz :props))))

    ;; nest keys in a slot
    (should (equal '(2 2) (let ((baz (bazzer-create)))
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
    (should (eq 2 (let ((baz (bazzer-create)))
                    (setf (mu. baz :missing :a :b) 2)
                    (mu. baz :missing :a :b))))


    ;; attempting to set keys in non-associative should throw
    (should (mu--error-match "protocol mu-table does not extend"
                             (let ((baz (bazzer-create :props 2)))
                               (setf (bazzer baz :props :a) 2)
                               (bazzer baz :props))))))
