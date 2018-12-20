(load-file "prelude.el")


;;* basics ------------------------------------------------------- *;;


(ert-deftest mu-test-basic-patterns ()
  "most basic patterns should work"

  (defmacro mu-case--clause-test (expr pat &rest body)
    (declare (indent 1))
    `(pcase ,expr
       ,(mu-case--clause 'seq (cons pat body))
       (otherwise
        'no-match)))

  (should (equal 'match (mu-case--clause-test '()
                          (l) 'match)))

  (should (equal 'symbol (mu-case--clause-test '(a)
                           (l 'a) 'symbol)))

  (should (equal 'b (mu-case--clause-test '(a b)
                      (l (or 'a 'b) (and x 'b)) x)))

  (should (equal 'match (mu-case--clause-test '(:key)
                          (l :key) 'match)))

  (should (equal :key (mu-case--clause-test '(:key)
                        (l x) x)))

  (should (equal '(a :over b) (mu-case--clause-test '(a (:over) b)
                                (l x (or (l rel) :to) y) (list x rel y)))))


(ert-deftest mu-test-standard-pcase-patterns ()
  "standard pcase patterns should work"

  (should (equal '(a over b none) (mu-case '(a over b)
                                    ((l x
                                        (and (pred symbolp)
                                             (or 'over 'to) rel)
                                        y)
                                     (list x rel y 'none)))))

  (should (equal '(1 2 3) (mu-case '(1 2)
                            ((l (and (pred numberp)
                                     (app 1- 0)
                                     x)
                                (and num
                                     (let y 3)))
                             (list x num y))))))


;;* list --------------------------------------------------------- *;;


(ert-deftest mu-test-list-patterns ()
  "list patterns `lst' and `l' should work"

  (should (equal 'match (mu-case '(a)
                          (_ 'match))))

  (should (equal '(a) (mu-case '(a)
                        (lst lst))))

  (should (equal '(a) (mu-case '(a)
                        ((l x y) (list x y))
                        ((l x) (list x)))))

  (should (equal 'empty (mu-case '()
                          ((l x) x)
                          (otherwise 'empty))))

  (should (equal 'match (mu-case '(a b c)
                          ((l 'a _ 'c) 'match))))

  (should (equal '(2 3) (mu-case '(1 (2 3))
                          ((l _ (l a b)) (list a b)))))

  (should (equal 'match (mu-case '((1 2))
                          ((l (l 1 &rest (l (pred numberp)))) 'match))))

  (should (equal '(1 2 3 4) (mu-case '((1 2) (3 4))
                              ((l (l (and (pred numberp) a) &rest (l b))
                                  (l (and (pred numberp) c) &rest (l d)))
                               (list a b c d)))))

  (comment

   ;; TODO current implementation of l-pat doesn't work for improper lists, but
   ;; since pcase does, imo I need to fix this. Problem is with my using `seq-take'
   ;; and `seq-subseq' which presuppose that we are acting on a proper sequence.
   (mu-case '(1 . 2)
     ((l a &rest b) (list a b)))
   ;; comment
   ))


;;* vector ------------------------------------------------------- *;;


(ert-deftest mu-test-vector-patterns ()
  "vector patterns `vec' and `v' should work"

  ;; vec-pattern of fixed length

  (should (equal '(1 2) (mu-case [1 2]
                          ((vec b c) (list b c)))))

  (should (equal '(b c) (mu-case (list 'a [b c])
                          ((l a (vec b c)) (list b c)))))

  (should (mu--error-match "in mu-case vec-pattern"
                           (mu-case [1 2]
                             ((vec x &rest y) (list x y)))))

  ;; v-pattern with &rest support

  (should (equal 'match (mu-case [] ((v) 'match))))

  (should (equal 2 (mu-case [1 2] ((v 1 y) y))))

  (should (equal [2] (mu-case [1 2] ((v 1 &rest y) y))))

  (should (equal 2 (mu-case [1 2] ((v 1 &rest [y]) y))))

  (should-not (mu-case [1 2] ((v 1) 'match)))

  (should-not (mu-case [1 2] ((v 1 2 3) 'match)))

  (should (equal '(1 [2]) (mu-case [1 2]
                            ((v x &rest y) (list x y)))))

  (should (equal []
                 (mu-case [1 2]
                   ;; TODO IMO, this is consistent with lists, but probably
                   ;; warrants a mention in documentation.
                   ((v 1 2 &rest y) y)))))


;;* table -------------------------------------------------------- *;;


(ert-deftest mu-test-table-patterns ()
  "ht-pattern should work with hash-tables and alists"

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
                          ((l (ht :a) (ht b)) (list a b)))))

  ;; allow all ht key pattern styles: :key, key, (:key id) ('key id)
  (should (equal '(1 2 3 4) (mu-case (ht (:a 1) ('b 2) (:c 3) ('d 4))
                              ((ht :a b 'c ('d D)) (list a b c D)))))

  (should (mu--error-match "in mu-case malformed ht pattern"
                           (mu-case (ht (:a 1))
                             ((ht "a"))))))


;;* sequence ----------------------------------------------------- *;;


(ert-deftest mu-test-sequence-patterns ()
  "sequence patterns [], `seq' and `lv' should work"

  (mu-test (foo)

    ;; NOTE `mu-case' uses `lv' pattern, so is strict
    ;; NOTE `mu-let' uses `seq' pattern, so isn't strict

    (should (equal '(1 2) (mu-case `(1 2) ([x y] (list x y)))))

    ;; &rest pattern should work for sequences
    (should (equal '(1 (2)) (mu-case `(1 2) ([x &rest tail] (list x tail)))))

    ;; including vectors
    (should (equal '(1 [2]) (mu-case [1 2] ([x &rest tail] (list x tail)))))

    (should (equal '(1 2) (mu-case [1 2] ([x &rest [tail]] (list x tail)))))

    (should (equal '(1 2) (mu-case [[1 [2]]]
                            ([[a &rest [[b]]]] (list a b)))))

    (should (equal '(1 2 3 [4]) (mu-case [[1 [2 3]] 4]
                                  ([[a &rest [[b c]]] &rest d] (list a b c d)))))

    (should (equal '(1 2 3 (4)) (mu-case '((1 (2 3)) 4)
                                  ([[a &rest [[b c]]] &rest d] (list a b c d)))))

    ;; since [] is strict in mu-case then
    (should-not (mu-case `(1 2) ([x y z] (list x y z))))

    ;; ditto for vectors
    (should-not (mu-case [1 2] ([x y z] (list x y z))))

    ;; but [] is not strict in mu-let, so
    (should (equal '(1 2 nil) (mu-let (([x y z] '(1 2))) (list x y z))))

    ;; ditto for vectors
    (should (equal '(1 2 nil) (mu-let (([x y z] [1 2])) (list x y z))))


    ;; for strict seq
    (should (equal '(1 2) (mu--case lv [1 2]
                            ([a b c] 'skip)
                            ([a b] (list a b)))))

    ;; for non-strict seq
    (should (equal '(1 2 nil) (mu--case seq [1 2]
                                ([a b c] (list a b c))
                                ([a b] 'nope))))

    ;; or mu-let
    (should (equal '(1 2 nil) (mu-let (([a b c] '(1 2)))
                                (list a b c))))

    ;; works even when there're more patterns than seq elements
    (should (equal '(1 2 nil nil) (mu-let (([x y z &rest tail] '(1 2)))
                                    (list x y z tail))))


    ;; even for deeply nested sequences
    (should (equal '(1 2 nil 3) (mu-let (([[a [b c]] d] [[1 [2]] 3]))
                                  (list a b c d))))


    ;; [] must be strict in a simple single-head mu-defun
    (mu-defun foo (&rest args)
      ([a b c] (list a b c))
      ([a b] (list a b)))

    (should (equal '(1 2) (foo 1 2)))
    (should (equal '(1 2 3) (foo 1 2 3)))

    ;; even the nested [] must be strict
    (mu-defun foo (&rest args)
      ([a [b c] d] (list a b c d))
      ([a [b] c] (list a b c)))

    (should (equal '(1 2 3 4) (foo 1 '(2 3) 4)))
    (should (equal '(1 2 3) (foo 1 '(2) 3)))

    (comment
     ;; TODO
     (eval
      '(let ((mu-seq-pattern-force-list 'list))
         (mu-case [[1 [2]] 3]
           ([[x &rest [y]] &rest z] (list x y z))))
      'lexical-scope)))
  ;; comment
  )


;;* rest --------------------------------------------------------- *;;


(ert-deftest mu-test-rest-patterns ()
  "patterns with &rest should work"

  ;; &rest separator should work
  (should (equal '(b c) (mu-case '(a b c)
                          ((l 'a &rest tail) tail))))

  ;; | separator should work
  (should (equal '(b c) (mu-case '(a b c)
                          ((l 'a | tail) tail))))

  ;; & separator should work
  (should (equal '(b c) (mu-case '(a b c)
                          ((l 'a & tail) tail))))

  (should (equal 'c (mu-case '(a b c)
                      ((l 'a &rest (l 'b last)) last))))

  (should (equal 'c (mu-case '(a b c)
                      ((l 'a &rest (or (l ) (l 'b last))) last))))

  (should (equal 'match (mu-case '(a)
                          ((l 'a &rest (or (l ) (l 'b last))) 'match))))

  (should (equal '(a b h) (mu-case '(a :over b :in h)
                            ((l x (or :over :to) y &rest (or (l :in z) (l )))
                             (list x y (or z 'none))))))

  ;; destructuring nested list in &rest should work
  (should (equal '(1 2 3) (mu-case '(1 (2 3))
                            ((l a &rest (l (l b c))) (list a b c)))))

  (should (equal '(1 2 3 (4)) (mu-case '((1 (2 3)) 4)
                                ((l (l a &rest (l (l b c))) &rest d) (list a b c d)))))

  ;; ditto for vectors
  (should (equal '(1 2 3 [4]) (mu-case [[1 [2 3]] 4]
                                ((v (v a &rest (v (v b c))) &rest d) (list a b c d)))))


  ;; somewhat contrived pattern that still shouldn't fail
  (should (equal '((1 2) [1 2])
                 (mapcar
                  (lambda (arg)
                    (mu-case arg
                      ([&rest tail] tail)))
                  '((1 2) [1 2])))))


;;* mu-let ------------------------------------------------------- *;;


(ert-deftest mu-test-mu-let ()
  "`mu-let' should work"

  ;; Compare 3 different behaviors of mu-let, mu-if-let, mu-when-let when
  ;; list-pattern is much too short for a match:

  ;; - mu-let
  (should (equal '(1) (mu-let ((a 1)
                               ;; no match, b unbound
                               ((l b) '(0 4)))
                        ;; b isn't used, so no error
                        (list a))))

  (should (equal '(void-variable b) (should-error
                                     (mu-let ((a 1)
                                              ;; no match, b unbound
                                              ((l b) '(0 4)))
                                       ;; b is used but unbound, so error. b
                                       ;; would've matched 0 in Clojure
                                       (list a b))
                                     :type 'error)))

  (should (equal '(1 0) (mu-let ((a 1)
                                 ;; we can fake Clojure behavior for patterns that
                                 ;; are shorter than the value
                                 ((l b &rest _) '(0 4)))
                          (list a b))))

  (should (equal '(void-variable b) (should-error
                                     (mu-let ((a 1)
                                              ;; but not for patterns that are
                                              ;; longer than the value
                                              ((l b c d) '(0 4)))
                                       ;; b, c, d are unbound and error
                                       (list a b c d))
                                     :type 'error)))

  ;; - mu-when-let
  (should-not (mu-when-let ((a 1)
                            ;; no match, b unbound
                            ((l b) '(0 4)))
                ;; body never runs, entire block returns nil
                (list a b)))

  ;; - mu-if-let
  (should (equal '(1) (mu-if-let ((a 1)
                                  ;; no match, b unbound
                                  ((l b) '(0 4)))
                          (list a b)
                        ;; picks else branch, so no error
                        (list a))))

  ;; should allow uncluttered let-bindings
  (eval
   '(let ((mu-let-parens 'no))
      (should (equal '(1 2) (mu-let (a 1 b 2)
                              (list a b)))))
   'lexical-scope)

  (eval
   '(let ((mu-let-parens 'square))
      (should (equal '(1 2) (mu-let [a 1 b 2]
                              (list a b)))))
   'lexical-scope))


;;* mu-defun ----------------------------------------------------- *;;


(ert-deftest mu-test-mu-lambda ()
  "anonymous mu-lambda should work"

  ;; all calling conventions for mu-lambda should work:

  ;; with no arglist
  (should (funcall (mu [] t)))
  (should (funcall (mu [a] t) 1))

  ;; with arglist ignored
  (should (funcall (mu _ ([] t))))

  ;; with arglist bound
  (should (funcall (mu args ([] t))))

  ;; with arglist
  (should (funcall (mu (&rest args) ([] t))))
  (should (funcall (mu (a &rest args) ([_] t)) 1))

  ;; breaking calling conventions should error
  (should (mu--error-match "in mu-defun malformed" (funcall (mu t))))
  (should (mu--error-match "in mu-defun malformed" (funcall (mu _))))
  (should (mu--error-match "in mu-defun malformed" (funcall (mu _ t))))
  (should (mu--error-match "in mu-defun malformed" (funcall (mu () t))))
  (should (mu--error-match "in mu-defun malformed" (funcall (mu (a) t))))

  ;; single-head mu-lambda should work
  (should (equal '(1 2 3 4) (funcall (mu [a b | args] (list* a b args)) 1 2 3 4)))

  ;; multi-head mu-lambda should dispatch correctly
  (let ((mu-lambda (mu _
                     ([a b] (list a b))
                     ([a b c] (list a b c)))))
    (should (equal '(1 2) (funcall mu-lambda 1 2)))
    (should (equal '(1 2 3) (funcall mu-lambda 1 2 3))))

  (let ((mu-lambda (mu (a &rest _)
                     ([_ b] (list a b))
                     ([_ b c] (list a b c)))))
    (should (equal '(1 2) (funcall mu-lambda 1 2)))
    (should (equal '(1 2 3) (funcall mu-lambda 1 2 3)))))


(ert-deftest mu-test-mu-defun ()
  "single-head and multi-head `mu-defun' should work"
  (mu-test (foo simple-foo foo-fun foo-macro)

    (should (progn (mu-defun foo [] t) (foo)))
    (should (progn (mu-defun foo [a] t) (foo 1)))

    ;; with arglist ignored
    (should (progn (mu-defun foo _ ([] t)) (foo)))

    ;; with arglist bound
    (should (progn (mu-defun foo args ([] t)) (foo)))

    ;; with arglist
    (should (progn (mu-defun foo (&rest args) ([] t)) (foo)))
    (should (progn (mu-defun foo (a &rest args) ([_] t)) (foo 1)))

    ;; breaking calling conventions should error
    (should (mu--error-match "in mu-defun malformed" (mu-defun foo t)))
    (should (mu--error-match "in mu-defun malformed" (mu-defun foo _)))
    (should (mu--error-match "in mu-defun malformed" (mu-defun foo _ t)))
    (should (mu--error-match "in mu-defun malformed" (mu-defun foo () t)))
    (should (mu--error-match "in mu-defun malformed" (mu-defun foo (a) t)))

    (mu-defun simple-foo [a b &rest rest]
      "docstring"
      :interactive "P"
      (list* a b rest))

    (should (equal '((:a :b 1 2)
                     (:a :b))

                   (list
                    (simple-foo :a :b 1 2)
                    (simple-foo :a :b))))

    (should (mu--error-match "in mu-defun no matching clause" (simple-foo :a)))

    (should (documentation 'simple-foo))


    (mu-defun simple-foo [a [b [c]] &rest rest]
      (list* a b c rest))

    (should (mu--error-match "in mu-defun no matching clause" (simple-foo :a)))
    (should (mu--error-match "in mu-defun no matching clause" (simple-foo :a :b)))

    ;; internal []-patterns should be permissive
    (should (equal '(:a :b nil) (simple-foo :a [:b])))


    (mu-defun foo-fun (&optional a b &rest args)
      "docstring"
      :interactive t
      ([_ _ x y] (list a b x y))
      ([_ _ x] (list a b x))
      ([_ _] (list a b))
      ([_] (list a b))
      ([] (list a b)))

    (should
     (equal '((:a :b 1 2)
              (:a :b 1)
              (:a :b)
              (:a nil))

            (list
             (foo-fun :a :b 1 2)
             (foo-fun :a :b 1)
             (foo-fun :a :b)
             (foo-fun :a))))

    (should (mu--error-match "in mu-defun no matching clause" (foo-fun :a :b 1 2 3)))

    (mu-defmacro foo-macro (a &rest args)
      "docstring"
      :declare ((indent defun))
      ([_ x [1 y] | z] `(list ,a ,x ,y ',z))
      ([_ x [y] | z] `(list ,a ,x ,y ',z))
      (otherwise `(list ,a)))

    (should
     (equal '((:a :b :c (1 2))
              (:a :b :c (1))
              (:a))

            (list
             (foo-macro :a :b [1 :c] 1 2)
             (foo-macro :a :b [:c] 1)
             (foo-macro :a))))))


(ert-deftest mu-test-mu-defsetter ()
  "mu-defsetter should work"
  (mu-test (foo)

    ;; define a getter
    (mu-defun foo [table [level-1-key level-2-key]]
      (ht-get* table :cache level-1-key level-2-key))

    ;; simple single-head setter
    (mu-defsetter foo [val table [level-1-key level-2-key]]
      `(setf (ht-get* ,table :cache ,level-1-key ,level-2-key) ,val))

    ;; should work
    (should (equal :foo
                   (let ((table (ht)))
                     (setf (foo table [:a :b]) :foo)
                     (foo table [:a :b]))))

    ;; multi-head setter
    (mu-defsetter foo (val &rest args)
      ([_ table ['quote [level-1-key level-2-key]]]
       `(setf (ht-get* ,table :cache ,level-1-key ,level-2-key) ,val))
      ([_ table [level-1-key level-2-key]]
       `(setf (ht-get* ,table :cache ,level-1-key ,level-2-key) ,val)))

    ;; should work
    (should (equal '(:foo :updated-foo)
                   (let ((table (ht)))
                     (list (progn
                             (setf (foo table [:a :b]) :foo)
                             (foo table [:a :b]))
                           (progn
                             (setf (foo table '(:a :b)) :updated-foo)
                             (foo table '(:a :b)))))))))


;;* errors ------------------------------------------------------- *;;


(ert-deftest mu-test-pattern-errors ()
  "Detect and signal malformed patterns"
  (should
   (mu--error-match "in mu-case malformed &rest" (mu-case '(a b c)
                                                   ((l 'a &rest foo bar) 'oops))))

  (should (mu--error-match "in mu-let malformed" (mu-let (((l _))) 'foo)))

  ;; TODO more tests
  )


;;* internals ---------------------------------------------------- *;;


(ert-deftest mu-test-mu--pcase-nest ()
  "Nesting pcase clauses should work"

  (should (equal '(progn) (mu--pcase-nest 'expr '())))

  (should (equal '(progn
                    (pcase expr (pat1 body1)))
                 (mu--pcase-nest 'expr '((pat1 body1)))))

  (should (equal '(progn
                    (pcase expr
                      (pat1 body1)
                      (otherwise (pcase expr
                                   (pat2 body2)))))

                 (mu--pcase-nest 'expr
                                 '((pat1 body1) (pat2 body2)))))

  (should (equal '(progn
                    (pcase expr
                      (pat1 body1)
                      (otherwise (pcase expr
                                   (pat2 body2)
                                   (otherwise body)))))
                 (mu--pcase-nest 'expr '((pat1 body1)
                                         (pat2 body2)
                                         (otherwise body)))))

  ;; erroneousely placed otherwise simply cuts clauses short, imo reasonable
  (should (equal '(progn
                    (pcase expr
                      (pat1 body1)
                      (otherwise body)))
                 (mu--pcase-nest 'expr '((pat1 body1)
                                         (otherwise body)
                                         (pat2 body2)))))

  ;; lone otherwise simply returns its body, imo reasonable
  (should (equal '(progn body1 body2)
                 (mu--pcase-nest 'expr '((otherwise body1 body2))))))


;;* perf --------------------------------------------------------- *;;


(comment

 ;; NOTE Dash's `-split-when' may on occasion be a tiny bit faster, because it
 ;; uses a destructive `!cdr' to update the list in a while loop. If you
 ;; macro-expand my cl-loop above u'd see the body that's almost exactly like
 ;; -split-when and in fact I could re-write the above my loop to be 100% like the
 ;; -split-when except the !cdr part but it'd make it less readable.

 (byte-compile 'mu--split-when)
 (byte-compile '-split-when)

 (list
  (mu-test-time
    (dotimes (_ 1000)
      (list
       (mu--split-when #'mu--rest? '(a b &rest c d &rest e f))
       (mu--split-when #'mu--rest? '(&rest c d &rest e f))
       (mu--split-when #'mu--rest? '(a b &rest c d &rest))
       (mu--split-when #'mu--rest? '())
       (mu--split-when #'mu--rest? '(&rest))
       (mu--split-when #'mu--rest? '(a b)))))


  (mu-test-time
    (dotimes (_ 1000)
      (list
       (-split-when #'mu--rest? '(a b &rest c d &rest e f))
       (-split-when #'mu--rest? '(&rest c d &rest e f))
       (-split-when #'mu--rest? '(a b &rest c d &rest))
       (-split-when #'mu--rest? '())
       (-split-when #'mu--rest? '(&rest))
       (-split-when #'mu--rest? '(a b))))))

 ;; comment
 )
