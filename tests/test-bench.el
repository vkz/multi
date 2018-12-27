;; -*- lexical-binding: t; -*-


;; Inspiration:
;; https://github.com/alphapapa/emacs-package-dev-handbook#profiling--optimization


(require 'multi-prelude)


;; TODO beginning to think that what I often want is actually: put code in a file,
;; byte-compile it, load it then run forms. So, somethnig like:
;;
;;   (mu-bench
;;     :context (some code to be byte-compiled)
;;     forms)
;;
;; context gets compiled and loaded once, then forms run :times for perf
;; measurement


;; TODO consider :print option that simply prints result of execution. For quick
;; and easy sanity checks especially coupled with low number of :times.


(defun mu--bench/let (context times body)
  (with-gensyms (garbage time gcs gc gcs-delta gc-delta code-time empty-time time-delta)

    (let* ((lexical-binding t)
           (bindings     (mapcar #'car context))
           (values       (mapcar #'cadr context))
           (code-lambda  (byte-compile `(lambda (,@bindings) ,@body)))
           (empty-lambda (byte-compile `(lambda (,@bindings)))))
      `(let* (;; enable lexical scope
              (lexical-binding t)
              ;; splice supplied let-bindings
              ,@context
              ;; collect garbage
              (,garbage    (garbage-collect))
              ;; number of GC runs before benchmark
              (,gcs         gcs-done)
              ;; total GC time elapsed before benchmark
              (,gc          gc-elapsed)
              ;; benchmark body
              (,code-time  (benchmark-elapse (dotimes (,time ,times)
                                               (funcall ,code-lambda ,@values))))
              ;; collect GC deltas
              (,gcs-delta  (- gcs-done ,gcs))
              (,gc-delta   (- gc-elapsed ,gc))
              ;; collect garbage
              (,garbage    (garbage-collect))
              ;; benchmark empty body
              (,empty-time (benchmark-elapse (dotimes (,time ,times)
                                               (funcall ,empty-lambda ,@values))))
              (,time-delta (- ,code-time ,empty-time)))
         ;; NOTE theoretically empty-time could be higher than the code-time, say
         ;; due to GC pause or Emacs background activity. Or the body is so simple
         ;; that float error dwarfs the result. Return code-time instead of delta
         ;; if that happens. I doubt time resolution in Elisp is even worth going
         ;; for precision beyond certain point.
         (list
          ;; account for funcall overhead
          (if (> ,time-delta 0) ,time-delta ,code-time)
          ;; number of GC runs
          ,gcs-delta
          ;; total GC time
          ,gc-delta)))))


(defun mu-bench--in-context (let-context times raw compare context body &optional show temp-file)
  (let* ((temp-file (or temp-file (make-temp-file "mu-bench" nil ".el")))
         (bench (sym (gensym "bench")))
         (contents (with-temp-buffer
                     ;; enable lexical-binding
                     (insert ";; -*- lexical-binding: t; -*-") (newline) (newline)
                     ;; insert context to be compiled
                     (insert (pp-to-string context)) (newline) (newline)
                     ;; insert defun that runs benchmark
                     (insert
                      (pp-to-string
                       `(defun ,bench ()
                          (mu-bench*/let ,let-context
                            :times ,times
                            :raw ,raw
                            :compare ,compare
                            ,@body))))
                     (buffer-string))))
    (if show
        ;; show context in temp buffer
        (with-output-to-temp-buffer "*mu-bench-context*"
          (prin1 contents) nil)
      ;; insert context into temp file
      (with-temp-file temp-file
        (insert contents))
      (unwind-protect
          ;; byte-compile and load temp file
          (if (byte-compile-file temp-file 'load)
              ;; run benchmarks
              (funcall (symbol-function bench))
            (mu-error "in mu-bench failed to byte-compile and load context"))
        ;; clean up
        (delete-file temp-file)
        (fmakunbound bench)
        (unintern (symbol-name bench) nil)))))


(comment
 (mu-bench--in-context
  '((a 1) (b 2))
  5 nil nil
  '(defun foobar () (princ (+ 2 3)))
  '((:foobar (foobar)))
  nil
  (expand-file-name "foo.el"))


 (mu-bench--in-context '((a 1) (b 2)) 1000 nil nil '(progn (+ 2 3)) '((+ 1 2)) 'show (expand-file-name "foo.el"))
 ;; comment
 )


(mu-defmacro mu-bench/let [context | (ht| times raw [| body])]
  "Run `benchmark-run-compiled' BODY that many TIMES. Unless RAW
is requested collect results into an ORG-ready table. BODY will
have access to the variables bound according to VARLIST.

Accept optional arguments as attributes:
  :times number  (defaults to 10 000)  - number of iterations
  :raw   boolean (defaults to nil)     - return raw stats

\(fn varlist &key times raw &rest body)"
  :declare ((indent 1))
  (default times :to 10000)
  (let ((header '(list "Total time" "GCs" "GC time" "Timestamp")))
    (with-gensyms (timestamp stats)
      `(let* ((,timestamp (current-time-string))
              (,stats (append
                       ;; list of perf stats
                       ,(mu--bench/let context times body)
                       ;; timestamp
                       (list ,timestamp))))
         (if ,raw
             ,stats
           (list ,header
                 'hline
                 ,stats))))))


(mu-defmacro mu-bench*/let [context | (ht| times raw compare [| body])]
  "Like `mu-bench/let' but bench every form in the BODY, where
each form is a list (label &rest expr).

Accept one extra attribute argument:
  :compare boolean (defaults to nil) - relative performance

\(fn varlist &key times raw &rest (label body...)"
  :declare ((indent 1))
  (default times :to 10000)
  (let ((header  (cond
                  (compare  '(list "Form" "x slower" "Total time" "GCs" "GC time" "Timestamp"))
                  (:default '(list "Form"            "Total time" "GCs" "GC time" "Timestamp"))))
        (process (if compare #'mu--add-relative-stats #'identity)))
    (with-gensyms (timestamp stats)
      `(let* ((,timestamp (current-time-string))
              (,stats (list ,@(cl-loop for form in body
                                       collect `(append
                                                 ;; form name
                                                 (list ,(car form))
                                                 ;; list of perf stats
                                                 ,(mu--bench/let context times (cdr form))
                                                 ;; timestamp
                                                 (list ,timestamp))))))
         (if ,raw
             (funcall #',process ,stats)
           (list* ,header
                  'hline
                  (funcall #',process ,stats)))))))


(mu-defun --by [field :on compare]
  (lambda (&rest args)
    (apply compare (mapcar field args))))


(defun mu--add-relative-stats (stats)
  "Augment stats with a 'slowdown relative to the fastest'
factor"
  (let* ((fast-to-slow (sort stats (--by #'second :on #'<)))
         (fastest (second (car fast-to-slow))))
    (mapcar
     (mu [[label time gcs gc timestamp]]
       (list label
             (format "%.2f" (/ time fastest))
             (format "%.6f" time)
             gcs
             (format "%.6f" gc)
             timestamp))
     fast-to-slow)))


(defmacro mu-bench (&rest args)
  "Like `mu-bench/let' without the VARLIST.

\(fn &key times raw &rest body)"
  (declare (indent 0))
  `(mu-bench/let nil ,@args))


(defmacro mu-bench* (&rest args)
  "Like `mu-bench*/let' without additional context.

\(fn &key times raw &rest (label body...)"
  (declare (indent 0))
  `(mu-bench*/let nil ,@args))


(example
 (mu-bench
   :times 10000
   :raw t
   (+ 1 2))

 (mu-bench*
   :times 1000
   :compare t
   (:form1 (cl-loop for x from 1 to 1000
                    collect (+ x x)))
   (:form2 (cl-loop for x from 1 to 1000
                    collect (* x x))))

 (mu-bench/let ((a 1)
                (b 2))
   :times 100000
   (+ a b))

 (mu-bench*/let ((a 1)
                 (b 2))
   :times 10000
   ;; These forms are so simple that an empty-lambda will on occasion have worse
   ;; overhead than the one with actual computation resulting in negative
   ;; time-delta, exactly the case which will return code-time, not the delta.
   ;; Really, mu-bench or rather `benchmark.el' isn't well suited for such code.
   (:form1 (+ a b))
   (:form2 (* a b)))
 ;; example
 )


(provide 'multi-bench)
