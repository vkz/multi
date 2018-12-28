;; -*- lexical-binding: t; -*-


;; Inspiration:
;; https://github.com/alphapapa/emacs-package-dev-handbook#profiling--optimization


(require 'multi-prelude)

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

;; cause mu-case is freaking slow
(setq-local mu-prefer-nested-pcase t)


(mu-defmacro mu-bench _
  :declare ((indent 1))

  ;; (mu-bench let-varlist "doc" body)
  ([(l | let-varlist) (and doc (?  stringp)) | (ht| times raw ts [| body])]
   (default times :to 10000)
   (default ts :to (current-time-string))
   (let ((header '(list "Bench" "Total time" "GCs" "GC time" "Timestamp")))
     (with-gensyms (stats)
       `(let ((,stats (append
                       ;; description
                       (list ,doc)
                       ;; list of perf stats
                       ,(mu--bench/let let-varlist times body)
                       ;; timestamp
                       (list ,ts))))
          (if ,raw
              ,stats
            (list ,header
                  'hline
                  ,stats))))))

  ;; (mu-bench "doc" body)
  ([(and (?  stringp) doc) | body]
   `(mu-bench () ,doc ,@body))

  ;; (mu-bench varlist body)
  ([(l | let-varlist) | body]
   `(mu-bench ,let-varlist "" ,@body))

  ;; (mu-bench body)
  ([| body]
   `(mu-bench () "" ,@body)))


(defun mu-bench--raw (bench let-varlist times ts)
  (mu-case bench
    (['mu-bench (l | let-bench-varlist) (and (?  stringp) doc) | (ht| [| body])]
     `(mu-bench ,(append let-varlist let-bench-varlist) ,doc :times ,times :raw t :ts ,ts ,@body))

    ;; (mu-bench "doc" body)
    (['mu-bench (and (?  stringp) doc) | body]
     (mu-bench--raw `(mu-bench () ,doc ,@body) let-varlist times ts))

    ;; (mu-bench varlist body)
    (['mu-bench (l | let-bench-varlist) | body]
     (mu-bench--raw
      `(mu-bench ,let-bench-varlist ,(symbol-name (gensym "bench")) ,@body)
      let-varlist
      times
      ts))

    ;; (mu-bench body)
    (['mu-bench | body]
     (mu-bench--raw `(mu-bench () ,(symbol-name (gensym "bench")) ,@body) let-varlist times ts))))


(mu-defmacro mu-benches _
  :declare ((indent 1))

  ;; (mu-benches varlist "doc" body)
  ([(l | let-varlist) (and doc (?  stringp)) | (ht| times raw compare [| body])]
   (default times :to 10000)
   (let* ((ts (current-time-string))
          (benches (mapcar (lambda (bench) (mu-bench--raw bench let-varlist times ts)) body))
          (header (cond
                   (compare '(list "Bench" "x slower" "Total time" "GCs" "GC time" "Timestamp"))
                   (:default '(list "Bench" "Total time" "GCs" "GC time" "Timestamp"))))
          (process (if compare #'mu--add-relative-stats #'identity)))
     (with-gensyms (stats)
       `(let ((,stats (funcall #',process (list ,@benches))))
          (if ,raw
              ,stats
            (list* ,header
                   'hline
                   ,stats))))))

  ;; (mu-benches "doc" body)
  ([(and (? stringp) doc) | body]
   `(mu-benches () ,doc ,@body))

  ;; (mu-benches varlist body)
  ([(l | varlist) | body]
   `(mu-benches ,varlist "" ,@body))

  ;; (mu-benches body)
  ([| body]
   `(mu-benches () "" ,@body)))


;; butt slow without `mu-prefer-nested-pcase'
(mu-benches ((a 1)
             (b 2))
  "benches"
  :times 5
  :compare t
  (mu-bench "bench 1" (princ (+ a b)))
  (mu-bench "bench 2" (princ (+ 3 2))))


(defvar mu-bench-debug nil)


(defmacro mu-bench/context (mu-bench &rest context)
  (declare (indent 1))
  (let* ((temp-file (make-temp-file "mu-bench" nil ".el"))
         (bench (sym (gensym "bench")))
         (contents (with-temp-buffer
                     ;; enable lexical-binding
                     (insert ";; -*- lexical-binding: t; -*-") (newline) (newline)
                     ;; insert context to be compiled
                     (insert (pp-to-string `(progn ,@context))) (newline) (newline)
                     ;; insert defun that runs benchmark
                     (insert
                      (pp-to-string
                       `(defun ,bench ()
                          ,mu-bench)))
                     (buffer-string))))
    (if mu-bench-debug
        ;; show context in temp buffer
        `(with-output-to-temp-buffer "*mu-bench-context*"
           (prin1 ,contents) nil)
      ;; insert context into temp file
      `(progn
         (with-temp-file ,temp-file
           (insert ,contents))
         (unwind-protect
             ;; byte-compile and load temp file
             (if (byte-compile-file ,temp-file 'load)
                 ;; run benchmarks
                 (,bench)
               (mu-error "in mu-bench failed to byte-compile and load context"))
           ;; clean up
           (delete-file ,temp-file)
           (fmakunbound ',bench)
           (unintern (symbol-name ',bench) nil))))))


(example
 (let ((mu-bench-debug t))
   (mu-bench/context
       ;; benchmark
       (mu-bench
         :times 5
         (princ (list (foobar) (barfoo))))
     ;; context
     (defun foobar () 'foobar)
     (defun barfoo () 'barfoo)))
 ;; example
 )


(provide 'multi-bench)
