;; -*- lexical-binding: t; -*-


;; Inspiration:
;; https://github.com/alphapapa/emacs-package-dev-handbook#profiling--optimization


;; TODO :ensure-equal
;; TODO relative comparison i.e. slower-by X


(require 'multi-prelude)


(defun mu--bench/let (context times body)
  (with-gensyms (garbage time gcs gc gcs-delta gc-delta code-time empty-time)

    (let* ((lexical-binding t)
           (bindings     (mapcar #'car context))
           (values       (mapcar #'cadr context))
           (code-lambda  (byte-compile `(lambda (,@bindings) ,@body)))
           (code-loop    `(dotimes (,time ,times)
                            (funcall ,code-lambda ,@values)))
           (empty-lambda (byte-compile `(lambda (,@bindings))))
           (empty-loop   `(dotimes (,time ,times)
                            (funcall ,empty-lambda ,@values))))

      `(let* ((lexical-binding t)
              ,@context
              (,garbage    (garbage-collect))
              (,gcs         gcs-done)
              (,gc          gc-elapsed)
              (,code-time  (benchmark-elapse ,code-loop))
              (,gcs-delta  (- gcs-done ,gcs))
              (,gc-delta   (- gc-elapsed ,gc))
              (,empty-time (benchmark-elapse ,empty-loop)))

         (list (- ,code-time ,empty-time) ,gcs-delta ,gc-delta)))))


(mu-defmacro mu-bench/let [context | (ht| times raw [| body])]
  "Run `benchmark-run-compiled' BODY that many TIMES. Unless RAW
is requested collect results into an ORG-ready table. BODY will
have access to the variables bound according to VARLIST.

Accept optional arguments as attributes:
  :times number-of-iterations (defaults to 10 000)
  :raw   return-raw-data      (defaults to nil)

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


(mu-defmacro mu-bench*/let [context | (ht| times raw [| body])]
  "Like `mu-bench/let' but bench every form in the BODY, where
each form is a list (label &rest expr).

\(fn varlist &key times raw &rest (label body...)"
  :declare ((indent 1))
  (let ((header '(list "Form" "Total time" "GCs" "GC time" "Timestamp")))
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
             ,stats
           (list* ,header
                  'hline
                  ,stats))))))


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
   :times 10000
   (:form1 (+ 1 2))
   (:form2 (* 1 2)))

 (mu-bench/let ((a 1)
                (b 2))
   :times 100000
   (+ a b))

 (mu-bench*/let ((a 1)
                 (b 2))
   :times 10000

   (:form1 (+ a b))
   (:form2 (* a b)))
 ;; example
 )


(example

 (defun -on (compare take)
   (lambda (&rest args)
     (apply compare (mapcar take args))))

 (sort '((b 2) (d 5) (c 3) (a 1)) (-on #'< #'second))

 (defun -on (compare take)
   (lambda (&rest args)
     (when (apply compare args)
       (funcall take args))))
 ;; example
 )


(provide 'multi-bench)
