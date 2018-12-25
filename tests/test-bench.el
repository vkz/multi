;; -*- lexical-binding: t; -*-


;; Inspiration:
;; https://github.com/alphapapa/emacs-package-dev-handbook#profiling--optimization


(mu-defmacro mu-bench [| (ht| times raw hline [| body])]
  "Run BODY in `benchmark-run-compiled' TIMES times. Collect
results into an ORG-table as list.

When RAW is non-nil don't include a table header with column
names (default: nil).

When HLINE is nil don't add table separator to the
table (default: t)."
  :declare ((indent 0))
  (default times :to 10000)
  (default raw :to nil)
  (default hline :to t)
  (let ((table (gensym "table"))
        (results (gensym "results")))
    `(progn
       (let (,table ,results)
         (garbage-collect)
         (setq ,results (benchmark-run-compiled ,times (progn ,@body)))
         ;; add timestamp as the first column
         (setq ,table (list (cons (current-time-string) ,results)))
         ;; add hline above the results unless hline=nil
         (when ,hline (setq ,table (cons 'hline ,table)))
         ;; add column names unless raw=t
         (unless ,raw (setq ,table (cons (list "Timestamp" "Total runtime" "# of GCs" "Total GC runtime") ,table)))
         ;; return org-table as a list
         ,table))))


(cl-defmacro bench-multi (&key (times 1) forms ensure-equal raw)
  "Return Org table as a list with benchmark results for FORMS.
 Runs FORMS with `benchmark-run-compiled' for TIMES iterations.

 When ENSURE-EQUAL is non-nil, the results of FORMS are compared,
 and an error is raised if they aren't `equal'. If the results
 are sequences, the difference between them is shown with
 `seq-difference'.

 When RAW is non-nil, the raw results from
 `benchmark-run-compiled' are returned instead of an Org table
 list.

 If the first element of a form is a string, it's used as the
 form's description in the bench-multi-results; otherwise, forms
 are numbered from 0.

 Before each form is run, `garbage-collect' is called."
  ;; MAYBE: Since `bench-multi-lexical' byte-compiles the file, I'm not sure if
  ;; `benchmark-run-compiled' is necessary over `benchmark-run', or if it matters.
  (declare (indent defun))
  (let* ((keys (gensym "keys"))
         (result-times (gensym "result-times"))
         (header '(("Form" "x faster than next" "Total runtime" "# of GCs" "Total GC runtime")
                   hline))
         ;; Copy forms so that a subsequent call of the macro will get the original forms.
         (forms (copy-list forms))
         (descriptions (cl-loop for form in forms
                                for i from 0
                                collect (if (stringp (car form))
                                            (prog1 (car form)
                                              (setf (nth i forms) (cadr (nth i forms))))
                                          i))))
    `(unwind-protect
         (progn
           (defvar bench-multi-results nil)
           (let* ((bench-multi-results (make-hash-table))
                  (,result-times (sort (list ,@(cl-loop for form in forms
                                                        for i from 0
                                                        for description = (nth i descriptions)
                                                        collect `(progn
                                                                   (garbage-collect)
                                                                   (cons ,description
                                                                         (benchmark-run-compiled ,times
                                                                           ,(if ensure-equal
                                                                                `(puthash ,description ,form bench-multi-results)
                                                                              form))))))
                                       (lambda (a b)
                                         (< (second a) (second b))))))
             ,(when ensure-equal
                `(cl-loop with ,keys = (hash-table-keys bench-multi-results)
                          for i from 0 to (- (length ,keys) 2)
                          unless (equal (gethash (nth i ,keys) bench-multi-results)
                                        (gethash (nth (1+ i) ,keys) bench-multi-results))
                          do (if (sequencep (gethash (car (hash-table-keys bench-multi-results)) bench-multi-results))
                                 (let* ((k1) (k2)
                                        ;; If the difference in one order is nil, try in other order.
                                        (difference (or (setq k1 (nth i ,keys)
                                                              k2 (nth (1+ i) ,keys)
                                                              difference (seq-difference (gethash k1 bench-multi-results)
                                                                                         (gethash k2 bench-multi-results)))
                                                        (setq k1 (nth (1+ i) ,keys)
                                                              k2 (nth i ,keys)
                                                              difference (seq-difference (gethash k1 bench-multi-results)
                                                                                         (gethash k2 bench-multi-results))))))
                                   (user-error "Forms' bench-multi-results not equal: difference (%s - %s): %S"
                                               k1 k2 difference))
                               ;; Not a sequence
                               (user-error "Forms' bench-multi-results not equal: %s:%S %s:%S"
                                           (nth i ,keys) (nth (1+ i) ,keys)
                                           (gethash (nth i ,keys) bench-multi-results)
                                           (gethash (nth (1+ i) ,keys) bench-multi-results)))))
             ;; Add factors to times and return table
             (if ,raw
                 ,result-times
               (append ',header
                       (bench-multi-process-results ,result-times)
                       '(hline)))))
       (unintern 'bench-multi-results nil))))


(defun bench-multi-process-results (results)
  "Return sorted RESULTS with factors added."
  (setq results (sort results (-on #'< #'second)))
  (cl-loop with length = (length results)
           for i from 0 to (1- length)
           for description = (car (nth i results))
           for factor = (if (< i (1- length))
                            (format "%.2f" (/ (second (nth (1+ i) results))
                                              (second (nth i results))))
                          "slowest")
           collect (append (list description factor)
                           (list (format "%.6f" (second (nth i results)))
                                 (third (nth i results))
                                 (if (> (fourth (nth i results)) 0)
                                     (format "%.6f" (fourth (nth i results)))
                                   0)))))

(defun -on (compare take)
  (lambda (&rest args)
    (apply compare (mapcar take args))))

(example
 (sort '((b 2) (d 5) (c 3) (a 1)) (-on #'< #'second))

 (defun -on (compare take)
  (lambda (&rest args)
    (when (apply compare args)
      (funcall take args))))
 ;; example
 )


(cl-defmacro bench-multi-lexical (&key (times 1) forms ensure-equal raw)
  "Return Org table as a list with benchmark results for FORMS.
Runs FORMS from a byte-compiled temp file with `lexical-binding'
enabled, using `bench-multi', which see.

Afterward, the temp file is deleted and the function used to run
the benchmark is uninterned."
  (declare (indent defun))
  `(let* ((temp-file (concat (make-temp-file "bench-multi-lexical-") ".el"))
          (fn (gensym "bench-multi-lexical-run-")))
     (with-temp-file temp-file
       (insert ";; -*- lexical-binding: t; -*-" "\n\n"
               "(defvar bench-multi-results)" "\n\n"
               (format "(defun %s () (bench-multi :times %d :ensure-equal %s :raw %s :forms %S))"
                       fn ,times ,ensure-equal ,raw ',forms)))
     (unwind-protect
         (if (byte-compile-file temp-file 'load)
             (funcall (intern (symbol-name fn)))
           (user-error "Error byte-compiling and loading temp file"))
       (delete-file temp-file)
       (unintern (symbol-name fn) nil))))


;; TODO I want a :pre or :let where I can define bindings for all forms. Above
;; implementation feels backwards


(provide 'multi-bench)
