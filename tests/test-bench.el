;; -*- lexical-binding: t; -*-


;; Inspiration:
;; https://github.com/alphapapa/emacs-package-dev-handbook#profiling--optimization


(require 'multi-prelude)

;; TODO consider :print option that simply prints result of execution. For quick
;; and easy sanity checks especially coupled with low number of :times.


(defvar mu-bench--override-ts nil)
(defvar mu-bench--override-raw nil)
(defvar mu-bench--override-run t)
(defvar mu-bench--override-times nil)


(defun mu--bench (doc times body ts &optional bindings)
  (with-gensyms (garbage time gcs gc gcs-delta gc-delta code-time empty-time time-delta
                         code-lambda empty-lambda)

    (let* ((lexical-binding t)
           ;; NOTE this whole bindings weirdness because Elisp manages to break
           ;; lexical-scope in the most bizarre fashion, so I work around id. See:
           ;; https://emacs.stackexchange.com/questions/46812/byte-compile-and-lexical-binding
           (code `(lambda (,@bindings) ,@body))
           (empty `(lambda (,@bindings))))
      `(let* (;; enable lexical scope
              (lexical-binding t)
              (,code-lambda    (byte-compile ,code))
              (,empty-lambda   (byte-compile ,empty))
              ;; collect garbage
              (,garbage        (garbage-collect))
              ;; number of GC runs before benchmark
              (,gcs            gcs-done)
              ;; total GC time elapsed before benchmark
              (,gc             gc-elapsed)
              ;; benchmark body
              (,code-time      (benchmark-elapse (dotimes (,time (or mu-bench--override-times ,times))
                                                   (funcall ,code-lambda ,@bindings))))
              ;; collect GC deltas
              (,gcs-delta      (- gcs-done ,gcs))
              (,gc-delta       (- gc-elapsed ,gc))
              ;; collect garbage
              (,garbage        (garbage-collect))
              ;; benchmark empty body
              (,empty-time     (benchmark-elapse (dotimes (,time (or mu-bench--override-times ,times))
                                                   (funcall ,empty-lambda ,@bindings))))
              (,time-delta     (- ,code-time ,empty-time)))
         ;; NOTE theoretically empty-time could be higher than the code-time, say
         ;; due to GC pause or Emacs background activity. Or the body is so simple
         ;; that float error dwarfs the result. Return code-time instead of delta
         ;; if that happens. I doubt time resolution in Elisp is even worth going
         ;; for precision beyond certain point.
         (list
          ,doc
          ;; account for funcall overhead
          (if (> ,time-delta 0) ,time-delta ,code-time)
          ;; number of GC runs
          ,gcs-delta
          ;; total GC time
          ,gc-delta
          (or mu-bench--override-ts ,ts))))))


(defvar mu-bench--header  '(list "Form"            "Total time" "GCs" "GC time" "Timestamp"))
(defvar mu-bench--header* '(list "Form" "x slower" "Total time" "GCs" "GC time" "Timestamp"))


(defmacro mu-bench/let (varlist &optional doc &rest body)
  "like `mu-bench' but with additional bindings per VARLIST.

\(fn varlist &optional docstring &key name times raw &rest body)"
  (declare (indent 1))

  (unless (stringp doc)
    (push doc body)
    (setq doc "_"))

  (mu-let (((ht| name times raw [| body]) body))
    (default times :to 10000)
    (let ((bindings (mapcar #'car varlist))
          (stats (gensym "stats")))
      `(let* ((lexical-binding t)
              ,@varlist
              (,stats ,(mu--bench (or name doc) times body (current-time-string) bindings)))
         (if (or mu-bench--override-raw ,raw)
             ,stats
           (list mu-bench--header
                 'hline
                 ,stats))))))


(mu-defmacro mu-bench*/let [varlist | (ht| times raw compare [| body])]
  "Like `mu-bench*' but with additional bindings per VARLIST.

\(fn varlist &key times raw compare &rest mu-benches"
  :declare ((indent 1))
  (default times :to 10000)
  (let ((header  (if compare mu-bench--header* mu-bench--header))
        (process (if compare #'mu--add-relative-stats #'identity))
        (stats (gensym "stats")))
    `(let* ((lexical-binding t)
            (mu-bench--override-raw t)
            (mu-bench--override-times ,times)
            (mu-bench--override-ts (current-time-string))
            ,@varlist
            (,stats (list ,@body)))
       (if ,raw
           (funcall #',process ,stats)
         (list* ,header
                'hline
                (funcall #',process ,stats))))))


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
  "Run `benchmark-run-compiled' BODY that many TIMES. Unless RAW
is requested collect results into an ORG-ready table with
headings.

Accept optional arguments as attributes:
  :name  string                        - same as docstring
  :times number  (defaults to 10 000)  - number of iterations
  :raw   boolean (defaults to nil)     - return raw stats

\(fn &optional docstring &key name times raw &rest body)"
  `(mu-bench/let nil ,@args))


(defmacro mu-bench* (&rest args)
  "Like `mu-bench' but run multiple MU-BENCHES in the body.

Accept one extra attribute argument:
  :compare boolean (defaults to nil) - relative performance

\(fn &optional docstring &key name times raw &rest mu-benches)"
  `(mu-bench*/let nil ,@args))


(defmacro mu-defbench (name arglist &optional doc &rest body)
  "doc"
  (declare (indent defun))

  (unless (stringp doc)
    (push doc body)
    (setq doc (symbol-name name)))

  `(defun ,name ,arglist
     (mu-bench/let nil ,@body)))


(defmacro mu-defbench* (name arglist &optional doc &rest body)
  "doc"
  (declare (indent defun))

  (unless (stringp doc)
    (push doc body)
    (setq doc (symbol-name name)))

  `(defun ,name ,arglist
     (mu-bench*/let nil ,@body)))


(defvar mu-bench-debug nil)


(defmacro mu-bench/context (mu-bench &rest context)
  (declare (indent 1))
  (let* ((temp-file (make-temp-file "mu-bench" nil ".el"))
         (contents (with-temp-buffer
                     ;; enable lexical-binding
                     (insert ";; -*- lexical-binding: t; -*-") (newline) (newline)
                     ;; insert context to be compiled
                     (insert (pp-to-string `(progn ,@context))) (newline) (newline)
                     ;; insert defun that runs benchmark
                     (insert (pp-to-string mu-bench))
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
             (or (byte-compile-file ,temp-file 'load)
                 (mu-error "in mu-bench failed to byte-compile and load context"))
           ;; clean up
           (delete-file ,temp-file))))))


(example

 (mu-bench :times 3 (princ (+ 1 2)))

 (mu-bench*
   :times 3
   :compare t
   (mu-bench "1" (princ (+ 1 2)))
   (mu-bench/let ((a 1)) "2" (princ (+ 1 a))))

 (mu-bench*/let ((a 0)
                 (b 1))
   :times 3
   :compare t
   (mu-bench "1" (princ (+ a b)))
   (mu-bench/let ((a 1)) "2" (princ (+ a b))))

 (mu-defbench bar-bench (a b)
   :times 3
   (princ (+ a b)))

 (bar-bench 1 2)

 (mu-defbench* foo-bench (a b)
   :times 3
   :compare t
   (mu-bench :name "1" (princ (+ a b)))
   (mu-bench/let ((a 1)) :name "2" (princ (+ a b))))

 (foo-bench 0 1)

 (let ((mu-bench-debug t))
   (mu-bench/context
       ;; benchmark
       (mu-bench/let ((a 1) (b 2))
         :times 3
         (princ (list (foobar) (barfoo) a b)))
     ;; context
     (defun foobar () 'foobar)
     (defun barfoo () 'barfoo)))
 ;; example
 )


(provide 'multi-bench)
