;; -*- lexical-binding: t; -*-


;; Inspiration:
;; https://github.com/alphapapa/emacs-package-dev-handbook#profiling--optimization


(require 'multi-prelude)
(require 'multi-patterns)
(require 'benchmark)


(defvar mu-bench--override-ts nil)
(defvar mu-bench--override-raw nil)
(defvar mu-bench--override-run t)
(defvar mu-bench--override-times nil)


(defvar mu-bench--header  (list "Form"            "Total time" "GCs" "GC time" "Timestamp"))
(defvar mu-bench--header* (list "Form" "x slower" "Total time" "GCs" "GC time" "Timestamp"))


(defvar mu-bench-debug-print nil
  "t or NUMBER. When in scope every mu-bench will accumulate that
many results and pretty-print to STDOUT. `mu-bench/context' will
pretty-print its entire context in a temporary *mu-bench-context*
buffer.")


(defun mu--bench (doc times body ts &optional bindings)
  (with-gensyms (garbage time gcs gc gcs-delta gc-delta code-time empty-time time-delta
                         code-lambda empty-lambda iterations results)

    (let* ((lexical-binding t)
           ;; NOTE this whole bindings weirdness because Elisp manages to break
           ;; lexical-scope in the most bizarre fashion, so I work around it. See:
           ;; https://emacs.stackexchange.com/questions/46812/byte-compile-and-lexical-binding
           (code `(lambda (,@bindings) ,@body))
           (empty `(lambda (,@bindings))))
      `(let* (;; enable lexical scope
              (lexical-binding t)
              ,results
              (,code-lambda    (byte-compile ,code))
              (,empty-lambda   (byte-compile ,empty))
              ;; override times
              (,iterations     (or mu-bench--override-times ,times))
              ;; collect garbage
              (,garbage        (garbage-collect))
              ;; number of GC runs before benchmark
              (,gcs            gcs-done)
              ;; total GC time elapsed before benchmark
              (,gc             gc-elapsed)
              ;; benchmark body
              (,code-time      (benchmark-elapse
                                 (dotimes (,time ,iterations)
                                   (funcall ,code-lambda ,@bindings))))
              ;; collect GC deltas
              (,gcs-delta      (- gcs-done ,gcs))
              (,gc-delta       (- gc-elapsed ,gc))
              ;; collect garbage
              (,garbage        (garbage-collect))
              ;; benchmark empty body
              (,empty-time     (benchmark-elapse
                                 (dotimes (,time ,iterations)
                                   (funcall ,empty-lambda ,@bindings))))
              (,time-delta     (- ,code-time ,empty-time)))

         ;; collect and print results when `mu-bench-debug-print'
         (when mu-bench-debug-print
           (let ((mu-bench-debug-print
                  (if (numberp mu-bench-debug-print) mu-bench-debug-print ,iterations)))
             (pp (cons ,doc (cl-loop for ,time from 1 to mu-bench-debug-print
                                     collect (funcall ,code-lambda ,@bindings))))))

         ;; NOTE theoretically empty-time could be higher than the code-time, say
         ;; due to GC pause or Emacs background activity. Or the body is so simple
         ;; that float error dwarfs the result. Return code-time instead of delta
         ;; if that happens. I doubt time resolution in Elisp is even worth going
         ;; for precision beyond certain point.
         (list
          ;; name or docstring to use as row identifier
          ,doc
          ;; bench result accounting for funcall overhead
          (if (> ,time-delta 0) ,time-delta ,code-time)
          ;; number of GC runs
          ,gcs-delta
          ;; total GC time
          ,gc-delta
          ;; timestamp, override and use the same when part of `mu-bench*'
          (or mu-bench--override-ts ,ts))))))


(defmacro mu-bench/let (varlist &optional doc &rest body)
  "Like `mu-bench' but with additional bindings in VARLIST
available in BODY.

-------------------------
VARLIST = ((id expr) ...)
-------------------------

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
  "Like `mu-bench*' but with additional bindings in VARLIST
available in BODY.

-------------------------
VARLIST = ((id expr) ...)
-------------------------

\(fn varlist &key times raw compare &rest mu-benches)"
  :declare ((indent 1))
  (default times :to 10000)
  (let ((header  (if compare 'mu-bench--header* 'mu-bench--header))
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
  "`benchmark-run-compiled' BODY that many TIMES. Unless RAW is
requested collect results into an ORG-ready table with headings.

(mu-bench [DOC] ATTR-OPTION ... BODY)
-------------------------------------
        DOC = stringp

ATTR-OPTION = :name stringp
            | :times numberp
            | :raw boolean

       BODY = code ...
-------------------------------------

TIMES defaults to 10'000. RAW defaults to nil. Unless NAME
attribute is supplied DOC is used to identify the benchmark in
statistics produced.

\(fn &optional docstring &key name times raw &rest body)"
  (declare (indent 1))
  `(mu-bench/let nil ,@args))


(defmacro mu-bench* (&rest args)
  "Like `mu-bench' but BODY must be a sequence of mu-benches to
run.

(mu-bench* [DOC] ATTR-OPTION ... BODY)
--------------------------------------
        DOC = stringp

ATTR-OPTION = :times numberp
            | :raw boolean
            | :compare boolean

       BODY = bench ...

      bench = `mu-bench'
            | `mu-bench/let'
--------------------------------------

When COMPARE is t report performance relative to the benchmark
that shows the best time, sort benchmarks table by relative
performance fastest to slowest.

\(fn &optional docstring &key times raw compare &rest mu-benches)"
  `(mu-bench*/let nil ,@args))


(defmacro mu-defbench (name arglist &optional doc &rest body)
  "Like `mu-bench' that can be called by NAME with variables in
ARGLIST in scope."
  (declare (indent defun))

  (unless (stringp doc)
    (push doc body)
    (setq doc (symbol-name name)))

  `(defun ,name ,arglist
     (mu-bench/let nil ,@body)))


(defmacro mu-defbench* (name arglist &optional doc &rest body)
  "Like `mu-bench*' that can be called by NAME with variables in
ARGLIST in scope."
  (declare (indent defun))

  (unless (stringp doc)
    (push doc body)
    (setq doc (symbol-name name)))

  `(defun ,name ,arglist
     (mu-bench*/let nil ,@body)))


(defmacro mu-bench/context (mu-bench &rest context)
  "Run MU-BENCH with additional CONTEXT compiled and loaded as
`progn' before MU-BENCH. Semantically it is as if one wrote
CONTEXT code followed by MU-BENCH in a file, byte-compiled then
loaded it.

--------------------------------------
MU-BENCH = `mu-bench' | `mu-bench/let'
CONTEXT = body
--------------------------------------"
  (declare (indent 1))
  (let* ((temp-file (make-temp-file "mu-bench" nil ".el"))
         (bench (sym (gensym "mu-bench")))
         (contents (with-temp-buffer
                     ;; enable lexical-binding
                     (insert ";; -*- lexical-binding: t; -*-") (newline) (newline)
                     ;; insert context to be compiled
                     (insert (pp-to-string `(progn ,@context))) (newline) (newline)
                     ;; insert defun that runs benchmark
                     (insert (pp-to-string `(defun ,bench () ,mu-bench)))
                     (buffer-string))))
    `(progn
       (with-temp-file ,temp-file
         (insert ,contents))
       (unwind-protect
           ;; byte-compile and load temp file
           (if (byte-compile-file ,temp-file 'load)
               (,bench)
             (mu-error "in mu-bench failed to byte-compile and load context"))
         ;; clean up
         (delete-file ,temp-file)
         (makunbound ',bench)
         (unintern ',bench nil)
         (when mu-bench-debug-print
           (with-output-to-temp-buffer "*mu-bench-context*"
             (prin1 ,contents) nil))))))


(provide 'multi-bench)
