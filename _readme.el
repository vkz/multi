(require 'ox)

(find-file "_readme.org")
(org-babel-execute-buffer)
(org-babel-tangle-file "_readme.org" "_readme-tests.el")
(org-export-to-file 'org "README.org")
(load-file "_readme-tests.el")
;; run tests and exit

(if noninteractive
    (ert-run-tests-batch-and-exit nil)
  (ert t)
  nil)
