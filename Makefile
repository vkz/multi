.POSIX:
.SUFFIXES:

# TODO don't want to deal with deps atm, remove -l init.el
EMACSBATCH = emacs -batch \
	-l ~/.emacs.d/init.el \
	-eval '(progn (add-to-list (quote load-path) "${PWD}/"))'

TESTS = ${PWD}/tests

# TODO I know requiring files being compiled is dumb, but no dumber than Elisp
# byte-compiler being unable to load macros defined in the same file. I'll fix
# when I'm less pissed at it
BYTECOMP = $(EMACSBATCH) \
	-eval '(progn (require (quote multi-prelude)) (require (quote multi-patterns)) (require (quote multi-structs)))' \
	-f batch-byte-compile *.el

readme:
	$(EMACSBATCH) \
	-l ~/Code/drill/dr-prelude.el \
	-eval "(progn (find-file \"make-readme.org\") (org-babel-execute-buffer))"

clean:
	rm -f README.org test-readme.el *.elc

compile: clean
	$(BYTECOMP)

test:
	$(EMACSBATCH) \
	-l ert \
	-chdir $(TESTS) \
	-l $(TESTS)/test-patterns.el \
	-l $(TESTS)/test-methods.el \
	-l $(TESTS)/test-structs.el \
	-f ert-run-tests-batch-and-exit

test-patterns:
	$(EMACSBATCH) \
	-l ert \
	-chdir $(TESTS) \
	-l $(TESTS)/test-patterns.el \
	-f ert-run-tests-batch-and-exit

test-methods:
	$(EMACSBATCH) \
	-l ert \
	-chdir $(TESTS) \
	-l $(TESTS)/test-methods.el \
	-f ert-run-tests-batch-and-exit

test-structs:
	$(EMACSBATCH) \
	-l ert \
	-chdir $(TESTS) \
	-l $(TESTS)/test-structs.el \
	-f ert-run-tests-batch-and-exit

