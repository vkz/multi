.POSIX:
.SUFFIXES:


readme:
	emacs -batch \
	-l ~/.emacs.d/init.el \
	-l ~/Code/drill/prelude.el \
	-eval "(progn (find-file \"make-readme.org\") (org-babel-execute-buffer))"


clean:
	rm -f README.org test-readme.el



