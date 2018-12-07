.POSIX:
.SUFFIXES:

readme:
	emacs -batch \
	-l ~/.emacs.d/init.el \
	-l ~/Code/drill/prelude.el \
	-l ./_readme.el

clean:
	rm -f README.org _readme-tests.el



