LISP ?= sbcl

build:
	$(LISP) --load nmebious.asd \
		--eval '(ql:quickload :nmebious)' \
		--eval '(asdf:make :nmebious)' \
		--eval '(quit)'

run:
	sbcl --load nmebious.asd \
	     --eval '(ql:quickload :nmebious)' \
	     --eval '(in-package :nmebious)' \
	     --eval '(start-server)'
