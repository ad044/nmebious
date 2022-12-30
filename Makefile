LISP ?= sbcl

build:
	$(LISP) --load nmebious.asd \
		--eval '(ql:quickload :nmebious)' \
		--eval '(asdf:make :nmebious)' \
		--eval '(quit)'

run:
	$(LISP) --load nmebious.asd \
	     	--eval '(ql:quickload :nmebious)' \
	     	--eval '(in-package :nmebious)' \
	     	--eval '(main)'
