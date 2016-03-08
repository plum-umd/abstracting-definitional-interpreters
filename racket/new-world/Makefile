.PHONY: clean default test
.DEFAULT: test

test:
	@for test in $$(find tests/ -name '*.rkt') ; \
		do raco test $${test} ; \
	done
	@raco test ../monad-transformers.rkt
