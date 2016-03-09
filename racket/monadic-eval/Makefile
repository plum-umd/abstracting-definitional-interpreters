.PHONY: clean default test
.DEFAULT: test

test:
	raco make **/*.rkt
	@raco test transformers.rkt
	@raco test tests
	@raco test examples
