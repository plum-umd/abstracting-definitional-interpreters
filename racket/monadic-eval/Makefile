.PHONY: clean default test
.DEFAULT: test

init:
	@echo "Initializing racket #lang"
	@cd .. && raco link monadic-eval

test: init
	raco make **/*.rkt
	@raco test transformers.rkt
	@raco test tests
	@raco test examples
