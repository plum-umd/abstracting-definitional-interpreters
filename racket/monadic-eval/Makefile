.PHONY: clean default test
DEFAULT: compile

compile:
	raco make -j 4 **/*.rkt

init:
	@echo "Initializing racket #lang"
	cd .. && raco link monadic-eval

test: compile
	@raco test transformers.rkt
	@raco test tests

examples: compile init
	@raco test examples
