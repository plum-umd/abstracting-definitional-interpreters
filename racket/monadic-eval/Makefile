.PHONY: clean default test
DEFAULT: compile

init:
	@echo "Initializing #lang monadic-eval"
	@for oldlink in $$(raco link -l | grep monadic-eval | sed 's/^.*path: //' | xargs); do \
	  raco link -r $$oldlink ; \
  done
	cd .. && raco link monadic-eval

compile: init
	raco make -j 4 **/*.rkt

test: compile
	@raco test transformers.rkt
	@raco test tests/eval*.rkt

examples: compile
	@raco test examples

clean:
	rm -f .\#* \#*\# *\~ &>/dev/null
	rm -f **/.\#* **/\#*\# **/*\~ &>/dev/null
	rm -rf $$(find . -type d -name compiled | xargs)
