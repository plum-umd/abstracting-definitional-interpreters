.PHONY: init test clean examples
DEFAULT: init

init:
	@echo "Initializing #lang monadic-eval"
	-@raco pkg remove monadic-eval
	raco pkg install ../monadic-eval/

test: init
	@raco test --drdr --timeout +inf.0 -j 4 --package monadic-eval

examples: compile
	@raco test examples

clean:
	rm -f .\#* \#*\# *\~ &>/dev/null
	rm -f **/.\#* **/\#*\# **/*\~ &>/dev/null
	rm -rf $$(find . -type d -name compiled | xargs)
