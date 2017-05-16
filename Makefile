.PHONY: install test pdf tex clean
DEFAULT: install

install:
	@echo "Installing #lang monadic-eval"
	-@raco pkg remove monadic-eval
	@raco pkg install monadic-eval/

test:
	@raco test --drdr --timeout +inf.0 -j 4 --package monadic-eval

pdf:
	make -C monadic-eval/paper/ main.pdf

tex:
	make -C monadic-eval/paper/ main.tex

clean:
	rm -f .\#* \#*\# *\~ &>/dev/null
	rm -f **/.\#* **/\#*\# **/*\~ &>/dev/null
	rm -rf $$(find . -type d -name compiled | xargs)
