.PHONY: default
default: main.pdf

PNG_FILES = $(wildcard *.png)
SED_FILES = $(wildcard *.sed)
BIB_FILES = $(wildcard *.bib)
TEX_FILES = $(wildcard *.tex)
STY_FILES = $(wildcard *.sty)
CLS_FILES = $(wildcard *.cls)

out/%.tex: %.tex $(SED_FILES)
	mkdir -p out
	cp $< $@
	sed -f latex-unicode-pre.sed      -i.bak $@
	sed -f latex-unicode-escape.sed   -i.bak $@
	sed -f latex-unicode.sed          -i.bak $@
	sed -f latex-unicode-unescape.sed -i.bak $@
	sed -f latex-unicode-post.sed     -i.bak $@

out/%.bib: %.bib
	bibclean $< > $@

out/%.png: %.png
	cp $< $@

main.pdf: $(patsubst %,out/%,$(TEX_FILES)) $(patsubst %,out/%,$(BIB_FILES)) $(patsubst %,out/%,$(PNG_FILES))
	cp *.cls out/
	cp *.sty out/
	cd out ; texfot pdflatex main.tex ; bibtex main
	mv out/main.pdf main.pdf

.PHONY: again
again:
	touch main.tex
	make

.PHONY: much
much:
	make again 
	make again 
	make again

.PHONY: clean
clean:
	rm -r out/


# main.pdf : main.tex style.tex sigplanconf.cls
# 	xelatex main.tex
# 
# main.tex sigplanconf.cls : main.scrbl sec-*.scrbl bib.rkt evals.rkt screen.pdf
# 	raco scribble --latex --style none.tex main.scrbl
# 
# main.html: main.scrbl sec-*.scrbl bib.rkt evals.rkt screen.png
# 	raco scribble --html main.scrbl
# 
# tmp/formalism.tex: formalism.tex latex-unicode.sed latex-unicode-custom.sed
# 	mkdir -p tmp
# 	cp formalism.tex tmp/
# 	sed -f latex-unicode-custom.sed -i "" tmp/formalism.tex
# 	sed -f latex-unicode.sed -i "" tmp/formalism.tex
# 
# tmp/defn-interp.tex: defn-interp.tex latex-unicode.sed latex-unicode-custom.sed
# 	mkdir -p tmp
# 	cp defn-interp.tex tmp/
# 	sed -f latex-unicode-custom.sed -i "" tmp/defn-interp.tex
# 	sed -f latex-unicode.sed -i "" tmp/defn-interp.tex
# 
# formalism-harness.pdf: tmp/formalism.tex tmp/defn-interp.tex formalism-harness.tex sigplanconf.cls
# 	cp formalism-harness.tex tmp/
# 	cp sigplanconf.cls tmp/
# 	cp mathpartir.sty tmp/
# 	cp galois.sty tmp/
# 	cd tmp ; xelatex formalism-harness.tex 
# 	mv tmp/formalism-harness.pdf formalism-harness.pdf
# 

