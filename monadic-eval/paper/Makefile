main.pdf : main.tex style.tex sigplanconf.cls
	xelatex main.tex

main.tex sigplanconf.cls : main.scrbl sec-*.scrbl bib.rkt evals.rkt screen.pdf
	raco scribble ++style style.tex --latex main.scrbl

main.html: main.scrbl sec-*.scrbl bib.rkt evals.rkt screen.png
	raco scribble --html main.scrbl

tmp/formalism.tex: formalism.tex latex-unicode.sed latex-unicode-custom.sed
	mkdir -p tmp
	cp formalism.tex tmp/
	sed -f latex-unicode-custom.sed -i "" tmp/formalism.tex
	sed -f latex-unicode.sed -i "" tmp/formalism.tex

formalism-harness.pdf: tmp/formalism.tex
	cp formalism-harness.tex tmp/
	cp sigplanconf.cls tmp/
	cp mathpartir.sty tmp/
	cp galois.sty tmp/
	cd tmp ; pdflatex formalism-harness.tex 
	mv tmp/formalism-harness.pdf formalism-harness.pdf
