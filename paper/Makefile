main.pdf : main.tex style.tex
	xelatex main.tex

main.tex : main.scrbl sec-*.scrbl bib.rkt evals.rkt
	raco scribble ++style style.tex --latex main.scrbl


