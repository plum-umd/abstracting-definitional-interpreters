main.pdf : main.tex style.tex
	xelatex main.tex

main.tex : main.scrbl reynolds.scrbl store-widen.scrbl related-work.scrbl try-it.scrbl bib.rkt evals.rkt
	raco scribble ++style style.tex --latex main.scrbl


