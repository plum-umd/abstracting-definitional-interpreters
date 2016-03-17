main.pdf : main.tex style.tex sigplan.cls
	xelatex main.tex

main.tex sigplan.cls : main.scrbl sec-*.scrbl bib.rkt evals.rkt
	raco scribble ++style style.tex --latex main.scrbl


