main.pdf : main.scrbl *-*.scrbl bib.rkt evals.rkt style.tex main.bib
	raco scribble ++style style.tex --xelatex main.scrbl

main.tex: main.scrbl *-*.scrbl bib.rkt evals.rkt style.tex main.bib
	raco scribble ++style style.tex --latex main.scrbl
