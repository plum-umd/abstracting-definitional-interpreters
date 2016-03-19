main.pdf : main.tex style.tex sigplanconf.cls
	xelatex main.tex

main.tex sigplanconf.cls : main.scrbl sec-*.scrbl bib.rkt evals.rkt screen.pdf
	raco scribble ++style style.tex --latex main.scrbl

main.html: main.scrbl sec-*.scrbl bib.rkt evals.rkt screen.png
	raco scribble --html main.scrbl
