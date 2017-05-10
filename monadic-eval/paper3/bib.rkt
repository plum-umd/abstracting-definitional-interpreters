#lang racket
(require scriblib/autobib scriblib/bibtex)
(provide ~cite citet generate-bib)

(define-bibtex-cite "dvanhorn.bib" ~cite citet generate-bib)
