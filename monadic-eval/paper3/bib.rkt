#lang racket
(require scriblib/autobib scriblib/bibtex)
(provide ~cite citet generate-bib)

(define-bibtex-cite "main.bib" ~cite citet generate-bib)
