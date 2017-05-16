#lang racket
(require scriblib/autobib scriblib/bibtex)
(provide ~cite citet generate-bib)
(provide (all-from-out scriblib/bibtex scriblib/autobib))

(define-bibtex-cite "main.bib" ~cite citet generate-bib)
