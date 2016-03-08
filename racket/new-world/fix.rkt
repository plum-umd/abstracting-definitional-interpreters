#lang racket
(provide fix)
(define ((fix f) e) ((f (fix f)) e))
