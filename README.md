Abstracting Definitional Interpreters
=====================================

[![Build Status](https://travis-ci.org/plum-umd/monadic-eval.png?branch=master)](https://travis-ci.org/plum-umd/monadic-eval)

Abstract
--------

We show that a definitional interpreter written in monadic style can
express not only the usual notion of interpretation, but also a wide
variety of collecting semantics, abstract interpretations, symbolic
execution, and their intermixings.  We give a rational reconstruction
of a definitional _abstract_ interpreter for a higher-order language
by building a series of components implementing monadic operations.
The denouement of our story is a computable abstract interpreter that
arises from the composition of simple, independent components.
Remarkably, this interpreter implements a form of pushdown control
flow analysis (PDCFA) in which calls and returns are always properly
matched in the abstract semantics.  True to the definitional style of
Reynolds, the evaluator involves no explicit mechanics to achieve this
property; it is simply inherited from the defining language.

For more, see:

* WWW: https://plum-umd.github.io/monadic-eval/
* PDF: https://plum-umd.github.io/monadic-eval/main.pdf

Installation
------------

This code has been tested with Racket 6.3, but probably works with
other versions of Racket too.

To install:
```
   raco pkg install https://github.com/plum-umd/monadic-eval.git
```

To test:
```
   raco test --package monadic-eval
```

This will test every module in the implementation.  If no errors
occur, the code is working as expected.

To uninstall:
```
   raco pkg remove monadic-eval
```
