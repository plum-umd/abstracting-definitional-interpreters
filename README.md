monadic-eval
============

A monadic evaluator and several monads.

Higher-order imperative language:

- eval-implicit : basic definitional interpreter
  that inherits the defining language's heap.
- eval-explicit : basic definitional interpreter
  with an explicity representation of the heap.
- sval : higher-order symbolic interpreter
- eval-set : singleton set interpreter
- eval-trace : trace interpreter
- aval-set : bounded heap abstract set interpreter
- pdcfa : bounded heap, symbolic abstract set total interpreter,
  aka Pushdown Control Flow Analysis.

More to come.

The haskell/ directory contains a similar development using monad
transformers in Haskell in order to calculate an implementation.

Abstract
--------

Definitional interpreters written in monadic style can express a wide
variety of interpreters for languages with effects. In this talk, we
show that such interpreters, under a slight reworking, can also
express a diverse set of abstract interpretations from flow analysis
to symbolic execution.

We give a rational reconstruction of a definitional abstract
interpreter for a higher-order language by constructing a series of
units implementing monadic operations. We implement units realizing
reachable state semantics, trace semantics, dead-code elimination,
symbolic execution, and a finite store abstraction. The denouement of
our story is a sound and computable abstract interpreter that arises
from the composition of simple, independent components. Remarkably,
this interpreter implements a form of pushdown control flow analysis
(PDCFA) in which calls and returns are always properly matched in the
abstract semantics. True to the definitional style of Reynolds, the
evaluator involves no explicit mechanics to achieve this property; it
is simply inherited from the defining language.