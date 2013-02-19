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