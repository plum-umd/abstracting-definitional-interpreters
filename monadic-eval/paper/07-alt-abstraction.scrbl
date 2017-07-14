#lang scribble/acmart @acmlarge

@(require scriblib/figure 
          scribble/manual 
          scribble/eval
          "evals.rkt"
          "bib.rkt")

@title[#:tag "s:alt-abstraction"]{An Alternative Abstraction}

In this section, we demonstrate how easy it is to experiment with
alternative abstraction strategies by swapping out components.  In
particular we look at an alternative abstraction of primitive
operations and store joins that results in an abstraction that---to
the best of our knowledge---has not been explored in the literature.
This example shows the potential for rapidly prototyping novel
abstractions using our approach.

@Figure-ref{f:pres-delta} defines two new components:
@racket[precise-δ@] and @racket[store-crush@].  The first is an
alternative interpretation for primitive operations that is
@emph{precision preserving}.  Unlike @racket[δ^@], it does not
introduce abstraction, it merely propagates it.  When two concrete
numbers are added together, the result will be a concrete number, but
if either number is abstract then the result is abstract.

This interpretation of primitive operations clearly doesn't impose a
finite abstraction on its own, because the state space for concrete
numbers is infinite. If @racket[precise-δ@] is linked with the
@racket[store-nd@] implementation of the store, termination is
therefore not guaranteed.

The @racket[store-crush@] operations are designed to work with
@racket[precise-δ@] by performing @emph{widening} when joining
multiple concrete values into the store. This abstraction offers a
high-level of precision; for example, constant arithmetic expressions
are computed with full precision:
@interaction[#:eval the-alt-eval
(* (+ 3 4) 9)]
Even linear binding and arithmetic preserves precision:
@interaction[#:eval the-alt-eval
((λ (x) (* x x)) 5)]
Only when the approximation of binding structure comes in to
contact with base values that we see a loss in precision:
@interaction[#:eval the-alt-eval
(let ((f (λ (x) x)))
  (* (f 5) (f 5)))
]

This combination of @racket[precise-δ@] and @racket[store-crush@]
allows termination for most programs, but still not all. In the
following example, @racket[id] is eventually applied to a widened
argument @racket['N], which makes both conditional branches
reachable. The function returns @racket[0] in the base case, which is
propagated to the recursive call and added to @racket[1], which yields
the concrete answer @racket[1].  This results in a cycle where the
intermediate sum returns @racket[2], @racket[3], @racket[4] when
applied to @racket[1], @racket[2], @racket[3], etc.
@interaction[#:eval the-alt-eval
((rec id (λ (n) (if0 n 0 (+ 1 (id (- n 1))))))
 3)
]
To ensure termination for all programs, we assume all references to
primitive operations are @math{η}-expanded, so that store-allocations
also take place at primitive applications, ensuring widening at
repeated bindings. In fact, all programs terminate when using
@racket[precise-δ@], @racket[store-crush@] and @math{η}-expanded
primitives, which means we have a achieved a computable and uniquely
precise abstract interpreter.

Here we see one of the strengths of the extensible, definitional
approach to abstract interpreters. The combination of added precision
and widening is encoded quite naturally. In contrast, it's hard to
imagine how such a combination could be formulated as, say, a
constraint-based flow analysis.
