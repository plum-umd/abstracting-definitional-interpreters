#lang scribble/manual
@(require "bib.rkt")

@title{From Machines to Compositional Evaluators}

In recent years, there has been considerable effort in the systematic
construction of abstract interpreters for higher-order languages using
abstract machines---first-order transition systems---as a semantic
basis.  The so-called @emph{abstracting abstract machines} (AAM)
approach to abstract interpretation@~cite[aam] is a recipe for
transforming a machine semantics into an easily abstractable form.
There are a few essential elements to the transformation:

@itemlist[
@item{allocating continuations in the store}
@item{allocated variable bindings in the store}
@item{using a store that maps addresses to @emph{sets} of values}
@item{interpreting store updates as a join}
@item{interpreting store dereference as a non-deterministic choice}]

These transformations are semantics-preserving as the original and
derived machines operate in a lock-step correspondence.  But the real
value of the derived semantics stems from the fact that it's possible
to turn the derived machine into an abstract interpreter with two
simple steps:

@itemlist[
@item{bounding store allocation to a finite set of addresses}
@item{widening base values to some abstract domain}
]

Moreover, the soundness of the resulting abstraction is self-evident
and easily proved.

The AAM approach has been applied to a wide variety of languages and
applications, and given the success of the AAM approach, it's natural
to wonder what is essential about the low-level machine basis of the
semantics and whether a similar approach is possible with a
higher-level formulation of the semantics such as a compositional
evaluation function.

This paper shows that the essence of the AAM approach can be put on a
high-level semantic basis.  We show that compositional evaluators,
written in monadic style can express similar abstractions to that of
AAM.  Moreover, we show that the high-level semantics offers a number
of benefits not available to the machine model.  

First, as we will see, the definitional interpreter approach is not
formulated as a transformation on the semantics itself, but rather
uses alternative notions of a monad to express the ``abstracting''
transformations.  This means the concrete and abstract interpreters
for a language can share large parts of their implementation; there is
just one interpreter with a multiplicity of interpretations.}

Second, there is a rich body of work and many tools and techniques for
constructing @emph{extensible} interpreters, all of which applies to
high-level semantics, not machines.  By putting abstract
interpretation for higher-order languages on a high-level semantic
basis, we can bring these results to bear on the construction of
extensible abstract interpreters.  In particular, we use @emph{monad
transformers} to build re-usable components for mixing and matching
the constiuent parts of an abstract interpreter.

Finally, using definitional interpreters for abstract interpretation
satisfies an intellectual itch that asks whether it can be done at
all.  In solving this technical challenge, we discover a pleasant
surprise about the definitional interpreter approach: it is inherently
``pushdown.''  Under the interpreter approach, the property follows
for free as a gift from the metalanguage.

