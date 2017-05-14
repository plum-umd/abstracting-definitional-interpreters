#lang scribble/acmart @acmlarge
@require["bib.rkt"]

@title[#:tag "s:aam"]{From Machines to Compositional Evaluators}

In recent years, there has been considerable effort in the systematic
construction of abstract interpreters for higher-order languages using abstract
machines---first-order transition systems---as a semantic basis.  The so-called
@emph{Abstracting Abstract Machines} (AAM) approach to abstract
interpretation @~cite{dvanhorn:VanHorn2010Abstracting} is a recipe for
transforming a machine semantics into an easily abstractable form. The
transformation includes the following ingredients:
@itemlist[
@item{Allocating continuations in the store;}
@item{Allocating variable bindings in the store;}
@item{Using a store that maps addresses to @emph{sets} of values;}
@item{Interpreting store updates as a join; and}
@item{Interpreting store dereference as a non-deterministic choice.}
]
These transformations are semantics-preserving due to the original and derived
machines operating in a lock-step correspondence.  After transforming the
semantics in this way, a @emph{computable} abstract interpreter is achieved by:
@itemlist[
@item{Bounding store allocation to a finite set of addresses; and}
@item{Widening base values to some abstract domain.}
]
After performing these transformations, the soundness and computability of the
resulting abstract interpreter are then self-evident and easily proved.

The AAM approach has been applied to a wide variety of languages and
applications, and given the success of the approach it's natural to wonder what
is essential about its use of low-level machines. It is not at all clear
whether a similar approach is possible with a higher-level formulation of the
semantics, such as a compositional evaluation function defined recursively over
the syntax of expressions.

This paper shows that the essence of the AAM approach can be applied to a
high-level semantic basis.  We show that compositional evaluators written in
monadic style can express similar abstractions to that of AAM, and like AAM,
the design remains systematic.  Moreover, we show that the high-level semantics
offers a number of benefits not available to the machine model.  

There is a rich body of work concerning tools and techniques for
@emph{extensible} interpreters @~cite{dvanhorn:Liang1995Monad
local:jaskelioff2009lifting local:kiselyov2012typed}, all of which
applies to high-level semantics.  By putting abstract interpretation
for higher-order languages on a high-level semantic basis, we can
bring these results to bear on the construction of extensible abstract
interpreters.
