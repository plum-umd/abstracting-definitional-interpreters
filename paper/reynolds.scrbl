#lang scribble/manual
@(require "bib.rkt")

@title{Pushdown @emph{à la} Reynolds}

By combining the finite abstraction of base values and closures with
the termination-guaranteeing cache-based fixed-point algorithm, we
have obtained a terminating abstract interpreter.  But what kind of
abstract interpretation are we really doing?  

We have followed the basic recipe of AAM, but adapted to a
compositional evaluator instead of an abstract machine.  However we
did manage to skip over one of the key steps in the AAM method:
we never store-allocated continuations.

@centered{@emph{In fact, there are no continuations at all!}}

The abstract machine formulation of the semantics models the
object-level stack explicitly as an inductively defined data
structure.  Because stacks may be arbitrarily large, they must be
finitized like base values and closures.  Like closures, the AAM trick
is to thread them through the store and then finitize the store.  But
in the definitional interpreter approach, the stack is implicit and
inherited from the meta-language.

But here is the remarkable thing: since the stack is inherited from
the meta-language, the abstract interpreter inherits the ``call-return
matching'' of the meta-language, which is just to say there is no loss
of precision of in the analysis of the control stack.  This is a
property that usually comes at considerable effort and engineering in
the formulations of higher-order flow analysis that model the stack
explicitly.  So-called higher-order ``pushdown'' analysis has been the
subject of multiple publications and a dissertation@~cite[cfa2-lmcs
pdcfa-sfp10 cfa2-diss aam-jfp earl-icfp12 johnson-dls14 johnson-jfp14
gilray-popl16].  Yet when formulated in the definitional interpreter
style, the pushdown property requires no mechanics and is simply
inherited from the meta-language.

Reynolds, in his celebrated paper @emph{Definitional interpreters for
higher-order languages}@~cite[reynolds72], first observed that when
the semantics of a programming language is presented as a definitional
interpreter, the defined language could inherit semantic properties of
the defining language.  We have now shown this observation can be
extended to @emph{abstract} interpretation as well, namely in the
important case of the pushdown property.

In the remainder of this paper, we harvest some of the fruits of our
labor by exploring a few natural extensions and variations on the
basic pushdown abstract interpreter we have established at this point.
