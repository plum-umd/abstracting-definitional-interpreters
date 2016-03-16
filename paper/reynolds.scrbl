#lang scribble/manual

@title{Pushdown @emph{à la} Reynolds}

By combining the finite abstraction of base values and closures with
the termination-guaranteeing cache-based fixed-point algorithm, we
have obtained a terminating abstract interpreter.  But what kind of
abstract interpretation are we really doing?  

We have followed the basic recipe of AAM, but adapted to a
compositional evaluator instead of an abstract machine.  However we
did manage to skip over one of the key steps in the AAM method:
continuations are not store-allocated.

@centered{@emph{In fact, there are no continuations at all!}}

The abstract machine formulation of the semantics models the
object-level stack explicitly as an inductively defined data
structure.  Because stacks may be arbitrarily large, they must be
finitized like base values and closures.  Like closures, the trick is
to thread them through the store and then finitize the store.  But in
the definitional interpreter approach, the stack is implicit and
inherited from the meta-language.