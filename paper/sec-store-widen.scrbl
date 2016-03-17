#lang scribble/manual
@(require "bib.rkt"
	  "evals.rkt"
	  scribble/eval)

@title{Widening the Store}

The abstract interpreter we've constructed so far uses a
store-per-program-state abstraction, which while precise can be
unwieldy in terms of cost.  A common technique to combat this cost is
to use a global ``widenened'' store, which can be thought of as a
single store that is the join of all the individual stores in the
current set-up.  Thanks to our monad transformer framework, expressing
this change boils down to a simple re-ordering of the monad stack.

Whereas before we had:
@racketblock[
(ReaderT (FailT (StateT (NondetT
  (ReaderT (StateT+ ID))))))
]
if we change this stack to the following:
@racketblock[
(ReaderT (FailT (NondetT (StateT+
  (ReaderT (StateT+ ID))))))
]
we get a store-widened variant of the abstract interpreter.  The idea
here is that we swap the order of the non-determinism transformer and
state transformer that is model the store.  Because stores now need to
be joined together, we have to change @racket[StoreT] to
@racket[StoreT+].  And we are done!

To see the difference, here is an example without store-widening:
@interaction[#:eval the-pdcfa-eval/show-store
(let x (add1 0)
  (let y (if0 x 1 2)
    (let z (if0 x 3 4)
      (if0 x y z))))
]
and with:
@interaction[#:eval the-widen-pdcfa-eval/show-store
(let x (add1 0)
  (let y (if0 x 1 2)
    (let z (if0 x 3 4)
      (if0 x y z))))
]
Notice that before widening, the result is a set of value, store
pairs.  After widening the result is a pair of a set of values and a
store.  Importantly, the cache, which bounds the overall run-time of
the abstract interpreter, is potentially exponential without
store-widening, but collapses to polynomial after store-widening.
