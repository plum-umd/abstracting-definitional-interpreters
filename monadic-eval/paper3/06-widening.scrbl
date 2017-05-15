#lang scribble/acmart @acmlarge

@(require scriblib/figure 
          scribble/manual 
          scribble/eval
          "evals.rkt"
          "bib.rkt")

@title[#:tag "s:widening"]{Widening the Store}

In this section, we show how to recover the well-known technique of
store-widening in our formulation of a definitional abstract
interpreter.  This example demonstrates the ease of which we can
construct existing abstraction choices.

The abstract interpreter we've constructed so far uses a
store-per-program-state abstraction, which is precise but
prohibitively expensive. A common technique to combat this cost is to
use a global ``widened'' store@~cite{dvanhorn:might-phd
dvanhorn:Shivers:1991:CFA}, which over-approximates each individual
store in the current set-up. This change is achieved easily in the
monadic setup by re-ordering the monad stack, a technique due to
@citet{local:darais-oopsla2015}. Whereas before we had
@racket[monad-cache@] we instead swap the order of @racket[StateT] for
the store and @racket[NondetT]:
@racketblock[
(ReaderT (FailT (NondetT (StateT+ (ReaderT (StateT+ ID))))))
]
we get a store-widened variant of the abstract interpreter. Because
@racket[StateT] for the store appears underneath nondeterminism, it
will be automatically widened. We write @racket[StateT+] to signify
that the cell of state supports such widening.
