#lang scribble/manual
@(require scribble/eval
	  scriblib/figure
          "evals.rkt")

@title[#:tag "cache"]{Detecting Cycles with a Cache}

At this point, it's worth observing that the interpreter obtained by
linking together @racket[δ^@] and @racket[alloc^@] components will
only ever visit a finite number of states for a given program.  The
state consists of an expression, environment, and store mapping
addresses to sets of values.  To see that this is a finite set is
pretty straightforward: expressions (in the given program) are finite,
environments are maps from variables (again, finite in a program) to
address.  The addresses are finite thanks to @racket[alloc^], so
environments are finite.  The store maps addresses (finite by
@racket[alloc^]) to sets of values.  The values are base values, which
are finite by @racket[δ^], or closures which consist of expressions
(in the given program) and an environment, which we just seen are
finite.  Since the elements of the sets are finite, the sets
themselves are finite, and therefore stores, and finally states, are
all finite sets.

The problem is that while the interpreter only ever see a finite set
of inputs, it @emph{doesn't know it}.  So even a simple loop will
cause the interpreter to diverge:

@interaction[#:eval the-0cfa-eval
(rec f (λ (x) (f x)) (f 0))
]

To solve this problem, let's introduce a notion of a @emph{cache}
which is a mapping from states to sets of value, store pairs.  The basic idea is
that we will use the cache to do a form of co-inductive programming.
While evaluating a state @racket[ς] for the first time, we may at some
point be asked to evaluate exactly @racket[ς] again.  Should this
happen, we can return the empty set of results.  We will use the cache
to track the encountered states and the results they produce.  By
maintaining the cache, we avoid the possibility of diverging.

@figure["ev-cache0" "Caching, first attempt"]{
@filebox[@racket[ev-cache′@]]{
@racketblock[
(define (((ev-cache ev₀) ev) e)
  (do ρ ← _ask-env
      σ ← _get-store
      ς ≔ (list e ρ σ)
      Σ ← _get-$
      (if (∈ ς Σ)
          (for/monad+ ([v×σ (Σ ς)])
            (do (_put-store (cdr v×σ))
                (_return (car v×σ))))
          (do (_put-$ (Σ ς ∅))
              v  ← ((ev₀ ev) e)
              (_update-$ 
                (λ (Σ) 
                  (Σ ς (set-add (Σ ς)
                                (cons v σ)))))
              (_return v)))))
]}}

We use the following monad stack, which adds a ``cache'' component,
which will be a finite map from states to sets of values:
@racketblock[
(ReaderT (FailT (StateT (NondetT (StateT+ ID)))))
]

The @racket[StateT+] monad transformer provides operations
@racket[get-$] and @racket[update-$] for getting and updating the
cache, respectively. It joins its finite maps by union of the range
when @racket[mplus] is called, because it cannot defer to an
underlying monoid as the outer @racket[StateT] does with
@racket[NondetT].

@Figure-ref{ev-cache0} gives an @racket[ev]-wrapper that interposes
itself on each recursive call to do the following steps:
@nested[#:style 'inset]{Check if the
current state is in the cache.  If it's in the cache, return all the
results given in the cache.  If it's not, set the cache for the
current state to the empty set, evaluate the expression,
add the resulting value to the cache for the state, and
return the result.}

We can now define an evaluation function that mixes in
@racket[ev-cache]:
@racketblock[
(define (eval e)
  (mrun ((fix (ev-cache ev)) e)))
]

If we were to link this together with @racket[alloc@] and @racket[δ@],
we'd obtain a concrete interpreter that either 1) produces the empty
set because it encountered a loop, 2) produces a singleton result, or
3) diverges because it encounters an infinite set of states.  But if
we were to link this together with @racket[alloc^@] and @racket[δ^@],
we'd obtain an abstract interpreter that is @emph{total}: it
terminates on all inputs.

@;{@note{Unfortunately, without a smarter allocation strategy, this
will never happen because looping programs allocate an inifinite
amount of memory. This is possible to fix, but would be a
distraction.}}


To see why this, observe that for a given program there only a finite
set of possible caches.  We have already seen that there are a finite
set of states and values, so it follows that there are only a finite
set of maps from states to sets of values.  Now notice that on each
recursive call, either the state is in the cache and it returns
immediately, or the cache grows.  So programs simply cannot run
forever because that would imply the cache would grow forever.

It should be easy to see that if evaluating a state @racket[ς] requires
recursively evaluating that same state, it will now produce the empty
set since the cache will be updated to map @racket[ς] to @racket[∅] before
proceeding to the sub-expressions.

We can now see that the caching abstract interpreter halts on programs
that loop (for simplicity, the cache and store are omitted from the
printed results):
@interaction[#:eval the-simple-cache-eval
(rec f (λ (x) (f x)) (f 0))
]
This accomplishes the goal of terminating on this example, and it even
gives the right answer---the empty set---since this program produces
no results.

It also works for recursive functions that terminate in the concrete,
but have loops once abstracted:
@interaction[#:eval the-simple-cache-eval
(rec fact (λ (n)
           (if0 n 1 (* n (fact (sub1 n)))))
  (fact 5))
]

It may seem we've accomplished our goal of making a sound and
decidable abstract interpreter.  However this approach is broken in
general: it is not sound in the presence of abstraction.  The problem
here is that when the interpreter reaches ``the same'' state it has
seen before, what we mean by ``the same'' in the presence of
abstraction is subtle.  For example, imagine evaluating a function
application of some function @racket[f] to an abstract value
@racket['N].  Suppose in evaluating this application we encounter
another application of @racket[f] to @racket['N].  Is it the same
application?  Well, yes and no.  It is the same @emph{abstract} state,
however the abstract state stands for a set of concrete states; in
this case, the application of @racket[f] to all numbers.  So there are
states stood for in the abstraction that are equal @emph{and} not
equal.  In other words, in the presence of abstraction, when a loop is
detected, there @emph{may} be a loop in the concrete interpretation.
Our naive loop detection set-up however is assuming there @emph{must}
be a loop.

We can demonstrate the problem with a simple counter-example to
soundness:

@interaction[#:eval the-simple-cache-eval
(rec f (λ (x) 
         (if0 x 0 (if0 (f (sub1 x)) 2 3)))
   (f (add1 0)))
]

Concretely, this program returns @racket[2], however with the
combination of loop detection and abstraction, the abstract
interpreter determines that this program produces @racket[0], which is
clearly unsound.
