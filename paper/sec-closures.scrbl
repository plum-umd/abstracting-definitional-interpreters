#lang scribble/manual
@(require scribble/eval
	  scriblib/figure
          "evals.rkt")

@title[#:tag "closures"]{Abstracting Closures}

Closures consist of code---a lambda term---and an environment---a
finite map from variables to addresses.  Since the set of lambda terms
and variables is bounded by the program text, it suffices to abstract
closures by abstracting the set of addresses.  Following the AAM
approach, we can do this by modifying the allocation function to
always produce elements drawn from a finite set.  In order to retain
soundness in the semantics, we will need to modify the store to map
addresses to @emph{sets} of values and model store update as a join
and dereference as a non-deterministic choice.

Any abstraction of the allocation function that produces a finite set
will do, but the choice of abstraction will determine the precision of
the resulting analysis.  A simple choice is to allocate variable
bindings by using a variable's name as its address.  This gives a
monomorphic, or 0CFA-like, abstraction.

@Figure-ref{0cfa-abs} shows an alternative component for finite
allocation that uses variables names as the notion of addresses and a
component for the derived operations @racket[find] and @racket[ext]
when the store uses a @emph{set} as its range.  The
@racket[for/monad+] form is just a convenience for combining a set of
computations with @racket[_mplus]; in other words, @racket[find]
returns @emph{all} of the values in the store at a given address.  The
@racket[ext] function joins whenever an address is already allocated,
otherwise it maps the address to a singleton set.

@figure["0cfa-abs" "Abstracting allocation: 0CFA"]{
@filebox[@racket[alloc^@]]{
@racketblock[
(define (alloc x)
  (_return x))
]}
@filebox[@racket[store-nd@]]{
@racketblock[
(define (find a)
  (do σ ← get-store
      (for/monad+ ([v (σ a)])
        (_return v))))

(define (ext a v)
  (_update-store
    (λ (σ) (σ a (if (∈ a σ) 
                    (set-add (σ a) v) 
                    (set v))))))
]}
}

By linking these components together with the same monad stack from
@Secref{base}, we obtain an interpreter that loses precision whenever
variables are bound to multiple values.  For example, this program
binds @racket[x] to both @racket[0] and @racket[1] and therefore
produces both answers when run:
@interaction[#:eval the-0cfa-eval
  (let f (λ (x) x)
    (let _ (f 0) (f 1)))]

We've now taken care of making a sound, finite abstraction of the
space of all closures that arise during evaluation.  It would seem we
are very close to having a sound, total abstract interpretation
function.
