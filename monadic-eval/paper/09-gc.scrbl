#lang scribble/acmart @acmlarge

@(require scriblib/figure 
          scribble/manual 
          scribble/eval
          "evals.rkt"
          "bib.rkt")

@title[#:tag "s:gc"]{Garbage Collection}

As a denouement to our series of examples, we show how to incorporate
garbage collection into our definitional abstract interpreter.

This example, like store-widening, is the re-creation of a well-known
technique: abstract garbage collection
@~cite{dvanhorn:Might:2006:GammaCFA} mimics the process of reclaiming
unreachable heap addresses as done in garbage-collecting concrete
interpreters.  While garbage collection in the concrete can largely be
considered an implementation detail that doesn't effect the results of
computation (modulo pragmatic issues of memory consumption), in the
abstract semantics, garbage collection can have a significant positive
effect on the precision of analysis results.  This is because store
locations mediate joins, and therefore points of imprecision, in the
abstract semantics.  If an address can be cleared-out and recycled,
this represents the avoidance of a join that would be encountered in a
non-garbage-collecting abstract interpreter.

In the finite-state-machine model, abstract garbage collection is
fairly straightforward and closely follows concrete formulations
@~cite{dvanhorn:Might:2006:GammaCFA dvanhorn:VanHorn2010Abstracting}.
However, incorporating both pushdown control flow and abstract garbage
collection has proved rather involved and required new techniques
@~cite{dvanhorn:Earl2012Introspective dvanhorn:Johnson2014Pushdown}.

@figure["f:gc-monad" "Monad Instance with Root Address Set"]{
@filebox[@racket[monad-pdcfa-gc@]]{
@racketblock[
(define-monad (ReaderT (ReaderT (FailT (StateT (NondetT (ReaderT (StateT+ ID))))))))
]}
@filebox[@racket[mrun-pdcfa-gc@]]{
@racketblock[
(define (mrun m)
  (run-StateT+ ∅ (run-ReaderT ∅   @code:comment{out-$₀, in-$₀}
  (run-StateT  ∅ (run-ReaderT ∅   @code:comment{σ₀, ρ₀}
  (run-ReaderT (set) m))))))       @code:comment{ψ₀}
]}}

The key difficulty for pushdown machine models, which essentially use
abstract models that are pushdown automata, is that the usual approach
to garbage collection is to crawl the call stack to compute the root
set of reachable addresses @~cite{dvanhorn:Morrisett1995Abstract}.
Traversing the stack, however, is not something that can be expressed
by a pushdown automata.
@;{}
This difficulty is somewhat exacerbated by the definitional
interpreter approach in that there isn't even a stack to traverse!
Nevertheless, as we demonstrate, this challenge can be overcome to
obtain a pushdown, garbage-collecting abstract interpreter.
@;{}
Doing so shows that the definitional abstract interpreter approach
also scales to handle so-called @emph{introspective} pushdown analysis
that require some level of introspection on the stack
@~cite{dvanhorn:Earl2012Introspective dvanhorn:Johnson2014Pushdown}.

Solving the abstract garbage collection problem for a definitional
abstract interpreter boils down to answer the following question: how
can we track root addresses that are live on the call stack when the
call stack is implicitly defined by the metalanguage?  The answer is
fairly simple: we extend the monad with a set of root addresses.  When
evaluating compound expressions, we calculate the appropriate root
sets for the context.  In essence, we render explicit only the
addresses of the calling context, while still relying on the
metalanguage to implicitly take care of the rest as before.

@Figure-ref{f:gc-monad} defines the appropriate monad instance.  All
that has changed is there is an added reader component, which will be
used to model the context's current root set.
@;{}
The use of this added component necessitates a change to the caching
and fixed-point calculation, namely we must include the root sets as
part of the configuration.  Compared with the @racket[ev-cache@]
component of @secref{s:cache}, we make a simple adjustment to the
first few lines to cache the root set along with the rest of the
configuration:
@racketblock[
(define (((ev-cache ev₀) ev) e)
  (do ρ   ← ask-env  σ ← get-store  ψ ← ask-roots
      ς   ≔ (list e ρ σ ψ)
      ...))]
Similarly, for @racket[fix-cache@]:
@racketblock[
(define ((fix-cache eval) e)  
  (do ρ ← ask-env  σ ← get-store  ψ ← ask-roots
      ς ≔ (list e ρ σ ψ)
      ...))
]

We can now write a @racket[ev-collect@] component that performs the
garbage collection: it asks for the current roots in the context,
evaluates an expression to a value, then updates the store after
garbage collecting all addresses not reachable from the root set of
the context and the roots in the produced value:

@racketblock[
(define (((ev-collect ev0) ev) e)
  (do ψ ← ask-roots
      v ← ((ev0 ev) e)
      (update-store (gc (set-union ψ (roots-v v))))
      (return v)))
]
Here, @racket[gc] and @racket[roots-v] are (omitted) helper functions
that perform garbage collection and calculate the set of root
addresses in a value, respectively.

All that remains is to define a component that propagates root sets
appropriately from compound expressions to their constituents.
@Figure-ref{f:gc-collect-roots} gives the @racket[ev-roots@] component, which
does exactly this.
@;{}
Finally, the pieces are stitched together with the following to obtain
a pushdown, garbage-collecting definitional abstract interpreter:
@racketblock[
(define (eval e)
  (mrun ((fix-cache (fix (ev-cache (ev-collect (ev-roots ev))))) e)))
]

@figure["f:gc-collect-roots" "Address Collection and Propagation"]{
@filebox[@racket[ev-roots@]]{
@racketblock[
(define (((ev-roots ev₀) ev) e)
  (match e
    [(if0 e₀ e₁ e₂) (do ψ  ← ask-roots
                        ρ  ← ask-env
                        ψ′ ≔ (set-union ψ (roots e₁ ρ) (roots e₂ ρ))
                        v  ← (local-roots ψ′ (ev e₀))
                        b  ← (truish? v)
                        (ev (if b e₁ e₂)))]
    [(op2 o e₀ e₁)  (do ψ  ← ask-roots
                        ρ  ← ask-env
                        v₀ ← (local-roots (set-union ψ (roots e₁ ρ)) (ev e₀))
                        v₁ ← (local-roots (set-union ψ (roots-v v₀)) (ev e₁))
                        (δ o v₀ v₁))]
    [(app e₀ e₁)    (do ρ  ← ask-env
                        ψ  ← ask-roots
                        v₀ ← (local-roots (set-union ψ (roots e₁ ρ)) (ev e₀))
                        v₁ ← (local-roots (set-union ψ (roots-v v₀)) (ev e₁))
                        (cons (lam x e₂) ρ′) ≔ v₀
                        a  ← (alloc x)
                        (ext a v₁)
                        (local-env (ρ′ x a) (ev e₂)))]
    [_ ((ev₀ ev) e)]))]}}
