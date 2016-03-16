#lang scribble/manual

@(require "bib.rkt")

@title{Related Work}

This work draws upon and re-presents many existing ideas from the
literature on abstract interpretation for higher-order languages.  In
particular, it closely follows the abstracting abstract
machine@~cite[aam aam-jfp] approach to deriving abstract interpreters
from semantics for higher-order languages.  The key difference here is
that we have done it in the setting of a monadic definitional
interpreter instead of an abstract machine.  This involved a novel
caching mechanism and fixed-point algorithm, but otherwise followed
the same recipe.  Remarkably, the pushdown property is simply
inherited from the meta-language rather than require explicit
mechanisms within the abstract interpreter.

The use of monads and monad transformers to make extensible (concrete)
interpreters is a well-known idea@~cite[moggi-89 steele-popl94
liang-popl95], which we have extended to work for compositional
abstract interpreters.  The use of monads and monad transformers in
machine based-formulatons of abstract interpreters has previously been
explored by Sergey, @emph{et al.}@~cite[sergey-pldi13] and Darais
@emph{et al.}@~cite[darais-oopsla15], respectively.  Darais has also
shown that certain monad transformers are also @emph{Galois
transformers}, i.e. they compose to form monads that are Galois
connections.  This idea may pave a path forward for having both
componential code @emph{and proofs} for abstract interpreters in the
style presented here.

The caching mechanism used to ensure termination in our abstract
interpreter is similar to that used by Johnson and Van
Horn@~cite[johnson-dls14].  They use a local- and meta-memoization
table in a machine-based interpreter to ensure termination for a
pushdown abstract interpreter.  This mechanism is in turn reminiscent
of Glück's use of memoization in an interpreter for two-way
non-deterministic pushdown automata@~cite[glück-schmidtfest13].

Vardoulakis, who was the first to develop the idea of a pushdown
abstraction for higher-order flow analysis@~cite[cfa2-lmcs],
formalized CFA2 using a CPS model, which is similar in spirit to a
machine-based model.  However, in his dissertation@~cite[cfa2-diss] he
sketches an alternative presentation dubbed ``Big CFA2'' which is a
big-step operational semantics for doing pushdown analysis quite
similar in spirit to the approach presented here.  One key difference
is that Big CFA2 fixes a particular coarse abstraction of base values
and closures---for example, both branches of a conditional are always
evaluated.  Consequently, it only uses a single iteration of the
abstract evaluation function, similar to the @emph{unsound} approach
of @secref{cache} and avoids the need for the cache-based fixed-point
of @secref{fixing-cache}.  We don't believe Big CFA2 as stated is
unsound, however if the underlying abstractions were tightened, it
appears it would run in to the same issues identified here.

Our formulation of a pushdown abstract interpreter computes an
abstraction similar to the many existing variants of pushdown flow
analysis@~cite[cfa2-lmcs pdcfa-sfp10 cfa2-diss aam-jfp earl-icfp12
johnson-dls14 johnson-jfp14 gilray-popl16].  Our incorporation of an
abstract garbage collector into a pushdown abstract interpreter
achieves a similar goal as that of so-called @emph{introspective}
pushdown abstract interpreters@~cite[earl-icfp12 johnson-jfp14].  The
mixing of symbolic execution and abstract intrepretation is similar in
spirit to the @emph{logic flow analysis} of Might@~cite[might-popl07],
albeit in a pushdown setting and with a stronger notion of negation;
generally, our presentation resembles traditional formulations of
symbolic execution more closely.  Our approach to symbolic execution
only handles the first-order case of symbolic values, as is
traditional.  However, Nguyễn's work on higher-order symbolic
execution@~cite[nguyen-pldi15] demonstrates how to scale to behavioral
symbolic values.  In principle, it should be possible to handle this
case in our approach by adapting Nguyễn's method to a formulation in a
compositional evaluator.

We have eschewed soundness proofs in this paper.  This is done in part
to emphasize the pearly intuitions and constructions of abstract
definitional interpreters and in part because it is far less clear how
to prove soundness when compared to the machine-based formulations.
Part of the difficulty stems from the set-up to support extensibility.
As mentioned previously, perhaps Galois
transformers@~cite[darais-oopsla15] can help with this aspect.  But
even if we fixed a particular set of components and monad transformer
stack, we run up against the challenge of having to prove soundness in
the presence of concrete computations which may not terminate.
Handling this in the small-step setting is easy using a preservation
argument, but it's not clear how to do it with our approach.  Rompf
and Amin's recent work on proving type soundness with definitional
interpreters@~cite[rompf-15] appears revelant and perhaps paves a way
forward.

Now that we have abstract interpreters formulated with a basis in
abstract machines and with a basis in monadic interpreters, an obvious
question is can we obtain a correspondence between them similar to the
functional correspondence between their concrete
counterparts@~cite[ager-tcs05]?  An interesting direction for future
work is to try to apply the usual tools of defunctionalization, CPS,
and refocusing to see if we can interderive these abstract semantic
artifacts.


