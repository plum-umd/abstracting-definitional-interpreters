#lang scribble/acmart @acmlarge

@(require "bib.rkt")

@title[#:tag "s:related-work"]{Related work}

This work draws upon and re-presents many ideas from the literature on
abstract interpretation for higher-order
languages @~cite{dvanhorn:Midtgaard2012Controlflow}.  In particular, it
closely follows the abstracting abstract
machines @~cite["dvanhorn:VanHorn2010Abstracting" "dvanhorn:VanHorn2012Systematic"]
approach to deriving abstract interpreters from a small-step
machine. The key difference here is that we operate in the setting of
a monadic definitional interpreter instead of an abstract machine. In
moving to this new setting we developed a novel caching mechanism and
fixed-point algorithm, but otherwise followed the same
recipe. Remarkably, in the setting of definitional interpreters, the
pushdown property for the analysis is simply inherited from the
meta-language rather than requiring explicit instrumentation to the
abstract interpreter.

Compositionally defined abstract interpretation functions for
higher-order languages were first explored by
@citet{local:jones-book1995}, which introduces the technique of
interpreting a higher-order object language directly as terms in a
meta-language to perform abstract interpretation. While their work
lays the foundations for this idea, it does not consider abstractions
for fixed-points in the domain, so although their abstract interpreters
are sound, they are not in general computable. They propose a naïve
solution of truncating the interpretation of syntactic fixed-points to
some finite depth, but this solution isn't general and doesn't account
for non-syntactic occurrences of bottom in the concrete domain (@emph{e.g.}
@emph{via} Y combinators). Our work develops such an abstraction for concrete
denotational fixed-points using a fixed-point caching algorithm, resulting in
general, computable abstractions for arbitrary definitional interpreters.

The use of monads and monad transformers to make extensible (concrete)
interpreters is a well-known
idea @~cite["davdar:Moggi:1989:Monads" "local:steele-popl94" "dvanhorn:Liang1995Monad"],
which we have extended to work for compositional abstract
interpreters.  The use of monads and monad transformers in machine
based-formulations of abstract interpreters has previously been
explored by @citet{dvanhorn:Sergey2013Monadic} and
@citet{local:darais-oopsla2015}, respectively, and inspired our own
adoption of these ideas.  Darais has also shown that certain monad
transformers are also @emph{Galois transformers}, i.e. they compose to
form monads that transport Galois connections.  This idea may pave a
path forward for obtaining both compositional code @emph{and proofs}
for abstract interpreters in the style presented here.

The caching mechanism used to ensure termination in our abstract interpreter is
similar to that used by @citet{dvanhorn:Johnson2014Abstracting}. They use a local- and
meta-memoization table in a machine-based interpreter to ensure termination for
a pushdown abstract interpreter.  This mechanism is in turn reminiscent of
Glück's use of memoization in an interpreter for two-way non-deterministic
pushdown automata @~cite{local:gluck-schmidtfest13}.

Caching recursive, non-deterministic functions is a well-studied
problem in the functional logic programming community under the rubric
of ``tabling'' @~cite["local:tamaki1986old" "local:bol1993tabulated"
  "local:chen1996tabled" "local:swift2012xsb"], and has been usefully
applied to program verification and
analysis @~cite["local:dawson1996practical"
  "local:janssens1998use"]. Unlike these systems, our approach uses a
shallow embedding of cached non-determinism that can be applied in
general-purpose functional languages.
%
Monad transformers that enable shallow embedding of cached non-determinism are
of continued interest since Hinze's @emph{Deriving Backtracking Monad
  Transformers} @~cite["local:hinze2000deriving" "local:kiselyov2005backtracking"
  "local:fischer2011purely"], and recent work @~cite["local:ploeg2014reflection"
  "local:vandenbroucke2016fixing"] points to potential optimizations and
specializations that can be applied to our relatively naive iteration strategy.

Vardoulakis, who was the first to develop the idea of a pushdown abstraction
for higher-order flow analysis @~cite{dvanhorn:Vardoulakis2011CFA2}, formalized
CFA2 using a CPS model, which is similar in spirit to a machine-based model.
However, in his dissertation @~cite{local:vardoulakis-diss12} he sketches an
alternative presentation dubbed ``Big CFA2'' which is a big-step operational
semantics for doing pushdown analysis quite similar in spirit to the approach
presented here.  One key difference is that Big CFA2 fixes a particular coarse
abstraction of base values and closures---for example, both branches of a
conditional are always evaluated.  Consequently, it only uses a single
iteration of the abstract evaluation function, and avoids the need for the
cache-based fixed-point of @secref{s:cache}.  We believe Big CFA2 as
stated is sound, however if the underlying abstractions were tightened, it
may then require a more involved fixed-point finding algorithm like the one we
developed.

Our formulation of a pushdown abstract interpreter computes an abstraction
similar to the many existing variants of pushdown flow analysis 
@~cite["dvanhorn:Vardoulakis2011CFA2"
"dvanhorn:Earl2010Pushdown"
"local:vardoulakis-diss12"
"dvanhorn:VanHorn2012Systematic"
"dvanhorn:Earl2012Introspective"
"dvanhorn:Johnson2014Abstracting"
"dvanhorn:Johnson2014Pushdown"
"local:p4f"].
@;{}
The mixing of symbolic execution and abstract interpretation is
similar in spirit to the @emph{logic flow analysis} of Might
@~cite{local:might-popl07}, albeit in a pushdown setting and with a
stronger notion of negation; generally, our presentation resembles
traditional formulations of symbolic execution more closely
@~cite{dvanhorn:King1976Symbolic}.  Our approach to symbolic execution
only handles the first-order case of symbolic values, as is common.
However, Nguyễn's work on higher-order symbolic execution
@~cite{dvanhorn:Nguyen2015Relatively} demonstrates how to scale to
behavioral symbolic values.  In principle, it should be possible to
handle this case in our approach by adapting Nguyễn's method to a
formulation in a compositional evaluator, but this remains to be
carried out.

Now that we have abstract interpreters formulated with a basis in abstract
machines and with a basis in monadic interpreters, an obvious question is can
we obtain a correspondence between them similar to the functional
correspondence between their concrete
counterparts @~cite{dvanhorn:Ager2005Functional}.  An interesting direction for
future work is to try to apply the usual tools of defunctionalization, CPS, and
refocusing to see if we can interderive these abstract semantic artifacts.
