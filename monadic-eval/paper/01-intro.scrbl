#lang scribble/acmart @acmlarge

@require["bib.rkt"]

@title{Introduction}

An abstract interpreter is intended to soundly and effectively compute
an over-approximation to its concrete counterpart.  For higher-order
languages, these concrete interpreters tend to be formulated as
state-machines (e.g@._ @citet{dvanhorn:jagannathan-weeks-popl95};
@citet{dvanhorn:jagannathan-etal-popl98};
@citet{dvanhorn:wright-jagannathan-toplas98};
@citet{dvanhorn:Might:2006:DeltaCFA};
@citet{dvanhorn:midtgaard-jensen-sas-08};
@citet{dvanhorn:Midtgaard2009Controlflow};
@citet{dvanhorn:Might2011Family}; and
@citet{dvanhorn:Sergey2013Monadic}).  There are several reasons for
this choice: they operate with simple transfer functions defined over
similarly simple data structures, they make explicit all aspects of
the state of a computation, and computing fixed-points in the set of
reachable states is straightforward.  The essence of the state-machine
based approach was distilled by Van Horn and Might in their
``abstracting abstract machines'' (AAM) technique, which provides a
systematic method for constructing abstract interpreters from standard
abstract machines like the CEK- or Krivine-machines
@~cite{dvanhorn:VanHorn2010Abstracting}.  Language designers who would
like to build abstract interpreters and program analysis tools for
their language can now, in principle at least, first build a
state-machine interpreter and then turn the crank to construct the
approximating abstract counterpart.

A natural question to arise from this past work is to wonder: can a
systematic abstraction technique similar to AAM be carried out for
interpreters written, @emph{not} as state-machines, but instead as
high-level definitional interpreters, i.e. recursive, compositional
evaluators?  This functional pearl answers in the affirmative and
demonstrates some of the interesting consequences of doing so.

First, we show the AAM recipe can be applied to definitional
interpreters with only a slight adaptation of the original method. The
primary technical challenge in this new setting is handling
interpreter fixed-points in a way that is both sound and always
terminates---a naive abstraction of fixed-points will be sound but
isn't always terminating, and a naive use of caching for fixed-points
will guarantee termination but is inherently unsound. We address this
technical challenge with a caching fixed-point-finding algorithm which
is both sound and guaranteed to terminate when abstracting arbitrary
definitional interpreters.

Second, we claim that the abstract definitional interpreter
perspective is fruitful in two regards.  The first is unsurprising:
high-level abstract interpreters offer the usual beneficial properties
of their concrete counterparts in terms of being re-usable and
extensible.  In particular, we show that abstract interpreters can be
structured with monad transformers to good effect.  The second regard
is more surprising, and we consider its observation to be the main
contribution of this pearl.

Definitional interpreters, in contrast to abstract machines, can leave
aspects of computation implicit, relying on the semantics of the
defin@emph{ing}-language to define the semantics of the
defin@emph{ed}-language, an observation made by Reynolds in his
landmark paper, @emph{Definitional Interpreters for Higher-order
Programming Languages} @~cite{dvanhorn:reynolds-acm72}.  For example,
Reynolds showed it is possible to write a definitional interpreter
such that it defines a call-by-value language when the metalanguage is
call-by-value, and defines a call-by-name language when the
metalanguage is call-by-name.  Inspired by Reynolds, we show that
@emph{abstract} definitional interpreters can likewise inherit
properties of the metalanguage.  In particular we construct an
abstract definitional interpreter where there is no explicit
representation of continuations or a call stack.  Instead the
interpreter is written in a straightforward recursive style, and the
call stack is implicitly handled by the metalangauge.  What emerges
from this construction is a total abstract evaluation function that
soundly approximates all possible concrete executions of a given
program.  But remarkably, since the abstract evaluator relies on the
metalanguage to manage the call stack implicitly, it is easy to
observe that it introduces no approximation in the matching of calls
and returns, and therefore implements a ``pushdown'' analysis
@~cite["dvanhorn:Earl2010Pushdown" "dvanhorn:Vardoulakis2011CFA2"],
all without the need for any explicit machinery to do so.

@section[#:style 'unnumbered]{Outline}

In the remainder of this pearl, we present an adaptation of the AAM
method to the setting of recursively-defined, compositional evaluation
functions, a.k.a. definitional interpreters.  We first briefly review
the basic ingredients in the AAM recipe (@secref{s:aam}) and then
define our definitional interpreter (@secref{s:interp}).  The
interpreter is largely standard, but is written in a monadic and
extensible style, so as to be re-usable for various forms of semantics
we examine.  The AAM technique applies in a basically straightforward
way by store-allocating bindings and soundly finitizing the heap.  But
when naively run, the interpreter will not always terminate.  To solve
this problem we introduce a caching strategy and a simple fixed-point
computation to ensure the interpreter terminates (@secref{s:cache}).
It is at this point that we observe the interpreter we have built
enjoys the ``pushdown'' property @emph{à la} Reynolds---it is
inherited from the defining language of our interpreter and requires
no explicit mechanism (@secref{s:reynolds}).

Having established the main results, we then explore some variations
in brief vignettes that showcase the flexibility of our definitional
abstract interpreter approach.  First we consider the widely used
technique of so-called ``store-widening,'' which trades precision for
efficiency by modelling the abstract store globally instead of locally
(@secref{s:widening}).  Thanks to our monadic formulation of the
interpreter, this is achieved by a simple re-ordering of the monad
transformer stack.  We also explore some alternative abstractions,
showing that due to the extensible construction, it's easy to
experiment with alternative components for the abstract interpreter.
In particular, we define an alternative interpretation of the primitive
operations that remains completely precise until forced by joins in
the store to introduce approximation (@secref{s:alt-abstraction}).  As
another variation, we explore computing a form of symbolic execution
as yet another instance of our interpreter (@secref{s:symbolic}).  Lastly, we
show how to incorporate so-called ``abstract garbage collection,'' a
well-known technique for improving the precision of abstract
interpretation by clearing out unreachable store locations, thus
avoiding future joins which cause imprecision (@secref{s:gc}).  This
last variation is significant because it demonstrates that even though
we have no explicit representation of the stack, it is possible to
compute analyses that typically require such explicit representations
in order to calculate root sets for garbage collection.

Finally, we place our work in the context of the prior literature on
higher-order abstract interpretation (@secref{s:related-work}) and draw
some conclusions (@secref{s:conclusion}).


@section[#:style 'unnumbered]{Style}

To convey the ideas of this paper as concretely as possible, we
present code implementing our definitional abstract interpreter and
all its variations.  As a metalanguage, we use an applicative subset
of Racket @~cite{dvanhorn:plt-tr1}, a dialect of Scheme.  This choice
is largely immaterial: any functional language would do.  However, to
aide extensibility, we use Racket's @emph{unit}
system @~cite{local:flatt-pldi98} to write program components that can
be linked together.

All of the code presented in this pearl runs; this document is a
literate Racket program.  We have also implemented a small DSL for
composing and experimenting with these interpreters easily.  Assuming
Racket is installed, you can install the @tt{monadic-eval} package with
@bold{(URL redacted for double-blind)} and a brief tutorial is
available on github.
