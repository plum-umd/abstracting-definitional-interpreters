#lang scribble/acmart @acmlarge

@require[scriblib/bibtex scriblib/autobib]
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

A natural pair of questions that arise from this past work is to
wonder:
@itemlist[#:style 'ordered
@item{can a systematic abstraction technique similar to AAM  be
carried out for interpreters written, @emph{not} as state-machines,
but instead as high-level definitional interpreters, i.e. recursive,
compositional evaluators?}
@item{is such a perspective fruitful?}
]
In this functional pearl, we seek to answer both questions in the
affirmative.

For the first question, we show the AAM recipe can be applied
to definitional interpreters in a straightforward adaptation of the
original method. The primary technical challenge in this new setting
is handling interpreter fixed-points in a way that is both sound and
always terminates---a naive abstraction of fixed-points will be sound but
isn't always terminating, and a naive use of caching for fixed-points
will guarantee termination but is inherently unsound. We address this
technical challenge with a straightforward caching fixed-point-finding
algorithm which is both sound and guaranteed to terminate when abstracting
arbitrary definitional interpreters.

For the second question, we claim that the abstract definitional
interpreter perspective is fruitful in two regards.  The first is
unsurprising: high-level abstract interpreters offer the usual
beneficial properties of their concrete counterparts in terms of being
re-usable and extensible.  In particular, we show that abstract
interpreters can be structured with monad transformers to good effect.
The second regard is more surprising, and we consider its observation
to be the main contribution of this pearl.

Definitional interpreters, in contrast to abstract machines, can leave
aspects of computation implicit, relying on the semantics of the
defin@emph{ing}-language to define the semantics of the
defin@emph{ed}-language, an observation made by Reynolds in his landmark
paper, @emph{Definitional Interpreters for Higher-order Programming
  Languages} @~cite{dvanhorn:reynolds-acm72}.  For example, Reynolds showed it is
possible to write a definitional interpreter such that it defines a
call-by-value language when the metalanguage is call-by-value, and
defines a call-by-name language when the metalanguage is call-by-name.
Inspired by Reynolds, we show that @emph{abstract} definitional interpreters can likewise
inherit properties of the metalanguage.  In particular we construct an
abstract definitional interpreter where there is no explicit
representation of continuations or a call stack.  Instead the
interpreter is written in a straightforward recursive style, and the
call stack is implicitly handled by the metalangauge.  What emerges
from this construction is a total abstract evaluation function that
soundly approximates all possible concrete executions of a given
program.  But remarkably, since the abstract evaluator relies on the
metalanguage to manage the call stack implicitly, it is easy to
observe that it introduces no approximation in the matching of calls
and returns, and therefore implements a ``pushdown''
analysis @~cite{dvanhorn:Earl2010Pushdown
  dvanhorn:Vardoulakis2011CFA2}, all without the need for any explicit
machinery to do so.

