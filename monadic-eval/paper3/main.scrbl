#lang scribble/acmart @acmlarge
@require["bib.rkt"]

@title{Abstracting Definitional Interpreters}
@subtitle{Functional Pearl}

@abstract{In this functional pearl, we examine the use of
 definitional interpreters as a basis for abstract
 interpretation of higher-order programming languages. As it
 turns out, definitional interpreters, especially those
 written in monadic style, can provide a nice basis for a
 wide variety of collecting semantics, abstract
 interpretations, symbolic executions, and their
 intermixings.

 But the real insight of this story is a replaying of an
 insight from Reynold's landmark paper, @emph{Definitional
  Interpreters for Higher-Order Programming Languages}, in
 which he observes definitional interpreters enable the
 defined-language to inherit properties of the
 defining-language. We show the same holds true for
 definitional @emph{abstract} interpreters. Remarkably, we
 observe that abstract definitional interpreters can inherit
 the so-called ``pushdown control flow'' property, wherein
 function calls and returns are precisely matched in the
 abstract semantics, simply by virtue of the function call
 mechanism of the defining-language.

 The first approaches to achieve this property for
 higher-order languages appeared within the last ten years,
 and have since been the subject of many papers. These
 approaches start from a state-machine semantics and
 uniformly involve significant technical engineering to
 recover the precision of pushdown control flow. In contrast,
 starting from a definitional interpreter, the pushdown
 control flow property is inherent in the meta-language and
 requires no further technical mechanism to achieve.}

@include-section{01-intro.scrbl}
@include-section{02-aam.scrbl}
@include-section{03-interp.scrbl}
@include-section{04-cache.scrbl}
@include-section{05-reynolds.scrbl}
@include-section{06-widening.scrbl}
@include-section{07-alt-abstraction.scrbl}
@include-section{08-symbolic-execution.scrbl}
@include-section{09-gc.scrbl}
@include-section{10-related-work.scrbl}
@include-section{11-conclusions.scrbl}


@(generate-bib)
