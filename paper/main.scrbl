#lang scribble/sigplan @nocopyright
@(require "bib.rkt")

@title{Abstracting Definitional Interpreters @subtitle{Functional Pearl}}

@authorinfo["David Darais" "University of Maryland" "darais@cs.umd.edu"]
@authorinfo["Nicholas Labich" "University of Maryland" "labichn@cs.umd.edu"]
@authorinfo["Phúc C. Nguyễn" "University of Maryland" "pcn@cs.umd.edu"]
@authorinfo["David Van Horn" "University of Maryland" "dvanhorn@cs.umd.edu"]

@abstract{In this paper, we show that a definitional interpreter
written in monadic style can express not only the usual notion of
interpretation, but also a wide variety of collecting semantics,
abstract interpretations, symbolic execution, and their intermixings.
We give a rational reconstruction of a definitional @emph{abstract}
interpreter for a higher-order language by building a series of
components implementing monadic operations.  The denouement of our
story is a computable abstract interpreter that arises from the
composition of simple, independent components.  Remarkably, this
interpreter implements a form of pushdown control flow analysis
(PDCFA) in which calls and returns are always properly matched in the
abstract semantics.  True to the definitional style of Reynolds, the
evaluator involves no explicit mechanics to achieve this property; it
is simply inherited from the defining language.}

@keywords{definitional interpreters, abstract interpretation, pushdown
control flow analysis, symbolic execution}

@include-section{intro.scrbl}
@include-section{aam.scrbl}
@include-section{defn-interp.scrbl}
@include-section{closures.scrbl}
@include-section{cache.scrbl}
@include-section{fixing-cache.scrbl}
@include-section{reynolds.scrbl}
@include-section{store-widen.scrbl}
@include-section{alt-abstraction.scrbl}
@include-section{symbolic-execution.scrbl}
@include-section{try-it.scrbl}
@include-section{related-work.scrbl}
@include-section{conclusions.scrbl}

@(generate-bibliography)
