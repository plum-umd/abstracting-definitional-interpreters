#lang scribble/acmart @acmlarge
@(require "bib.rkt")

@title[#:tag "s:conclusion"]{Conclusions}

We have shown that the AAM methodology can be adapted to definitional
interpreters written in monadic style.  Doing so captures a wide
variety of semantics, such as the usual concrete semantics, collecting
semantics, and various abstract interpretations.  Beyond recreating
existing techniques from the literature such as store-widening and
abstract garbage collection, we can also design novel abstractions and
capture disparate forms of program analysis such as symbolic
execution.  Further, our approach enables the novel combination of
these techniques.

To our surprise, the definitional abstract interpreter we obtained
implements a form of pushdown control flow abstraction in which calls
and returns are always properly matched in the abstract semantics.
True to the definitional style of Reynolds, the evaluator involves no
explicit mechanics to achieve this property; it is simply inherited
from the metalanguage.

We believe this formulation of abstract interpretation offers a
promising new foundation towards re-usable components for the static
analysis and verification of higher-order programs.  Moreover, we
believe the definitional abstract interpreter approach to be a
fruitful new perspective on an old topic.  We are left wondering: what
else can be profitably inherited from the metalanguage of an abstract
interpreter?

@acks{The seeds of this work were planted while at the Northeastern
  University Programming Research Laboratory and the ideas benefited
  greatly from lively discussions within the PRL.  In particular, we
  thank Sam Tobin-Hochstadt and Dionna Glaze for several fruitful
  conversations.  We also thank the people of the Laboratory for
  Programming Languages at the University of Maryland for help
  developing these ideas more recently.  Finally, we are grateful for
  the constructive feedback from the anonymous reviewers of ICFP 2016
  and 2017.  We thank Reviewer B of ICFP 2017 in particular for the
  reference to the @citet{local:friedman-ai} tutorial, @emph{Using an
  Abstracted Interpreter to Understand Abstract Interpretation}, an
  (unknown to us) precursor to the current work that anticipated our
  use of interpreters written using open recursion to explain abstract
  intepretation.}
