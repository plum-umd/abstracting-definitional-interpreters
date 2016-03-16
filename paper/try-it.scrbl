#lang scribble/manual
@(require scriblib/figure
          "bib.rkt")

@title{Try It Out}

All of the components discussed in this paper have been implemented as
units@~cite[flatt-pldi98] in Racket@~cite[plt-tr1].  We have also
implemented a @smaller{@tt{#lang}} language so that composing and
experimenting with these interpreters is easy.  Assuming Racket is
installed, you can install the @smaller{@tt{monadic-eval}} package
with (URL redacted for double-blind):

@centered{
@verbatim[#:indent 2]{
raco pkg install \ 
  https://github.com/<anon>/monadic-eval.git}}

A @smaller{@tt{#lang monadic-eval}} program starts with a list of
components, which are linked together, and an expression producing an
evaluator.  Subsequent forms are interpreted as expressions when run.
Programs can be run from the command-line or interactively in the
DrRacket IDE.  For example, @figure-ref{screen} shows a screen shot of the PDCFA
evaluator running the example from @secref{fixing-cache}.

@figure["screen" "Screenshot of monadic language"]{
@image[#:scale .29]{screen.pdf}}


