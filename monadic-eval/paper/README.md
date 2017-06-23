This directory contains the Scribble source code for the literate
version of "Definitional Abstract Interpreters for Higher-Order
Languages."

(This version of the paper is slightly different from the submitted
version of the paper: it lacks several typsetting improvements made by
hand in the submission and it does not include the appendix which is
written in latex.)

To build the paper, you will need:

- XeLaTeX
- Racket v6.9.0.6 or later (nightly build from 6/23/17)
- Install monadic-eval package (make install from root directory)
- Modified version of the latest version of acmart.cls (see below)

Patching to use (a modified version of!) the latest version of
acmart.cls:

- Get the source code from https://github.com/borisveytsman/acmart
- Comment out line 2613 of acmart.dtx (using libertine)
- latex acmart.ins
- mv acmart.cls RACKET_HOME/share/pkgs/scribble-lib/scribble/acmart/
  (where RACKET_HOME is the directory where Racket is intalled)


Run:
```
   make main.pdf
```

