#lang scribble/sigplan @nocopyright
@require[scribble/manual]
@(require scribble/decode)
@(require scriblib/figure)

@title{Abstracting Definitional Interpreters}
@;title{Definitional Abstract Interpreters for Higher-Order Programming Languages}
@subtitle{Functional Pearl}

@authorinfo["David Van Horn" "Northeastern University" "dvanhorn@ccs.neu.edu"]

@abstract{A definitional interpreter written in monadic style can
express a wide variety of abstract interpretations.  We give a
rational reconstruction of a definitional abstract interpreter for a
higher-order language by constructing a series of units implementing
monadic operations.  The denouement of our story is a sound and
computable abstract interpreter that arises from the composition of
simple, independent components.  Remarkably, this interpreter
implements a form of pushdown control flow analysis (PDCFA) in which
calls and returns are always properly matched in the abstract
semantics.  True to the definitional style, the evaluator involves no
explicit mechanics to achieve this property; it is simply inherited
from the defining language.}

@section{Introduction}

@subsection{Notation and terminology}

Our technical development is carried out in Racket [Racket TR], a Lisp
dialect.  We assume a basic familiarity with definitional interpreters
in the style of Landin [Landin].

@section{A Definitional Interpreter}

We begin by giving a definitional interpreter for a prototypical
higher order language.

The following grammar defines the abstract syntax:
@codeblock[#:keep-lang-line? #f]|{
  #lang racket
  ;;  E  = (vbl X)       Variable
  ;;    |  (num Number)  Number
  ;;    |  (lam X E)     Lambda
  ;;    |  (app E E)     Application
  ;;    |  (op1 O E)     Unary primitive
  ;;    |  (op2 O E E)   Binary primitive
  ;;    |  (ifz E E E)   Conditional
  ;;  X = Symbol         Variable name
  ;; O1 = 'add1 | ...    Unary operator
  ;; O2 = '+ | '- | ...  Binary operator
}|
The result of evaluation is called an @emph{answer} and consists of a
value and a store (a heap).  Values in this language include numbers
and functions, represented as closures; heaps are maps from addresses
to values:
@codeblock[#:keep-lang-line? #f]|{
  #lang racket
  ;; Ans = (cons V H)          Answers
  ;;   V = Number              Values
  ;;     | (cons (lam X E) R)
  ;;   R = (X ↦ A)            Environments
  ;;   A = Symbol              Addresses
  ;;   H = (A ↦ V)            Heaps
}|


We define the meaning of a program (a closed expression) by way of an
evaluation function that compositionally interprets expressions.  The
evaluator, given in @figure-ref{pcf-eval}, is written in the usual
definitional interpreter style [Landin, Friedman & Wand], with some
minor exceptions: (1) it is written in monadic style, (2) it uses a
store and binds variables to values via the store, and (3) it is
implicitly parameterized by a few metafunctions, indicated with
italics.  Each of these choices facilitate the subsequent abstraction.


@figure["pcf-eval" "Definitional interpreter"]{
@codeblock[#:keep-lang-line? #f]|{
#lang racket
(define (ev e r s) ;; E R S -> Ans
  (match e
    [(vbl x)
     (do (lookup s r x)
       [v (_return (cons v s))])]
    [(num n) (_return (cons n s))]
    [(ifz e0 e1 e2)
     (do (ev e0 r s)
       [(cons 0 s) (ev e1 r s)]
       [(cons n s) (ev e2 r s)])]
     [(op1 o e0)
      (do (ev e0 r s)
        [(cons v s)
         (_return (_δ o v s))])]
     [(op2 o e0 e1)
      (do (ev e0 r s)
        [(cons v0 s)
         (do (ev e1 r s)
           [(cons v1 s)
            (_return (_δ o v0 v1 s))])])]
     [(lam x e)
      (_return (cons (cons (lam x e) r) s))]
     [(app e0 e1)
      (do (ev e0 r s)
        [(cons v0 s)
         (do (ev e1 r s)
           [(cons v1 s) (ap v0 v1 s)])])]))

(define (av v0 v1 s) ;; V V S -> Ans
  (match v0
    [(cons (lam x e) r0)
     (define a (alloc v0 v1 s))
     (ev e
         (extend-env r0 x a)
         (update-sto s a v1))]))
}|
}

The evaluator is written in monadic style using
@racket[do] and @racket[_return].  The @racket[do] syntax is simple
syntactic sugar for
@racket[_bind]:
@codeblock[#:keep-lang-line? #f]|{
#lang racket
   (do e c ...)
     ≡ (_bind e (λ (r) (match r c ...)))
}|
The @racket[_bind] and @racket[_return] functions should, for the moment,
be thought of as implementing the identity monad:
@codeblock[#:keep-lang-line? #f]|{
#lang racket
      _return ≡ (λ (x) x)
        _bind ≡ (λ (r f) (f r))
}|
By writing the interpreter in monadic style, we can, simply through
alternate definition of @racket[_bind] and @racket[_return], express
computations producing a @emph{set} or @emph{tree} of values.

By using a store-based evaluator, it becomes easy to model imperative
features, but more importantly, the store becomes a singe point of
approximation and, as will be shown, if the store is bounded to some
finite size, the interpreter becomes total.  This is just following
the same approach of @emph{Abstracting Abstract Machines} [Van Horn
and Might], but in the setting of a compositional interpreter.

The environment is managed with @racket[lookup] and @racket[extend]:
the @racket[lookup] function fetches a value of a variable by looking
up the variables address in the environment, then dereferencing that
adress in the store; the @racket[extend] function extends an
environment to bind a variable to an address.  Both are defined as
usual:
@racketblock[
(lookup s r x) ≡ @#,elem{@racket[s](@racket[r](@racket[x]))}
(extend r x a) ≡ @#,elem{@racket[r]' where @racket[r]'(@racket[x]') @racket[=] @racket[a] if @racket[x] @racket[=] @racket[x]',}
                     @#,elem{and @racket[r]'(@racket[x]') @racket[=] @racket[r](@racket[x]') otherwise.}
]

Finally, the evaluator is parameterized over the metafunctions for
managing the store.  For the purposes of the definitional interpreter, we
assume the following definitions:
@racketblock[
(_alloc v0 v1 s) ≡ @#,elem{@racket[a] such that @racket[a] is not in @racket[s]}
(_update s a v)  ≡ @#,elem{@racket[s]' where @racket[s]'(@racket[a]') @racket[=] @racket[v] if @racket[a] @racket[=] @racket[a]',}
                     @#,elem{and @racket[s]'(@racket[a]') @racket[=] @racket[s](@racket[a]') otherwise.}
]

At this point, we've established a fairly run of the mill definitional
evaluator in monadic style.  As we'll see, turning it into a
sound and total abstract interpreter is just a matter of plugging in
the right parameters.  But first, we take a slight detour and extend
our evaluator to handle @emph{symbolic} data.

@section{Symbolic Evaluation}

We have two primary motivations for extending the definitional
evaluator to interpret @emph{symbolic} values:

@itemlist[#:style 'numbered
@item{Abstracting base values

When designing an abstract interpreter, an important task is
abstracting the base values of the language.  Often, this abstraction
can be expressed in terms of giving a semantics to symbolic values.
As an example, our model's only base type is number, which
includes an inifinite set of values.  We can give a finite abstraction
of this set by considering an abstract number as being either one of
the set of numbers that @emph{literally} appear in a given program, or
the symbolic value ``number,'' written @racket['N].  The addition of
the @racket['N] value requires extending the @racket[_δ]
function to interpret operations over arguments such as @racket['N].
}

@item{Modularity

An important, but typically missing, aspect of program analysis is
being able to analyze program modules with unknown or parametric
components.  This can be handled by treating unknown values as
symbolic.
}
]

We extend the language to include symbolic terms.  A symbolic term evaluates to
its symbol, which we take to stand for the value of an arbitrary
(well-typed) program.
@codeblock[#:keep-lang-line? #f]|{
#lang racket
;;  E = ...
;;    | (sym Symbol)      Symbolic term
;;  V = ...
;;    | Sexpr             Symbolic value

;; Sexpr = Symbol
;;       | (list V V)
;;       | (list O1 V)
;;       | (list O2 V V)

(define (eval e r s)
  (match e
    [(sym x) (_return (cons x s))]
    ...))


}|


@section{Related work}

PLDI 2013: small-step monad.

Danvy, monadic interpreters and abstract machines.

CFA2

Big CFA2 in Dimitris' diss.

Acknowledgments: Sam Tobin-Hochstadt, J. Ian Johnson, Olivier Danvy.