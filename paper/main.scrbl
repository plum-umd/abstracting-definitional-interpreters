#lang scribble/sigplan @nocopyright
@(require scribble/manual)
@(require scribble/decode)
@(require scriblib/figure)
@(require "bib.rkt")


@title{Abstracting Definitional Interpreters}
@;title{Definitional Abstract Interpreters for Higher-Order Programming Languages}
@;subtitle{Functional Pearl}

@authorinfo["David Van Horn" "Northeastern University" "dvanhorn@ccs.neu.edu"]

@abstract{A definitional interpreter written in monadic style can
express a wide variety of abstract interpretations.  We give a
rational reconstruction of a definitional abstract interpreter for a
higher-order language by constructing a series of units implementing
monadic operations.  The denouement of our story is a sound and
computable abstract interpreter that arises from the composition of
simple, independent components.  Remarkably, this interpreter
implements a form of pushdown control flow analysis (PDCFA) in which
calls and units are always properly matched in the abstract
semantics.  True to the definitional style of Reynolds, the evaluator
involves no explicit mechanics to achieve this property; it is simply
inherited from the defining language.}

@keywords{definitional interpreters, abstract interpretation, pushdown
control flow analysis}

@section{Introduction}

In his landmark paper, @emph{Definitional interpreters for
higher-order languages}@~cite[reynolds72], Reynolds first observed
that when a language is defined by way of an interpreter, it is
possible for the defined language to inherit semantic characteristics
of the defining language of the interpreter.  For example, it is easy
to write a compositional evaluator that defines a call-by-value
language if the defining language is call-by-value, but defines a
call-by-name language if the defining language is call-by-name.

In this paper, we make the following two observations:

@itemlist[#:style 'ordered

@item{Definitional interpreters, written in monadic style,
can simultaneously define a language's semantics as well as
safe approximations of those semantics.}

@item{These definitional abstract interpreters can inherit
characteristics of the defining language.  In particular, we show that
the abstract interpreter inherits the call and unit matching
property of the defining language and therefore realizes an abstract
intpretation in the pushown style of analyses@~cite[cfa2-diss
pdcfa-sfp10].}

]

A common problem of past approaches to the control flow analysis of
functional languages is the inability to properly match a function
call with its unit in the abstract semantics, leading to infeasible
program (abstract) executions in which a call is made from one point
in the program text, but control units to another.  The CFA2
analysis of Vardoulakis and Shivers@~cite[cfa2-lmcs] was the first
approach that overcame this shortcoming.  In essence, this kind of
analysis can be viewed as replacing the traditional finite automata
abstractions of programs with pushdown automata@~cite[pdcfa-sfp10].


@;{
@subsection{Notation and terminology}

Our technical development is carried out in Racket [Racket TR], a Lisp
dialect.  We assume a basic familiarity with definitional interpreters
in the style of Landin [Landin].
}

@section{A Definitional Interpreter}

We begin by giving a definitional interpreter for an archetypal
applicative higher order language.

@subsection{Applicative higher-order language}

The following grammar defines the abstract syntax:
@codeblock[#:keep-lang-line? #f]|{
  #lang racket
  ;; E  = (vbl X)       Variable
  ;;    | (num Number)  Number
  ;;    | (lam X E)     Lambda
  ;;    | (ifz E E E)   Conditional
  ;;    | (op1 O1 E)    Unary primitive
  ;;    | (op2 O2 E E)  Binary primitive
  ;;    | (app E E)     Application
  ;;    | (lrc X E E)   Letrec
  ;;  X = Symbol        Variable name
  ;; O1 = 'add1 | ...   Unary operator
  ;; O2 = '+ | '- | ... Binary operator
}|

The result of evaluation is a value computation.  Values in this
language include numbers and functions, represented as closures; heaps
are maps from addresses to values:

@codeblock[#:keep-lang-line? #f]|{
  #lang racket
  ;; V = Number              Values
  ;;   | (cons (lam X E) R)
  ;; R = (X ↦ A)            Environments
}|


We define the meaning of a program (a closed expression) by way of an
evaluation function that compositionally interprets expressions.  The
evaluator, given in @figure-ref{pcf-eval}.


@figure["pcf-eval" "Definitional interpreter"]{
@codeblock[#:keep-lang-line? #t]|{
(define (ev e r) ;; E R -> [M V]
  (match e
    [(vbl x) (_lookup-env r x)]
    [(num n) (_unit n)]
    [(lam x e) (_unit (cons (lam x e) r))]
    [(ifz e0 e1 e2)
     (do v ← (_ev e0 r)
       (match v
         [0 (_ev e1 r)]
         [(? number?) (_ev e2 r)]))]
    [(op1 o e0)
     (do v ← (_ev e0 r)
       (_δ o v))]
    [(op2 o e0 e1)
     (do v0 ← (_ev e0 r)
         v1 ← (_ev e1 r)
       (_δ o v0 v1))]
    [(app e0 e1)
     (do v0 ← (_ev e0 r)
         v1 ← (_ev e1 r)
       (match v0
         [(cons (lam x e) r0)
          (do a ← (_alloc v0 v1)
            (_ev e (ext-env r0 x a)))]))]
    [(lrc f (lam x e0) e)
     (do a ← (_ralloc f (cons (lam x e0) r))
       (_ev e (ext-env r f a)))]))

}|
}

@~cite[flatt-pldi98]

The evaluator is written in monadic style using @racket[do] notation,
which is syntactic sugar for @racket[_bind]:

@racketblock[
(do b) ≡ b
(do x ← e . r) ≡ (_bind e (λ (x) (do . r)))
]

The evaluator is implicity parameterized over a set of names written
in italic, namely: @racket[_unit], @racket[_bind],
@racket[_lookup-env], @racket[_alloc], @racket[_ralloc], and even
@racket[_ev], which should not be confused with @racket[ev].  Compared
with other monadic evaluators, such as that of @citet[ager-tcs05],
which use only @racket[_bind] and @racket[_unit], this evaluator makes
use of several more operations.  We give a brief description and
motivation of the additional operations:

@itemlist[

@item{@racket[_lookup-env]: this operation produces a value
computation from a variable and environment.  By making the
environment lookup an operation in the monad, our evaluator take a
number of implementation strategies for implementing binding.  In
particular, if we would like variables to bound in the heap, we want
@racket[_lookup-env] to produce a function that dereferences the
location denoted by the variable in the heap.}

@item{@racket[_alloc]: this operation produces an ``address'' to
allocate for the binding of a variable.  The choice of addresses is up
to the monad implementation, but forms the domain of the environment.}

@item{@racket[_ralloc]: similar to @racket[_alloc] but for binding a
recursive function.}

@item{@racket[_ev]: this operation invokes a recursive call to the
evaluator.  It is an operation in the monad in order to allow the
monad implementation to observe evaluation of subexpressions.}

]


@figure["delta" "Delta"]{
@racketblock[
(define-unit delta@
  (import return^)
  (export δ^)
  (define (δ o . vs)
    (_return
      (match* (o vs)
        [('add1 (list n)) (add1 n)]
        [('sub1 (list n)) (sub1 n)]
        [('+ (list n1 n2)) (+ n1 n2)]
        [('- (list n1 n2)) (- n1 n2)]
        [('* (list n1 n2)) (* n1 n2)]))))
]}

@figure["implicit-store" "Implicit store"]{
@racketblock[
(import return^)
(export sto-monad^)

(define (lookup-env r x)
  (return (hash-ref r x)))

(define (alloc f v)
  (return v))

(define (ralloc x v)
  (match v
    [(cons e r)
     (define p (make-placeholder #f))
     (define f (cons e (hash-set r x p)))
     (placeholder-set! p f)
     (_unit (make-reader-graph f))]))

(define (new v)
  (return (box v)))

(define (sbox a v)
  (set-box! a v)
  (return a))

(define (ubox a)
  (return (unbox a)))
]}


@figure["eval" "Eval unit"]{
@racketblock[
(define-unit eval@
  (import ev^)
  (export eval^ return^ ev-monad^)
  (define (eval e) (_ev e (hash)))
  (define (ev e r) (_ev e r))
  (define (unit v) v)
  (define (bind v f) (f v)))
]}




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

@racketblock[
(import unit^ δ^ ev-monad^ sto-monad^)
(export ev^)
]

@racketblock[
(define-signature unit^ (unit))
(define-signature δ^ (δ))
(define-signature ev-monad^ (bind ev))
(define-signature sto-monad^
  (lookup-env alloc ralloc))
]


@subsection{Imperative higher-order language}



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
    [(sym x) (_unit (cons x s))]
    ...))


}|


@section{Related work}

Danvy, monadic interpreters and abstract machines.

@subsection{Monadic interpreters}

@~cite[steele-popl94 liang-popl95]

@subsection{Monadic abstract interpreters}

PLDI 2013: small-step monad.

@subsection{Big CFA2}

@~cite[cfa2-diss]


@section{Conclusion}

@bold{Acknowledgments}: Sam Tobin-Hochstadt, J. Ian Johnson, Olivier Danvy.

@(generate-bibliography)
