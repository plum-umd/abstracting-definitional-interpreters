#lang scribble/sigplan @10pt @nocopyright
@(require scribble/manual)
@(require scribble/decode)
@(require scriblib/figure)
@(require "bib.rkt")

@(require scribble/eval)

@(define the-pure-eval
  (make-base-eval #:pretty-print? #f
		  #:lang 'monadic-eval/lang '(link eval-pure@ ev@ δ@ env@ err@)))

@(define the-box-eval
  (make-base-eval #:pretty-print? #f
		  #:lang 'monadic-eval/lang '(link eval-pure@ ev@ δ@ env-box@ err@)))

@(define the-explicit-sto-eval
  (make-base-eval #:pretty-print? #f
		  #:lang 'monadic-eval/lang 
		  '(link eval-explicit@ ev!@ δ@ sto-explicit@)
		  #;'(link eval-sto@ sto-monad@ ev@ δ@ env-sto@)))

@(define the-trace-eval
  (make-base-eval #:pretty-print? #f
		  #:lang 'monadic-eval/lang 
		  '(link trace@ ev!@ δ@ env-sto@ sto@))
		  #;'(link eval-trace@ ev@ δ@ env-sto@))

@(define the-reachable-eval
  (make-base-eval #:pretty-print? #f
		  #:lang 'monadic-eval/lang 
		  '(link reach@ ev@ δ@ env-sto@)
		  #;'(link eval-reachable@ ev@ δ@ env-sto@)))


@title{Abstracting Definitional Interpreters}
@;title{Definitional Abstract Interpreters for Higher-Order Programming Languages}
@;subtitle{Functional Pearl}

@authorinfo["David Van Horn" "University of Maryland" "dvanhorn@cs.umd.edu"]

@abstract{A definitional interpreter written in monadic style can
express a wide variety of abstract interpretations.  We give a
rational reconstruction of a definitional abstract interpreter for a
higher-order language by constructing a series of units implementing
monadic operations.  The denouement of our story is a sound and
computable abstract interpreter that arises from the composition of
simple, independent components.  Remarkably, this interpreter
implements a form of pushdown control flow analysis (PDCFA) in which
calls and returns are always properly matched in the abstract
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
the abstract interpreter inherits the call and return matching
property of the defining language and therefore realizes an abstract
intpretation in the pushown style of analyses@~cite[cfa2-diss
pdcfa-sfp10].}

]

A common problem of past approaches to the control flow analysis of
functional languages is the inability to properly match a function
call with its return in the abstract semantics, leading to infeasible
program (abstract) executions in which a return is made from one point
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
@filebox[@racket[ev@]]{
@racketblock[
(import unit^ bind^ δ^ env^)
(export ev^)

(define (ev e r) ;; E R -> [M V]
  (match e
    [(vbl x) (_get r x)]
    [(num n) (_unit n)]
    [(lam x e) (_unit (cons (lam x e) r))]
    [(ifz e0 e1 e2)
     (do v ← (_rec e0 r)
       (match v
         [0 (_rec e1 r)]
         [n (_rec e2 r)]))]
    [(op1 o e0)
     (do v ← (_rec e0 r)
       (_δ o v))]
    [(op2 o e0 e1)
     (do v0 ← (_rec e0 r)
         v1 ← (_rec e1 r)
       (_δ o v0 v1))]
    [(app e0 e1)
     (do v0 ← (_rec e0 r)
         v1 ← (_rec e1 r)
       (match v0
         [(cons (lam x e) r0)
          (do a ← (_alloc v0 v1)
            (_rec e (ext-env r0 x a)))]))]
    [(lrc f (lam x e0) e)
     (do a ← (_ralloc f (cons (lam x e0) r))
       (_rec e (ext-env r f a)))]))
]}}

@~cite[flatt-pldi98]

The evaluator is written in monadic style using @racket[do] notation,
which is syntactic sugar for @racket[_bind]:

@racketblock[
(do b) ≡ b
(do x ← e . r) ≡ (_bind e (λ (x) (do . r)))
]

The evaluator is implicity parameterized over a set of names written
in italics, namely: @racket[_unit], @racket[_bind], @racket[_δ],
@racket[_get], @racket[_alloc], @racket[_ralloc], and @racket[_rec].
Compared with other monadic evaluators, such as that of
@citet[ager-tcs05], which use only @racket[_bind] and @racket[_unit],
this evaluator makes use of several more operations.  We give a brief
description and motivation of the additional operations:

@itemlist[

@item{@racket[_δ]: this function interprets primitive operations.
By making primitive interpretation an operation in the monad, we
can provide non-standard intpretations of primitives.}

@item{@racket[_get]: this operation produces a value
computation from a variable and environment.  By making the
environment lookup an operation in the monad, our evaluator take a
number of implementation strategies for implementing binding.  In
particular, if we would like variables to bound in the heap, we want
@racket[_get] to produce a function that dereferences the
location denoted by the variable in the heap.}

@item{@racket[_alloc]: this operation produces an ``address'' to
allocate for the binding of a variable.  The choice of addresses is up
to the monad implementation, but forms the domain of the environment.}

@item{@racket[_ralloc]: similar to @racket[_alloc] but for binding a
recursive function.}

@item{@racket[_rec]: this operation invokes a recursive return to the
evaluator.  It is an operation in the monad in order to allow the
monad implementation to observe evaluation of subexpressions.}

]

At this point, we have established the skeleton of a fairly run of the
mill definitional interpreter in monadic style.  We use Racket's unit
system to organize components and define the following signatures:
@racketblock[
   (define-signatures
     [eval^ : eval]
     [ev^   : ev]
     [unit^ : unit]
     [bind^ : bind]
     [rec^  : rec]
     [δ^    : δ]
     [env^  : get alloc ralloc])
]

[A quick primer on units.]

@subsection{Implementation 1: Circular immutable closures}

@Figure-ref{eval-units} implements a set of units that enable the
construction of a first evaluator.  The implementation strategy uses
the identity monad and constructs recursive functions by way of
circular closures.  The implementation of @racket[ralloc] in
@racket[env@] makes use of @racket[make-placeholder], which is
Racket's mechanism for constructing immutable circular data.

The evaluation function is obtained with the following linkage:
@centered{
@racket[(link eval@ ev@ id-monad@ δ@ env@)]
}

@examples[#:eval the-pure-eval
                 (rec f (λ (x) f) f)]

@figure["eval-units" "Units for concrete evaluator"]{
@filebox[@racket[eval@]]{
@racketblock[
(import rec^)
(export eval^)

(define (eval e) (rec e (hash)))
]}

@filebox[@racket[id-monad@]]{
@racketblock[
(import ev^)
(export unit^ bind^ rec^)

(define (rec e r) (_ev e r))
(define (unit v) v)
(define (bind v f) (f v))
]}

@filebox[@racket[δ@]]{
@racketblock[
(import unit^)
(export δ^)

(define (δ o . vs)
  (_unit
    (match* (o vs)
      [('add1 (list n)) (add1 n)]
      [('sub1 (list n)) (sub1 n)]
      [('+ (list n1 n2)) (+ n1 n2)]
      [('- (list n1 n2)) (- n1 n2)]
      [('* (list n1 n2)) (* n1 n2)])))
]}

@filebox[@racket[env@]]{
@racketblock[
(import unit^)
(export env^)

(define (get r x) (_unit (hash-ref r x)))
(define (alloc f v) (_unit v))
(define (ralloc x v)
  (match v
    [(cons e r)
     (define p (make-placeholder #f))
     (define f (cons e (hash-set r x p)))
     (placeholder-set! p f)
     (_unit (make-reader-graph f))]))
]}}


@subsection{Implementation 2: Circular mutable closures}

@figure["env-box" "Boxing implementation"]{
@filebox[@racket[env-box@]]{
@racketblock[
(import unit^)
(export env^)

(define (get r x)
  (_unit (unbox (hash-ref r x))))
(define (alloc f v) (_unit (box v)))
(define (ralloc x v)
  (match v
    [(cons e r)
     (define b (box #f))
     (define f (cons e (hash-set r x b)))
     (set-box! b f)
     (_unit b)]))
]}}

An alternative implementation strategy for recursive functions is to
box variable bindings and create a cyclic closure via mutation.  This
is accomplished with an alternative implementation of the
@racket[env^] signature, shown in @figure-ref{env-box}.  The
alternative evaluation function is obtained with:
@centered{
@racket[(link eval@ ev@ id-monad@ δ@ env-box@)]
}

@examples[#:eval the-box-eval
                 (rec f (λ (x) f) f)]

@subsection{Implementation 3: Closures in an explicit store}

A third alternative is to model the store directly rather than rely
upon the defining language.  Units implementing this approach are
given in @figure-ref{eval-sto}.  The evalatuator is obtained with:
@centered{
@racket[(link eval-sto@ ev@ sto-monad@ δ@ env-sto@)]
}

@examples[#:eval the-explicit-sto-eval
                 (rec f (λ (x) f) f)]

@figure["eval-sto" "Eval with store"]{
@filebox[@racket[eval-sto@]]{
@racketblock[
(import rec^)
(export eval^)

(define (eval e) ((_rec e (hash)) (hash)))
]}

@filebox[@racket[sto-monad@]]{
@racketblock[
(import ev^)
(export unit^ bind^ rec^)

(define (rec e r) (_ev e r))
(define ((unit v) s) (cons v s))
(define ((bind a f) s)
  (match (a s)
    [(cons v s) ((f v) s)]))
]}

@filebox[@racket[env-sto@]]{
@racketblock[
(import unit^)
(export env^)

(define ((get r x) s)
  ((_unit (hash-ref s (hash-ref r x))) s))

(define ((alloc f v) s)
  (match f
    [(cons (lam x e) r)
     (define a (gensym))
     ((_unit a)
      (update-sto s a v))]))

(define ((ralloc x v) s)
  (match v
    [(cons e r)
     (define a (gensym))
     ((_unit a)
      (update-sto s a
        (cons e (hash-set r x a))))]))
]}
}

@subsection{Imperative higher-order language}

@codeblock{
;; E = ...
;;   | err       ;; Error
;;   | (ref E)   ;; Reference
;;   | (drf E)   ;; Dereference
;;   | (srf E)   ;; Set reference
}

@racketblock[
   (define-signatures
     [err^ : err]
     [sto^ : new sbox ubox])
]

@codeblock|{
(define (ev e r)
  (match e
     ... ; same as in ev@
     ['err (err)]
     [(ref e)
      (do v ← (rec e r)
        (new v))]
     [(drf e)
      (do a ← (rec e r)
        (ubox a))]
     [(srf e0 e1)
      (do a ← (rec e0 r)
          v ← (rec e1 r)
        (sbox a v))]))
}|


@racketblock[
   (link eval-sto@ err-sto-monad@
         δ@ env-sto@ sto@)]


@figure["eval-sto-units" "Units for an explicit store evaluator for imperative language"]{
@filebox[@racket[err-sto-monad@]]{
@racketblock[
(import ev^)
(export unit^ bind^ rec^ err^)

(define (rec e r) (_ev e r))
(define ((err) s) (cons 'err s))
(define ((unit v) s) (cons v s))
(define ((bind a f) s)
  (match (a s)
    [(cons 'err s) (cons 'err s)]
    [(cons v s) ((f v) s)]))
]}

@filebox[@racket[sto@]]{
@racketblock[
(import unit^)
(export sto^)

(define ((new v) s)
  (define a (gensym))
  ((_unit a) (update-sto s a v)))

(define ((sbox a v) s)
  ((_unit a) (update-sto s a v)))

(define ((ubox a) s)
  ((_unit (lookup-sto s a)) s))
]}

}

@section{Non-standard Semantics}

It is common in the abstract interpretation literature to consider an
idealized, non-standard semantics from which to abstract.  Often these
semantics take the form of a collecting semantics that collect either
traces or reachable states.

In this section, we demonstrate that such semantics can be realized as
implementations of the monadic operations for the interpreter given in
the previous section.

@subsection{Trace semantics}

A trace semantics not only computes the result of a computation but
also a string of intermediate states that deliver that result starting
from the initial program.  This is trivial to construct for a
small-step reduction relation, but less obvious for a compositional
evaluation function.  Crucially, by parameterizing our evaluator over
the @racket[rec^] signature, it is possible to observe each call to
the evaluator and therefore we can construct an implementation of
@racket[rec^] that records each call.


@centered{
@racket[
(link eval-trace@ ev!@ δ@ env-sto@ sto@)
]}

@examples[#:eval the-trace-eval
(* 2 2)
]


@figure["trace-eval" "Trace evaluator"]{
@filebox[@racket[eval-trace@]]{
@racketblock[
(import ev^)
(export eval^ unit^ bind^ rec^ err^)

(define (eval e)
  (((rec e (hash)) (hash)) empty))

(define (((rec e r) s) t)
  (((_ev e r) s) (cons (list e r s) t)))

(define (((unit v) s) t)
  (cons (cons v s) t))

(define (((err) s) t)
  (cons (cons 'err s) t))

(define (((bind a f) s) t)
  (match ((a s) t)
    [(cons (cons 'err s) t)
     (cons (cons 'err s) t)]
    [(cons (cons v s) t)
     (((f v) s) t)]))
]
}}


@subsection{Reachable state semantics}

The reachable state semantics is just an abstraction of the trace
semantics that ignores the ordering on states.  To implement such a
semantics, we simply collect a set of evaluator calls rather than a
sequence.

@examples[#:eval the-reachable-eval
(* 2 2)
]

@subsection{Co-inductive semantics}

In this section, we construct an interpreter that detects loops in
evaluation.  If a program terminates, it produces a singleton set; if
the evaluator loops, it produces the empty set.  (We choose a set
intrepretation rather than an option in anticipation of the subsequent
development of a non-deterministic semantics.)


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

@centered{
@url{https://github.com/dvanhorn/monadic-eval}}

@bold{Acknowledgments}: Sam Tobin-Hochstadt, J. Ian Johnson, Olivier Danvy.

@(generate-bibliography)
