#lang scribble/acmart @acmlarge
@(require scriblib/figure 
          scribble/manual 
          scriblib/footnote
          "bib.rkt")

@title[#:tag "s:interp"]{A Definitional Interpreter}

@;{
\begin{wrapfigure}{R}{0.5\textwidth} %{{{ f:syntax
  \begin{mdframed}
    \begin{alignat*}{4}
      e ∈ &&\mathrel{}   exp ⩴ &\mathrel{} 𝔥⸨(vbl⸩\ x𝔥⸨)⸩         &\hspace{3em} [⦑\emph{variable}⦒]
      \\[\mathgobble]     &&\mathrel{}       ∣ &\mathrel{} 𝔥⸨(num⸩\ n𝔥⸨)⸩         &\hspace{3em} [⦑\emph{number}⦒]
      \\[\mathgobble]     &&\mathrel{}       ∣ &\mathrel{} 𝔥⸨(if0⸩\ e\ e\ e𝔥⸨)⸩   &\hspace{3em} [⦑\emph{conditional}⦒]
      \\[\mathgobble]     &&\mathrel{}       ∣ &\mathrel{} 𝔥⸨(op2⸩\ b\ e\ e𝔥⸨)⸩   &\hspace{3em} [⦑\emph{binary op}⦒]
      \\[\mathgobble]     &&\mathrel{}       ∣ &\mathrel{} 𝔥⸨(app⸩\ e\ e𝔥⸨)⸩      &\hspace{3em} [⦑\emph{application}⦒]
      \\[\mathgobble]     &&\mathrel{}       ∣ &\mathrel{} 𝔥⸨(rec⸩\ x\ ℓ\ e𝔥⸨)⸩   &\hspace{3em} [⦑\emph{letrec}⦒]
      \\[\mathgobble]     &&\mathrel{}       ∣ &\mathrel{} ℓ                     &\hspace{3em} [⦑\emph{lambda}⦒]
      \\[\mathgobble]ℓ ∈ &&\mathrel{}   lam ⩴ &\mathrel{} 𝔥⸨(lam⸩\ x\ e𝔥⸨)⸩      &\hspace{3em} [⦑\emph{function defn}⦒]
      \\[\mathgobble] x ∈ &&\mathrel{}   var ≔ &\mathrel{} ❴𝔥⸨x⸩, 𝔥⸨y⸩, …❵        &\hspace{3em} [⦑\emph{variable names}⦒]
      %\\[\mathgobble] u ∈ &&\mathrel{}  unop ≔ &\mathrel{} ❴𝔥⸨add1⸩, …❵           &\hspace{3em} [⦑\emph{unary prim}⦒]
      \\[\mathgobble] b ∈ &&\mathrel{} binop ≔ &\mathrel{} ❴𝔥⸨+⸩, 𝔥⸨-⸩, …❵        &\hspace{3em} [⦑\emph{binary prim}⦒]
    \end{alignat*}
    \captionskip{Programming Language Syntax}
    \label{f:syntax}
  \end{mdframed}
\end{wrapfigure} %}}}
}

@figure["f:syntax" "Programming Language Syntax" "FIXME"]

We begin by constructing a definitional interpreter for a small but
representative higher-order, functional language.  The abstract syntax
of the language is defined in @Figure-ref{f:syntax}; it includes
variables, numbers, binary operations on numbers, conditionals,
@racket[letrec] expressions, functions and applications.

The interpreter for the language is defined in
@Figure-ref{f:interpreter}. At first glance, it has many conventional
aspects: it is compositionally defined by structural recursion on the
syntax of expressions; it represents function values as a closure data
structure which pairs the lambda term with the evaluation environment;
it is structured monadically and uses monad operations to interact
with the environment and store; and it relies on a helper function
@racket[δ] to interpret primitive operations.

There are a few superficial aspects that deserve a quick note:
environments @racket[ρ] are finite maps and the syntax @racket[(ρ x)]
denotes @math{ρ(x)} while @racket[(ρ x a)] denotes @math{ρ[x↦a]}.  For
simplicity, recursive function definitions (@racket[rec]) are assumed
to be syntactic values.  The @racket[do]-notation is just shorthand for
@racket[bind], as usual:
@racketblock[
  (do x ← e . r) ≡ (bind e (λ (x) (do . r)))
       (do e . r) ≡ (bind e (λ (_) (do . r)))
 (do x := e . r) ≡ (let ((x e)) (do . r))
          (do b) ≡ b
]
Finally, there are two unconventional aspects worth noting.

First, the interpreter is written in an @emph{open recursive style};
the evaluator does not call itself recursively, instead it takes as an
argument a function @racket[ev]—shadowing the name of the function
@racket[ev] being defined—and @racket[ev] (the argument) is called
instead of self-recursion.  This is a standard encoding for recursive
functions in a setting without recursive binding.  It is up to an
external function, such as the Y-combinator, to close the recursive
loop.  This open recursive form is crucial because it allows
intercepting recursive calls to perform “deep” instrumentation of the
interpreter.

Second, the code is clearly @emph{incomplete}.  There are a number of free
variables, typeset as italics, which implement the following:

@itemlist[
@item{The underlying monad of the interpreter: @racket[return] and @racket[bind];}
@item{An interpretation of primitives: @racket[δ] and @racket[zero?];}
@item{Environment operations: @racket[ask-env] for retrieving the
environment and @racket[local-env] for installing an environment;}
@item{Store operations: @racket[ext] for updating the store, and @racket[find] for
dereferencing locations; and}
@item{An operation for @racket[alloc]ating new store locations.}
]

Going forward, we make frequent use of definitions involving free
variables, and we call such a collection of such definitions a
@emph{component}. We assume components can be named (in this case,
we've named the component @racket[ev@], indicated by the box in the
upper-right corner) and linked together to eliminate free
variables.@note{We use Racket @emph{units} @~cite{local:flatt-pldi98}
to model components in our implementation.}

@figure["f:interpreter" "The Extensible Definitional Interpreter"]{
@filebox[@racket[ev@]]{
@racketblock[
(define ((ev ev) e)
  (match e
    [(num n)
     (return n)]
    [(vbl x)
     (do ρ ← ask-env
         (find (ρ x)))]    
    [(if0 e₀ e₁ e₂)
     (do v  ← (ev e₀)  z? ← (zero? v)
         (ev (if z? e₁ e₂)))]
    [(op2 o e₀ e₁)
     (do v₀ ← (ev e₀)  v₁ ← (ev e₁)
         (δ o v₀ v₁))]
    [(rec f l e)
     (do ρ  ← ask-env  a  ← (alloc f)
         ρ′ ≔ (ρ f a)
         (ext a (cons l ρ′))
         (local-env ρ′ (ev e)))]
    [(lam x e₀)
     (do ρ ← ask-env
         (return (cons (lam x e₀) ρ)))]
    [(app e₀ e₁)
     (do (cons (lam x e₂) ρ) ← (ev e₀)
          v₁ ← (ev e₁)
          a  ← (alloc x)
          (ext a v₁)
          (local-env (ρ x a) (ev e₂)))]))
]}}
