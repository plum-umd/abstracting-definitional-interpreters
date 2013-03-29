I'm running this with GHC 7.6.1.  Let me know if you can't get anything to
build, or if you have a package dependency problem.

If you want to play around with examples and are getting stuck somewhere in
typeclass soup, send me an email and I can help.

Right now running will output push-down-zero-cfa on e1 defined in
Lang.Lambda.Examples.  David VH has told me 0CFA will return {0,1} and you can
see this output is more precise returning {0}.  Here is the ZPDCFA output:

{ ( { 0 }                                <--- possible result values for expression
  , { id                                 <--- a result heap
      =>                                    | (different than evaluation heap)
      { < (lam (x) ((lam (q) q) x))         | (the cache in the fixpoint evaluator 
        , {}                                |  is a pair of result heaps)
        >                                   |
      }                                     |
    , q => { 0 , 1 }  <--- result heaps     |
    , x => { 0 , 1 }       map variables    |
    , y => { 0 }           to sets of       |
    , z => { 0 , 1 }       values           |
    }
  , ()                                   <--- abstract time (nothing for 0CFA)
  )
}

This data structure is the return type of runZPDCFA at the bottom of
Lang.Lambda.BigStep.Abstrct.

And the concrete evaluator output:

( 0                              <--- result
, { 0                            <--- heap mapping integers to values
    =>                              |
    < (lam (x) ((lam (q) q) x))     |
    , {}                            |
    >                               |
  , 1 => 0                          |
  , 2 => 0                          |
  , 3 => 0                          |
  , 4 => 1                          |
  , 5 => 1                          |
  , 6 => 1                          |
  }
, 7                              <--- final heap counter (concrete time)
)

This data structure is the return type of runConcrete at the bottom of
Lang.Lambda.BigStep.Abstrct.

If you have color in your terminal the output will also be colored :)

Code will also be very prettily printed if it gets big.

Summary of packages:

* AAI:
  * From Abstracting Abstract Interpreters, the Addressable type class captures
    the idea of abstract adresses and times for small-step operational
    semantics (and big-step too??).
* Analyses:
  * Various Analyses for big-step and small-step monadic interpreters.
  * AnalysisT is for general analyses
  * AbstractT is for generic abstract analyses and builds on AnalysisT
  * ZPDCFAT is push-down-zero-cfa and builds on AbstractT
  * ConcreteT is for concrete evaluation and builds on AnalysisT
* Fixpoints:
  * Fixpoint strategies for big-step monadic interpreters
  * MemoEval uses a cache to soundly approximate recursive calls to the
    evaluator while achieving termination
  * YEval is just a (lazy because it's Haskell) YCombinator for concrete evaluators
* Lang:
  * Languages with monadic-interpreters
  * Abstract interpeters can be instantiated with ConcreteT from Analyses to
    recover the Concrete interpreter
* Monads:
  * General-purpose monads for both big-step and small-step monadic
    interpreters
* SExpKit:
  * A general purpose s-expression library that support quasi-quoting
    (see Lang.Lambda.Data to see it in use)
* StateSpace:
  * Common instantiations for the abstract machine state-space
  * CFA defines abstract addresses and abstract time
  * KCFA should go here, but it doesn't exist yet
  * Concrete defines concrete address and time (integers)
  * Env and Store are the environment and store structures that _all_ abstract
    interpreters use.  Rather than making EnvLike and StoreLike classes, the
    Store structures is fixed but parameterized by a domain which must be a
    lattice.  When instantiating abstract evaluators to recover concrete
    semantics, dom should be ExtTB (see Util)
* Util:
  * Uncategorized utilities
  * ExtTB extends a type with a top and bottom element.  This allows concrete
    interpreters to instantiate dom to a domain that is a proper lattice.  If
    two values must be joined during concrete evaluation Top is returned, but
    this should never happen.
  * Lattice is a general purpose Lattice class
  * Lens is a utility for functional record-like projections and updates
    (not used at the moment, but comes in handy when a state monad transformers
    needs to have multiple things inside.  it can't be used at the moment
    because the order of the state monads in the transformer stack matters, so
    we can't assume all state lives at a single position on the stack)
  * ListSet is an attempt at a set-like structure that doesn't have the Ord
    constraint on monadic bind.  Comparison and printing are defined as a set,
    but bind is just concatMap like lists. (I'm only 90% it's sound)
  * Map defines pretty printing for maps
  * MonadFunctor defines higher-order monads (monads in the category of monads,
    see blog post "mmorph-1.0.0: Monad morphisms").  MonadFunctor is used
    heavily in the "plumbing" for new monads".
  * Pointed is a class with one function, `unit', which describes a pointed functor
  * Set defines pretty printing for sets (ListSet uses this)
* PrettyUtil:
  * The PrettyPrint library ansi-wl-pprint has a broken pretty-printer for
    tuple, and rather than patch or hack the library I define my own FPretty
    with correct tuple printing.  The rest of this package uses FPretty for
    pretty printing.
