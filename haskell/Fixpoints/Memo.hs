{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Fixpoints.Memo where

import Data.PartialOrder
import Control.Arrow
import Control.Monad.State
import Data.Map (Map)
import Data.Maybe
import Monads
import Data.Lattice
import Util.MFunctor
import qualified Data.Map as Map
import StateSpace.Semantics

type Memo expr val t = (Map (InBS t expr) val, Map (InBS t expr) val)

fixMemo ::
     ( BigStep t
     , Monad m
     , MonadState (Memo a b t) (t m)
     , Ord (InBS t a)
     , Lattice b
     )
  => ((a -> t m b) -> a -> t m b)
  -> a 
  -> t m b
fixMemo eval expr = do
  inSS <- askInBS expr
  (m1,mx) <- get
  case Map.lookup inSS m1 of
    -- We have seen this state before. Use the stored approximate value.
    Just outSS -> return outSS
    -- We haven't seen the state before.
    Nothing -> do
      -- Use an old approximation if we see this state again.
      modify -- update the memo state
        $ first -- update the first component (m1)
        $ ljoin -- perform a lattice join on the existing value
        $ Map.singleton inSS -- join with a singleton mapping inSS
        $ fromMaybe lbot -- to either bottom or
        $ Map.lookup inSS mx -- mx[inSS]
      --
      -- TODO: shouldn't this perform an update?
      -- is it a join or a replace?
      eval (fixMemo eval) expr

runMemo ::
     ( BigStep t
     , Inject (InBS t)
     , MonadState (Memo a b t) (t (State (Memo a b t)))
     , Ord (InBS t a)
     , Eq b
     , Lattice b
     )
  => ((a -> t (State (Memo a b t)) b) -> a -> t (State (Memo a b t)) b)
  -> a
  -> OutBS t b
runMemo eval e =
  let loop mx =
        let (outSS,(m1,mx')) =
              flip runState (lbot,mx) -- run with empty m1 and running mx
              $ runBS (fixMemo eval) (inject e) -- run memo semantics
        in -- assert (mx == mx') $
           if m1 == mx' 
             -- if no refinement, we have reached a fixpoint
             then outSS
             -- otherwise, loop with the refined cache
             else loop m1
  in loop lbot
