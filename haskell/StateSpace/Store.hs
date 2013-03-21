{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}

module StateSpace.Store where

import Control.Monad.State
import Util.Lens
import Data.Map (Map)
import Util.Lattice
import Util.Pointed
import qualified Data.Map as Map

type Store dom addr val = Map addr (dom val)

joinStore :: (Pointed dom, Lattice (dom val), Ord addr) 
          => addr 
          -> val
          -> Store dom addr val 
          -> Store dom addr val
joinStore a v s =
  ljoin (Map.singleton a (unit v)) s
