module StateSpace.Store where

import Data.Map (Map)
import Util.Lattice
import Util.Pointed
import qualified Data.Map as Map

type Store dom addr val = Map addr (dom val)

updateStoreD :: (Pointed dom, Lattice (dom val), Ord addr)
             => addr
             -> dom val
             -> Store dom addr val
             -> Store dom addr val
updateStoreD a vD = Map.unionWith ljoin (Map.singleton a vD)

updateStore :: (Pointed dom, Lattice (dom val), Ord addr) 
            => addr 
            -> val
            -> Store dom addr val 
            -> Store dom addr val
updateStore a v = updateStoreD a (unit v)

