module Util.Lattice where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

class Lattice a where
  lbot :: a
  ltop :: a
  ljoin :: a -> a -> a
  lmeet :: a -> a -> a
  lrefines :: a -> a -> Bool

instance Lattice () where
  lbot = ()
  ltop = ()
  _ `ljoin` _ = ()
  _ `lmeet` _ = ()
  _ `lrefines` _ = True

instance (Lattice a, Lattice b) => Lattice (a, b) where
  lbot = (lbot, lbot)
  ltop = (ltop, ltop)
  (a1, b1) `ljoin` (a2, b2) = (a1 `ljoin` a2, b1 `ljoin` b2)
  (a1, b1) `lmeet` (a2, b2) = (a1 `lmeet` a2, b1 `lmeet` b2)
  (a1, b1) `lrefines` (a2, b2) = (a1 `lrefines` a2) && (b1 `lrefines` b2)

instance (Ord a) => Lattice (Set a) where
  lbot = Set.empty
  ltop = error "no representation of top set"
  ljoin = Set.union
  lmeet = Set.intersection
  lrefines = Set.isSubsetOf

instance (Ord k, Lattice v) => Lattice (Map k v) where
  lbot = Map.empty
  ltop = error "no representation of top map"
  ljoin = Map.unionWith ljoin
  lmeet = Map.intersectionWith lmeet
  lrefines = Map.isSubmapOfBy lrefines

ljoins :: (Lattice l) => [l] -> l
ljoins = foldr ljoin lbot
