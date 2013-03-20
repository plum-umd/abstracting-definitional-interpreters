module Classes.Lattice where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

class Lattice a where
 bot :: a
 top :: a
 join :: a -> a -> a
 meet :: a -> a -> a
 refines :: a -> a -> Bool

instance Lattice () where
 bot = ()
 top = ()
 _ `join` _ = ()
 _ `meet` _ = ()
 _ `refines` _ = True

instance (Lattice a, Lattice b) => Lattice (a, b) where
 bot = (bot, bot)
 top = (top, top)
 (a1, b1) `join` (a2, b2) = (a1 `join` a2, b1 `join` b2)
 (a1, b1) `meet` (a2, b2) = (a1 `meet` a2, b1 `meet` b2)
 (a1, b1) `refines` (a2, b2) = (a1 `refines` a2) && (b1 `refines` b2)

instance (Ord s) => Lattice (Set s) where
 bot = Set.empty
 top = error "no representation of universal set"
 join = Set.union
 meet = Set.intersection
 refines = Set.isSubsetOf

instance (Ord k, Lattice v) => Lattice (Map k v) where
 bot = Map.empty
 top = error "no representation of top map"
 join = Map.unionWith join
 meet = Map.intersectionWith meet
 refines = Map.isSubmapOfBy refines

joins :: (Lattice l) => [l] -> l
joins = foldr join bot
