{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Util.ListSet where

import PrettyUtil
import Util.Pointed
import Control.Monad
import Util.Lattice
import Data.Function
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Reader
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Util.Set

newtype ListSet a = ListSet { runListSet :: [a] }
  deriving (Functor, Monad, MonadPlus, Show)

instance (FPretty a, Ord a) => FPretty (ListSet a) where
  fpretty = fpretty . Set.fromList . runListSet

instance (Ord a) => Eq (ListSet a) where
  (==) = (==) `on` Set.fromList . runListSet

instance (Ord a) => Ord (ListSet a) where
  (<=) = Set.isSubsetOf `on` Set.fromList . runListSet

instance (Ord a) => Lattice (ListSet a) where
  lbot = ListSet []
  ltop = error "no representation of top ListSet"
  x `ljoin` y = ListSet $ Set.toList $ (ljoin `on` Set.fromList . runListSet) x y
  x `lmeet` y = ListSet $ Set.toList $ (lmeet `on` Set.fromList . runListSet) x y
  lrefines = (<=)

instance Pointed ListSet where
  unit = return

newtype ListSetT m a = ListSetT { runListSetT :: m (ListSet a) }

mapListSetT :: (Monad m, Monad n) => (m (ListSet a) -> n (ListSet b)) -> ListSetT m a -> ListSetT n b
mapListSetT f = ListSetT . f . runListSetT

instance MonadTrans ListSetT where
  lift = ListSetT . liftM return

instance (Monad m) => Monad (ListSetT m) where
  return a = ListSetT $ return $ return a
  (ListSetT alsM) >>= atobM = ListSetT $ do
    as <- liftM runListSet alsM
    liftM (ListSet . concat) $ forM as $ \ a ->
      liftM runListSet $ runListSetT $ atobM a

instance (Monad m) => MonadPlus (ListSetT m) where
  mzero = ListSetT $ return $ mzero
  mplus aM bM = ListSetT $ do
    as <- runListSetT aM
    bs <- runListSetT bM
    return $ mplus as bs

instance (MonadState s m) => MonadState s (ListSetT m) where
  get = lift get
  put = lift . put

instance (MonadReader r m) => MonadReader r (ListSetT m) where
  ask = lift ask
  local = mapListSetT . local
