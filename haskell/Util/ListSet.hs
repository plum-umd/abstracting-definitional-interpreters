{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Util.ListSet where

import Data.PartialOrder
import Text.MPretty
import Data.Function
import qualified Data.Set as Set
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import Data.Lattice
import Util.Pointed
import Util.MFunctor
import Monads.Classes

newtype ListSet a = ListSet { runListSet :: [a] }
  deriving (Functor, Applicative, Monad, MonadPlus, Eq, Ord, Show)

liftListOp :: ([a] -> [b]) -> ListSet a -> ListSet b
liftListOp f (ListSet l) = ListSet $ f l

instance (IsPretty a, Ord a) => IsPretty (ListSet a) where
  pretty = pretty . Set.fromList . runListSet

instance (Ord a) => PartialOrder (ListSet a) where
  lte = lte `on` Set.fromList . runListSet

instance (Ord a) => Lattice (ListSet a) where
  lbot = ListSet []
  ltop = error "no representation of top ListSet"
  ljoin x y = ListSet $ Set.toList $ (ljoin `on` Set.fromList . runListSet) x y
  lmeet x y = ListSet $ Set.toList $ (lmeet `on` Set.fromList . runListSet) x y

instance Pointed ListSet where
  unit = return

newtype ListSetT m a = ListSetT { runListSetT :: m (ListSet a) }

-- Higher Order Monad
instance MonadTrans ListSetT where
  lift = ListSetT . liftM return

instance MFunctor ListSetT where
  mFmap = mMapM

instance MMonad ListSetT where
  mExtend f aMT =
    ListSetT
    $ liftM join
    $ runListSetT
    $ f
    $ runListSetT aMT

instance (Monad m) => MMorph ListSet (ListSetT m) where
  mMorph = ListSetT . return

-- Standard Monad
instance (Monad m) => Monad (ListSetT m) where
  return a = ListSetT $ return $ return a
  (ListSetT alsM) >>= atobM = ListSetT $ do
    as <- liftM runListSet alsM
    liftM (ListSet . concat) $ forM as $ \ a ->
      liftM runListSet $ runListSetT $ atobM a

instance (Monad m) => Applicative (ListSetT m) where
  pure = return
  (<*>) = ap

instance (Monad m) => Functor (ListSetT m) where
  fmap = liftM

instance (Monad m) => MonadPlus (ListSetT m) where
  mzero = ListSetT $ return $ mzero
  mplus aM bM = ListSetT $ do
    as <- runListSetT aM
    bs <- runListSetT bM
    return $ mplus as bs

instance (MonadState s m) => MonadState s (ListSetT m) where
  get = mGet
  put = mPut

instance (MonadReader r m) => MonadReader r (ListSetT m) where
  ask = mAsk
  local = mLocal
