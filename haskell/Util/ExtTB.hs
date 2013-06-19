{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Util.ExtTB where

import Data.PartialOrder
import Data.Lattice
import Text.MPretty
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad
import Control.Monad.Trans
import Util.Pointed
import Control.Applicative
import Util.MFunctor

data ExtTB a =
    ExtBot
  | Ext a
  | ExtTop
  deriving (Eq, Ord)

instance Pointed ExtTB where
  unit = Ext

instance Functor ExtTB where
  fmap = liftM

instance Applicative ExtTB where
  pure = return
  (<*>) = ap

instance Monad ExtTB where
  return = Ext
  aM >>= aTobM =
    case aM of
      ExtBot -> ExtBot
      Ext a -> aTobM a
      ExtTop -> ExtTop

instance (Eq a) => PartialOrder (ExtTB a) where
  pcompare ExtBot _ = Just LT
  pcompare _ ExtBot = Just GT
  pcompare ExtTop _ = Just GT
  pcompare _ ExtTop = Just LT
  pcompare (Ext x) (Ext y) = if x == y then Just EQ else Nothing

instance Lattice (ExtTB a) where
  lbot = ExtBot
  ltop = ExtTop
  ljoin ExtTop _ = ExtTop
  ljoin _ ExtTop = ExtTop
  ljoin ExtBot a = a
  ljoin a ExtBot = a
  ljoin (Ext _) (Ext _) = ExtTop
  lmeet ExtBot _ = ExtBot
  lmeet _ ExtBot = ExtBot
  lmeet a ExtTop = a
  lmeet ExtTop a = a
  lmeet (Ext _) (Ext _) = ExtBot

instance (IsPretty a) => IsPretty (ExtTB a) where
  pretty ExtBot = literal $ string "bot"
  pretty (Ext a) = pretty a
  pretty ExtTop = literal $ string "top"

newtype ExtTBT m a = ExtTBT { runExtTBT :: m (ExtTB a) }

-- Higher Order Monad
instance MonadTrans ExtTBT where
  lift = ExtTBT . liftM Ext

instance MFunctor ExtTBT where
  mFmap = mMapM

instance MMonad ExtTBT where
  mExtend f aMT = 
    ExtTBT 
    $ liftM join 
    $ runExtTBT 
    $ f 
    $ runExtTBT aMT

instance (Monad m) => MMorph ExtTB (ExtTBT m) where
  mMorph = ExtTBT . return

-- Standard Monad
instance (Monad m) => Monad (ExtTBT m) where
  return = ExtTBT . return . return
  aM >>= aTobM = ExtTBT $ do
    aE <- runExtTBT aM
    case aE of
      ExtBot -> return ExtBot
      Ext a -> runExtTBT $ aTobM a
      ExtTop -> return ExtTop

instance (Monad m) => Functor (ExtTBT m) where
  fmap = liftM

instance (Monad m) => Applicative (ExtTBT m) where
  pure = return
  (<*>) = ap

instance (Monad m) => MonadPlus (ExtTBT m) where
  mzero = ExtTBT $ return ExtBot
  mplus xMT yMT = ExtTBT $ do
    x <- runExtTBT xMT
    y <- runExtTBT yMT
    return $ ljoin x y

instance (MonadReader r m) => MonadReader r (ExtTBT m) where
  ask = lift ask 
  local f = ExtTBT . local f . runExtTBT

instance (MonadState s m) => MonadState s (ExtTBT m) where
  get = ExtTBT $ liftM Ext get
  put = ExtTBT . liftM Ext . put
