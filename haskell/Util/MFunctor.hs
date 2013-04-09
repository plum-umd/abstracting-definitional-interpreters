{-# LANGUAGE RankNTypes, TypeOperators, MultiParamTypeClasses #-}

module Util.MFunctor where

import Control.Monad.Trans

-- function between monads
type m :-> n = forall a. m a -> n a

class MMorph m n where
  mMorph :: m :-> n

class MFunctor t where
  mFmap :: (Monad m, Monad n) => (m :-> n) -> (t m :-> t n)

class (MonadTrans t) => MMonad t where
  mExtend :: (Monad m, Monad n) => (m :-> t n) -> (t m :-> t n)

mMapM :: (MMonad t) => (Monad m, Monad n) => (m :-> n) -> (t m :-> t n)
mMapM f = mExtend $ lift . f
