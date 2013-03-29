{-# LANGUAGE RankNTypes, TypeOperators, MultiParamTypeClasses #-}

module Util.MonadFunctor where

import Control.Monad.State
import Control.Monad.Reader

-- function between monads
type m :-> n = forall a. m a -> n a

class MonadMorph m n where
  mmorph :: m :-> n

-- just use MonadTrans
-- class MonadPointed t where
--   monadUnit :: (Monad m) => m :-> t m

monadGet :: (MonadState s m, MonadTrans t) => t m s
monadGet = lift get

monadPut :: (MonadState s m, MonadTrans t) => s -> t m ()
monadPut = lift . put

monadAsk :: (MonadReader r m, MonadTrans t) => t m r
monadAsk = lift ask

class MonadFunctor t where
  monadFmap :: (Monad m, Monad n) => (m :-> n) -> (t m :-> t n)

monadLocal :: (MonadReader r m, MonadFunctor t) => (r -> r) -> t m a -> t m a
monadLocal f = monadFmap $ local f

class (MonadTrans t) => MonadMonad t where
  monadExtend :: (Monad m, Monad n) => (m :-> t n) -> (t m :-> t n)

monadMapM :: (MonadMonad t) => (Monad m, Monad n) => (m :-> n) -> (t m :-> t n)
monadMapM f = monadExtend $ lift . f
