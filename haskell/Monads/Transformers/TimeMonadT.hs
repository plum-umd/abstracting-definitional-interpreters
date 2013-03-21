{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Monads.Transformers.TimeMonadT where

import Monads.Classes
import Control.Monad.State
import Control.Monad.Reader

newtype TimeMonadT time m a = TimeMonadT { unTimeMonadT :: StateT time m a}
  deriving ( Monad
           , MonadTrans
           , MonadReader r
           , MonadEnv env
           , MonadStore store
           )

instance (MonadState s m) => MonadState s (TimeMonadT time m) where
  get = lift get
  put = lift . put

instance (Monad m) => MonadTime time (TimeMonadT time m) where
  getTime = TimeMonadT get
  putTime = TimeMonadT . put

instance (Monad n, Promote m n) => Promote m (TimeMonadT time n) where
  promote = lift . promote
