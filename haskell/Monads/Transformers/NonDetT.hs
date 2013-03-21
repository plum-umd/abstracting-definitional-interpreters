{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Monads.Transformers.NonDetT where

import Monads.Classes
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.List

newtype NonDetT m a = NonDetT { unNonDetT :: ListT m a}
  deriving ( Monad
           , MonadTrans
           , MonadReader r
           , MonadState s
           , MonadEnv addr
           , MonadStore store
           , MonadTime time
           )

instance (Monad n, Promote m n) => Promote m (NonDetT n) where
  promote = lift . promote
