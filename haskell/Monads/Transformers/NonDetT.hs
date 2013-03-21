{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Monads.Transformers.NonDetT where

import Monads.Classes
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.List

newtype NonDetT m a = NonDetT { unNonDetT :: ListT m a}
  deriving ( Monad
           , MonadTrans
           , MonadPlus
           , MonadReader r
           , MonadState s
           , MonadEnv addr
           , MonadStore store
           , MonadTime time
           )

runNonDetT :: NonDetT m a -> m [a]
runNonDetT = runListT . unNonDetT

instance (Monad m) => Promote [] (NonDetT m) where
  promote = NonDetT . ListT . return
