{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Monads.Transformers.NonDetT where

import Monads.Classes
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.List
import Util.ListSet

newtype NonDetT m a = NonDetT { unNonDetT :: ListSetT m a}
  deriving 
  ( Monad
  , MonadTrans
  , MonadPlus
  , MonadReader r
  , MonadState s
  , MonadEnvReader env
  , MonadEnvState env
  , MonadStoreState store
  , MonadTimeState time
  )

runNonDetT :: NonDetT m a -> m (ListSet a)
runNonDetT = runListSetT . unNonDetT

instance (Monad m) => Promote ListSet (NonDetT m) where
  promote = NonDetT . ListSetT . return
