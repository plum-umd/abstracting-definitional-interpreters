{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Transformers.NonDetT where

import Classes
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Reader
import Control.Monad.List

newtype NonDetT m a = NonDetT { unNonDetT :: ListT m a}
  deriving (Monad, MonadTrans)

-- mtl style lifting

instance (MonadEnv addr m) => MonadEnv addr (NonDetT m) where
  askEnv = NonDetT $ lift $ askEnv
  localEnv f aM = NonDetT $ ListT $ localEnv f (runListT (unNonDetT aM))

instance (MonadStore dom addr val m) => MonadStore dom addr val (NonDetT m) where
  getStore = NonDetT $ lift $ getStore
  putStore = NonDetT . lift . putStore

instance (MonadTime time m) => MonadTime time (NonDetT m) where
  getTime = NonDetT $ lift $ getTime
  putTime = NonDetT . lift . putTime

instance (Monad n, Promote m n) => Promote m (NonDetT n) where
  promote = NonDetT . lift . promote
