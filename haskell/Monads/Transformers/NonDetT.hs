{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

module Monads.Transformers.NonDetT where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Monads.Classes
import Monads.Transformers.ListSetT
import Util.ListSet
import Util.MFunctor
import Util.MFunctor

newtype NonDetT m a = NonDetT { unNonDetT :: ListSetT m a}
  deriving 
  ( Monad
  , MonadTrans
  , MFunctor
  , MMonad
  , MonadPlus
  , MonadReader r
  , MonadState s
  , MonadEnvReader env
  , MonadEnvState env
  , MonadStoreState store
  , MonadTimeState time
  )

mkNonDetT :: m (ListSet a) -> NonDetT m a
mkNonDetT = NonDetT . ListSetT

runNonDetT :: NonDetT m a -> m (ListSet a)
runNonDetT = runListSetT . unNonDetT

instance (Monad m) => MMorph ListSet (NonDetT m) where
  mMorph = NonDetT . mMorph
