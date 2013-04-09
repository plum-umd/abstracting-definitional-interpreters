{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Monads.Transformers.ReaderT where

import Control.Monad.Reader
import Control.Monad.Trans
import Monads.Classes
import Util.MFunctor

instance MFunctor (ReaderT r) where
  mFmap = mMapM

instance MMonad (ReaderT r) where
  mExtend f aMT =
    ReaderT $ \r ->
    runReaderT (f $ runReaderT aMT r) r

instance (Monad m, MonadEnvReader env m) => MonadEnvReader env (ReaderT r m) where
  askEnv = mAskEnv
  localEnv = mLocalEnv

instance (Monad m, MonadEnvState store m) => MonadEnvState store (ReaderT r m) where
  getEnv = mGetEnv
  putEnv = mPutEnv

instance (Monad m, MonadStoreState store m) => MonadStoreState store (ReaderT r m) where
  getStore = mGetStore
  putStore = mPutStore

instance (Monad m, MonadTimeState time m) => MonadTimeState time (ReaderT r m) where
  getTime = mGetTime
  putTime = mPutTime

instance (Monad m, Monad n, MMorph m n) => MMorph m (ReaderT r n) where
  mMorph = mFmap mMorph . lift
