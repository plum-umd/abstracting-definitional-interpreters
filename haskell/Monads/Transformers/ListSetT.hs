{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TypeFamilies #-}

module Monads.Transformers.ListSetT where

import Control.Monad
import StateSpace.Semantics
import Monads.Classes
import Util.ListSet

instance (MonadEnvReader env m) => MonadEnvReader env (ListSetT m) where
  askEnv = mAskEnv
  localEnv = mLocalEnv

instance (MonadEnvState env m) => MonadEnvState env (ListSetT m) where
  getEnv = mGetEnv
  putEnv = mPutEnv

instance (MonadStoreState store m) => MonadStoreState store (ListSetT m) where
  getStore = mGetStore
  putStore = mPutStore

instance (MonadTimeState time m) => MonadTimeState time (ListSetT m) where
  getTime = mGetTime
  putTime = mPutTime

instance (Monad m, SmallStep m) => SmallStep (ListSetT m) where
  -- turn this back into newtype, get rid of :.:
  type SS (ListSetT m) = SS m :.: ListSet ListSetSS { unListSetSS :: SS m (ListSet a) }
  runSS k (ListSetSS ss) = ListSetSS $ runSS (\alist -> runListSetT $ (ListSetT $ return alist) >>= k) ss
