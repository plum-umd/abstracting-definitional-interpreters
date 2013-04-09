{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Monads.Classes.MonadEnvReader where

import Control.Monad.Trans
import Util.MFunctor

class (Monad m) => MonadEnvReader env m | m -> env where
  askEnv :: m env
  localEnv :: (env -> env) -> m a -> m a

mAskEnv :: (MonadEnvReader env m, MonadTrans t) => t m env
mAskEnv = lift askEnv

mLocalEnv :: (MonadEnvReader env m, MFunctor t) => (env -> env) -> t m a -> t m a
mLocalEnv f = mFmap $ localEnv f
