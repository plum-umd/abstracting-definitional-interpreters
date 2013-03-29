{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, TupleSections #-}

module Monads.Classes.MonadEnvReader where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.List
import Util

class (Monad m) => MonadEnvReader env m | m -> env where
  askEnv :: m env
  localEnv :: (env -> env) -> m a -> m a

monadAskEnv :: (MonadEnvReader env m, MonadTrans t) => t m env
monadAskEnv = lift askEnv

monadLocalEnv :: (MonadEnvReader env m, MonadFunctor t) => (env -> env) -> t m a -> t m a
monadLocalEnv f = monadFmap $ localEnv f
