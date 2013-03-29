{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

module Monads.Classes.MonadEnvState where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.List
import Util

class (Monad m) => MonadEnvState env m | m -> env where
  getEnv :: m env
  putEnv :: env -> m ()

modifyEnv :: (MonadEnvState env m) => (env -> env) -> m ()
modifyEnv f = do
  e <- getEnv
  putEnv $ f e

monadGetEnv :: (MonadEnvState env m, MonadTrans t) => t m env
monadGetEnv = lift getEnv

monadPutEnv :: (MonadEnvState env m, MonadTrans t) => env -> t m ()
monadPutEnv = lift . putEnv
