{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

module Monads.Classes.MonadEnvState where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.List
import Util.ListSet

class (Monad m) => MonadEnvState env m | m -> env where
  getEnv :: m env
  putEnv :: env -> m ()

modifyEnv :: (MonadEnvState env m) => (env -> env) -> m ()
modifyEnv f = do
  e <- getEnv
  putEnv $ f e

-- plumbing

instance (MonadEnvState env m) => MonadEnvState env (ListSetT m) where
  getEnv = lift getEnv
  putEnv = lift . putEnv

instance (MonadEnvState env m) => MonadEnvState env (ReaderT r m) where
  getEnv = lift getEnv
  putEnv = lift . putEnv

instance (MonadEnvState env m) => MonadEnvState env (StateT s m) where
  getEnv = lift getEnv
  putEnv = lift . putEnv

