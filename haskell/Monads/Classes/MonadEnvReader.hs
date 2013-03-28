{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, TupleSections #-}

module Monads.Classes.MonadEnvReader where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.List
import Util.ListSet

class (Monad m) => MonadEnvReader env m | m -> env where
  askEnv :: m env
  localEnv :: (env -> env) -> m a -> m a

-- plumbing

instance (MonadEnvReader env m) => MonadEnvReader env (ListSetT m) where
  askEnv = lift askEnv
  localEnv = mapListSetT . localEnv

instance (MonadEnvReader env m) => MonadEnvReader env (ReaderT r m) where
  askEnv = lift askEnv
  localEnv = mapReaderT . localEnv

instance (MonadEnvReader env m) => MonadEnvReader env (StateT s m) where
  askEnv = lift askEnv
  localEnv = mapStateT . localEnv
