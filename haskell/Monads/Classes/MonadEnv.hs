{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, TupleSections #-}

module Monads.Classes.MonadEnv where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.List
import Util.ListSet

class (Monad m) => MonadEnv env m | m -> env where
  askEnv :: m env
  localEnv :: (env -> env) -> m a -> m a

-- plumbing

instance (MonadEnv env m) => MonadEnv env (ListSetT m) where
  askEnv = lift askEnv
  localEnv = mapListSetT . localEnv

instance (MonadEnv env m) => MonadEnv env (ReaderT r m) where
  askEnv = lift askEnv
  localEnv = mapReaderT . localEnv

instance (MonadEnv env m) => MonadEnv env (StateT s m) where
  askEnv = lift askEnv
  localEnv = mapStateT . localEnv
