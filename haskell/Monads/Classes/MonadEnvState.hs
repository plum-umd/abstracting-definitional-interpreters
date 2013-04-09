{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Monads.Classes.MonadEnvState where

import Control.Monad.Trans

class (Monad m) => MonadEnvState env m | m -> env where
  getEnv :: m env
  putEnv :: env -> m ()

modifyEnv :: (MonadEnvState env m) => (env -> env) -> m ()
modifyEnv f = do
  e <- getEnv
  putEnv $ f e

localEnvState :: (MonadEnvState env m) => (env -> env) -> m a -> m a
localEnvState f aM = do
  e <- getEnv
  putEnv $ f e
  a <- aM
  putEnv e
  return a

mGetEnv :: (MonadEnvState env m, MonadTrans t) => t m env
mGetEnv = lift getEnv

mPutEnv :: (MonadEnvState env m, MonadTrans t) => env -> t m ()
mPutEnv = lift . putEnv
