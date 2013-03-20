{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Classes.MonadEnv where

import Data.Map (Map)

type Env addr = Map String addr

class (Monad m) => MonadEnv addr m | m -> addr where
  getEnv :: m (Env addr)
  putEnv :: Env addr -> m ()

