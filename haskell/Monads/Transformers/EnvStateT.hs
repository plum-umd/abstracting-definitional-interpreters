{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TypeFamilies, TypeOperators, InstanceSigs, ScopedTypeVariables #-}

module Monads.Transformers.EnvStateT where

import Data.Tuple
import StateSpace.Semantics
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Monads.Classes
import Monads.Transformers.StateT
import Util.MFunctor

newtype EnvStateT env m a = EnvStateT { unEnvStateT :: StateT env m a}
  deriving 
  ( Monad
  , MonadTrans
  , MFunctor
  , MonadPlus
  , MonadReader r
  , MonadStoreState store
  , MonadTimeState time 
  , MMorph n
  )

mkEnvStateT :: (env -> m (a,env)) -> EnvStateT env m a
mkEnvStateT = EnvStateT . StateT

runEnvStateT :: EnvStateT env m a -> env -> m (a,env)
runEnvStateT = runStateT . unEnvStateT

instance (MonadState s m) => MonadState s (EnvStateT env m) where
  get = mGet
  put = mPut

instance (Monad m) => MonadEnvReader env (EnvStateT env m) where
  askEnv = getEnv
  localEnv = localEnvState

instance (Monad m) => MonadEnvState env (EnvStateT env m) where
  getEnv = EnvStateT get
  putEnv = EnvStateT . put

instance (Monad m, SmallStep m) => SmallStep (EnvStateT env m) where
  type SS (EnvStateT env m) = SS m :.: (,) env
  runSS k ss = Compose $ runSS (\(env,a) -> liftM swap $ runEnvStateT (k a) env) $ unCompose ss 
