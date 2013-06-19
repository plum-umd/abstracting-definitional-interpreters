{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, FlexibleInstances #-}

module Analyses.BigStep.WideningT where

import qualified Data.Map as Map
import AAI.Addressable
import Data.PartialOrder
import Data.Lattice
import StateSpace.Semantics
import Util.ListSet
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Monads
import StateSpace
import Util.MFunctor

newtype WideningT addr time var val m a = WideningT
  { unWideningT :: 
      EnvReaderT (Env var addr)
        (TimeStateT time 
          (ListSetT
            (StoreStateT (Store ListSet addr val)
              m)))
        a 
  }
  deriving
  ( Monad
  , MonadPlus
  , MonadState s
  , MonadReader r
  , MonadEnvReader (Env var addr)
  , MonadStoreState (Store ListSet addr val)
  , MonadTimeState time
  , MMorph ListSet
  )

mkWideningT :: 
  (Monad m)
  => (Env var addr 
      -> time
      -> Store ListSet addr val 
      -> m (ListSet (a, time), Store ListSet addr val)
     ) 
  -> WideningT addr time var val m a
mkWideningT f = 
  WideningT $ 
  mkEnvReaderT $ \ env ->
  mkTimeStateT $ \ time ->
  ListSetT $
  mkStoreStateT $ \ store ->
  f env time store

runWideningT :: 
  (Monad m) 
  => WideningT addr time var val m a
  -> Env var addr 
  -> time 
  -> Store ListSet addr val 
  -> m (ListSet (a, time), Store ListSet addr val)
runWideningT aM env time store =
  flip runStoreStateT store
  $ runListSetT
  $ flip runTimeStateT time
  $ flip runEnvReaderT env
  $ unWideningT aM

instance MonadTrans (WideningT addr time var val) where
  lift = WideningT . lift . lift . lift . lift

instance MFunctor (WideningT addr time var val) where
  mFmap f aMT = mkWideningT $ \env store time ->
    f $ runWideningT aMT env store time

instance BigStep (WideningT addr time var val) where
  newtype InBS (WideningT addr time var val) a = WideningInBS
    { unWideningInBS :: (a, Env var addr, time, Store ListSet addr val) }
    deriving (PartialOrder, Lattice, Eq, Ord)
  newtype OutBS (WideningT addr time var val) a = WideningOutBS
    { unWideningOutBS :: (ListSet (a, time), Store ListSet addr val) }
    deriving (PartialOrder, Lattice, Eq, Ord)
  askInBS e = do
    env <- askEnv
    store <- getStore
    time <- getTime
    return $ WideningInBS (e, env, time, store)
  runBS eval (WideningInBS (e, env, time, store)) = 
    liftM WideningOutBS $ runWideningT (eval e) env time store

instance (Addressable addr var time) => Inject (InBS (WideningT addr time var val)) where
  inject a = WideningInBS (a, Map.empty, tzero, Map.empty)
