{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module Analyses.ZPDCFA where

import Control.Exception
import Analyses.AbstractM
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Monads
import StateSpace
import Util
import qualified Data.Map as Map
import Fixpoints.MemoEval

newtype ZPDCFAT var val m a = ZPDCFAT
  { unZPDCFAT :: AbstractMT (CFAAddr var) ZCFATime var val m a }
  deriving
  ( Monad
  , MonadTrans
  , MonadPlus
  , MonadState s
  , MonadReader r
  , MonadEnv (Env var (CFAAddr var))
  , MonadStore (Store ListSet (CFAAddr var) val)
  , MonadTime ZCFATime
  , Promote ListSet
  )

runZPDCFAT :: (Monad m)
           => ZPDCFAT var val m a
           -> Env var (CFAAddr var)
           -> Store ListSet (CFAAddr var) val
           -> ZCFATime
           -> m (ListSet (a, Store ListSet (CFAAddr var) val, ZCFATime))
runZPDCFAT = runAbstractMT . unZPDCFAT

type MemoTables_ZPDCFA expr var val = 
  MemoTables ListSet val expr (Env var (CFAAddr var)) (Store ListSet (CFAAddr var) val)

type ZPDCFA_Driver var val expr a = ZPDCFAT var val (StateT (MemoTables_ZPDCFA expr var val) Identity) a

driveZPDCFA :: (Ord val, Ord expr, Ord var)
            => ((expr -> ZPDCFA_Driver var val expr val) -> (expr -> ZPDCFA_Driver var val expr val))
            -> (expr -> ListSet (val, Store ListSet (CFAAddr var) val, ZCFATime))
driveZPDCFA eval expr =
  let memoEval' = memoEval (lbot :: ListSet ()) in
  let loop mx =
        let (_VxSxT_list,(m1,mx')) = 
              runIdentity 
              $ flip runStateT (lbot,mx) 
              $ runZPDCFAT (memoEval' eval expr) Map.empty lbot ()
        in assert (mx == mx') $
        if m1 == mx'
          then _VxSxT_list
          else loop m1
  in loop lbot
