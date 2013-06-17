{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Analyses.SmallStep.ZPDCFAT where

import Data.PartialOrder
import Analyses.SmallStep.AbstractT
import Control.Exception
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Fixpoints.MemoEval
import Monads
import StateSpace
import Data.Lattice
import Util.ListSet
import Util.MFunctor
import qualified Data.Map as Map

newtype ZPDCFAT var val m a = ZPDCFAT
  { unZPDCFAT :: AbstractT (CFAAddr var) ZCFATime var val m a }
  deriving
  ( Monad
  , MonadTrans
  , MFunctor
  , MonadPlus
  , MonadState s
  , MonadReader r
  , MonadEnvReader (Env var (CFAAddr var))
  , MonadEnvState (Env var (CFAAddr var))
  , MonadStoreState (Store ListSet (CFAAddr var) val)
  , MonadTimeState ZCFATime
  , MMorph ListSet
  )

mkZPDCFAT :: 
  (Monad m)
  => (Env var (CFAAddr var)
      -> Store ListSet (CFAAddr var) val
      -> ZCFATime
      -> m (ListSet (a, Env var (CFAAddr var), Store ListSet (CFAAddr var) val, ZCFATime))
     )
  -> ZPDCFAT var val m a
mkZPDCFAT = ZPDCFAT . mkAbstractT

runZPDCFAT :: 
  (Monad m)
  => ZPDCFAT var val m a
  -> Env var (CFAAddr var)
  -> Store ListSet (CFAAddr var) val
  -> ZCFATime
  -> m (ListSet (a, Env var (CFAAddr var), Store ListSet (CFAAddr var) val, ZCFATime))
runZPDCFAT = runAbstractT . unZPDCFAT

type ZPDCFA var val a = ZPDCFAT var val Identity a

-- Big Step

type MemoTablesZPDCFA expr var val = 
  MemoTables ListSet val expr (Env var (CFAAddr var)) (Store ListSet (CFAAddr var) val)

type ZPDCFADriver var val expr a = ZPDCFAT var val (StateT (MemoTablesZPDCFA expr var val) Identity) a

driveZPDCFA :: 
  (Ord val, Ord expr, Ord var)
  => ((expr -> ZPDCFADriver var val expr (ListSet val))
      -> expr 
      -> ZPDCFADriver var val expr (ListSet val)
     )
  -> expr 
  -> ListSet (ListSet val, Env var (CFAAddr var), Store ListSet (CFAAddr var) val, ZCFATime)
driveZPDCFA eval expr =
  let loop mx =
        let (_VxSxT_list,(m1,mx')) = 
              runIdentity 
              $ flip runStateT (lbot,mx) 
              $ runZPDCFAT (memoEval eval expr) Map.empty Map.empty ()
        in assert (mx == mx') $
        if m1 == mx'
          then _VxSxT_list
          else loop m1
  in loop lbot

-- Small Step

iterZPDCFA ::
  (Ord expr, Ord var, Ord val)
  => (expr -> ZPDCFA var val expr)
  -> expr
  -> ListSet (expr, Env var (CFAAddr var), Store ListSet (CFAAddr var) val, ZCFATime)
iterZPDCFA step e =
  let loop ss =
        let ss' = do
              (e,env,store,time) <- ss'
              runIdentity $ runZPDCFAT (step e) env store time
        in if lte ss' ss
          then ss
          else loop ss'
  in loop $ runIdentity $ runZPDCFAT (return e) Map.empty Map.empty ()
