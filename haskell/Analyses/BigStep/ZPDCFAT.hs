{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes #-}

module Analyses.BigStep.ZPDCFAT where

import Analyses.Executable
import Analyses.BigStep.AbstractT
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
  , MonadEnvState env
  , MonadStoreState (Store ListSet (CFAAddr var) val)
  , MonadTimeState ZCFATime
  , MMorph ListSet
  )

mkZPDCFAT :: 
  (Monad m)
  => (Env var (CFAAddr var)
      -> Store ListSet (CFAAddr var) val
      -> ZCFATime
      -> m (ListSet (a, Store ListSet (CFAAddr var) val, ZCFATime))
     )
  -> ZPDCFAT var val m a
mkZPDCFAT = ZPDCFAT . mkAbstractT

runZPDCFAT :: 
  (Monad m)
  => ZPDCFAT var val m a
  -> Env var (CFAAddr var)
  -> Store ListSet (CFAAddr var) val
  -> ZCFATime
  -> m (ListSet (a, Store ListSet (CFAAddr var) val, ZCFATime))
runZPDCFAT = runAbstractT . unZPDCFAT

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
  -> ListSet (ListSet val, Store ListSet (CFAAddr var) val, ZCFATime)
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

class MonadReaderT env t where
  askT :: forall m. (Monad m) => t m env
  
drive ::
  (forall m. MonadReader env (t m), Ord val, Ord expr, Executable t expr env store result)
  => ((forall m. (Monad m) => expr -> t m (dom val))
      -> forall m. (Monad m) => expr -> t m (dom val))
  -> expr
  -> result (dom val)
drive eval e =
  let loop mx =
        let (r,(m1,mx')) =
              flip runState (lbot,mx)
              $ exec Map.empty Map.empty ()
              $ memoEval eval e
        in -- assert (mx == mx') $
           if m1 == mx'
             then r
             else loop m1
  in loop lbot
