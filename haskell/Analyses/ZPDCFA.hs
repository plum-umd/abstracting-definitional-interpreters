{-# LANGUAGE GeneralizedNewtypeDeriving, Rank2Types, ScopedTypeVariables #-}

module Analyses.ZPDCFA where

import Util
import Monads
import StateSpace
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans
import qualified Fixpoints.MemoEval as MemoEval
import qualified Data.Map as Map

newtype AbstractM addr time var val a = AbstractM
  { unAbstractM :: EnvMonadT (Env var addr)
                   (StoreMonadT (Store [] addr val)
                   (TimeMonadT time 
                   (NonDetT 
                   Identity))) a }
  deriving
  ( Monad
  , MonadPlus
  , MonadEnv (Env var addr)
  , MonadStore (Store [] addr val)
  , MonadTime time
  , Promote []
  )

runAbstractM :: AbstractM addr time var val a
             -> Env var addr 
             -> Store [] addr val 
             -> time 
             -> [(a, Store [] addr val, time)]
runAbstractM aM env store time =
  let rs = 
        runIdentity 
        $ runNonDetT 
        $ flip runTimeMonadT time
        $ flip runStoreMonadT store
        $ flip runEnvMonadT env
        $ unAbstractM aM
  in flip map rs $ \ ((a,store),time) -> (a,store,time)

newtype ZPDCFA var val a = ZPDCFA
  { unZPDCFA :: AbstractM (CFAAddr var) ZCFATime var val a }
  deriving
  ( Monad
  , MonadPlus
  , MonadEnv (Env var (CFAAddr var))
  , MonadStore (Store [] (CFAAddr var) val)
  , MonadTime ZCFATime
  , Promote []
  )

runZPDCFA :: ZPDCFA var val a
          -> Env var (CFAAddr var)
          -> Store [] (CFAAddr var) val
          -> ZCFATime
          -> [(a, Store [] (CFAAddr var) val, ZCFATime)]
runZPDCFA = runAbstractM . unZPDCFA

type MemoTables_ZPDCFA expr var val = 
  MemoEval.MemoTables [] val expr (Env var (CFAAddr var)) (Store [] (CFAAddr var) val)

zpdcfa :: forall var val expr.
          ( Ord expr
          , Ord val
          , Ord var)
       => ((expr -> StateT (MemoTables_ZPDCFA expr var val) (ZPDCFA var val) val) 
           -> (expr -> StateT (MemoTables_ZPDCFA expr var val) (ZPDCFA var val) val))
      -> (expr -> ZPDCFA var val val)
zpdcfa eval = MemoEval.drive eval

