{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}

module Lang.Lambda.BigStep.Abstract where

import AAI
import Analyses.BigStep
import Control.Applicative
import Control.Monad hiding (sequence, mapM)
import Data.Maybe
import Data.Traversable
import Lang.Lambda.Data
import Lang.Lambda.Printing
import Monads
import Prelude hiding (sequence, mapM)
import PrettyUtil
import SExpKit
import StateSpace
import Util
import qualified Data.Map as Map
import qualified Text.PrettyPrint.ANSI.Leijen as PP

data Val addr =
    Num Integer
  | Nat
  | Clo String Expr (Env String addr)
  deriving (Eq, Ord)

instance (FPretty addr) => FPretty (Val addr) where
  fpretty (Num i) = litColor $ fpretty i
  fpretty Nat = PP.magenta $ PP.text "Nat"
  fpretty (Clo x e env) = prettyAngle
    [ fpretty $ LamE x e
    , fpretty env
    ]

instance (FPretty addr) => Show (Val addr) where
  show = showFromPretty

type AbstractMonad dom time addr m =
  ( MonadPlus m
  , MonadEnvReader (Env String addr) m
  , MonadStoreState (Store dom addr (Val addr)) m
  , MonadTimeState time m
  , Addressable addr String time
  , Pointed dom
  , Applicative dom
  , Functor dom
  , MMorph dom m
  , Lattice (dom (Val addr))
  , Ord addr
  ) 

delta :: Op -> [Val addr] -> Val addr
delta Add1 [Num _] = Nat
delta Add1 [Nat] = Nat
delta Sub1 [Num _] = Nat
delta Sub1 [Nat] = Nat

eval :: (AbstractMonad dom time addr m) 
     => (Expr -> m (dom (Val addr)))
     -> Expr 
     -> m (dom (Val addr))
eval eval (LitE n) = return $ unit $ Num n
eval eval (VarE x) = do
  env <- askEnv
  store <- getStore
  return $ fromJust $ Map.lookup (env Map.! x) store
eval eval (LamE x e) = do
  env <- askEnv
  return $ unit $ Clo x e env
eval eval (LetE x a e) = do
  i <- alloc x
  vD <- eval a
  modifyStore (updateStoreD i vD)
  localEnv (Map.insert x i) $
    eval e
eval eval (LetRecE f x b e) = do
  i <- alloc f
  localEnv (Map.insert f i) $ do
    env <- askEnv
    let v = Clo x b env
    modifyStore (updateStore i v)
    eval e
eval eval (IfZE c tb fb) = do
  cv <- mMorph =<< eval c
  case cv of
    Num 0 -> eval tb
    Num _ -> eval fb
    Nat -> eval tb `mplus` eval fb
    _ -> error "ill-formed if0 expression"
eval eval (AppE e1 e2) = do
  Clo x e env' <- mMorph =<< eval e1
  vD <- eval e2
  i <- alloc x
  modifyStore (updateStoreD i vD)
  localEnv (const $ Map.insert x i env') $
    eval e
eval eval (PrimE op es) = do
  vDs <- liftM sequenceA $ mapM eval es
  return $ fmap (delta op) vDs

type ZPDCFAAddr = CFAAddr String
type ZPDCFAVal = Val ZPDCFAAddr

runZPDCFA :: Expr -> ListSet (ListSet ZPDCFAVal, Store ListSet ZPDCFAAddr ZPDCFAVal, ZCFATime)
runZPDCFA = driveZPDCFA eval

type ConcreteVal = Val Integer

runConcrete :: Expr -> ExtTB (ExtTB ConcreteVal, Store ExtTB Integer ConcreteVal, Integer)
runConcrete = driveConcrete eval
