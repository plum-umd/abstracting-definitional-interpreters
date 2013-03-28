{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}

module Lang.Lambda.BigStep.Abstract where

import PrettyUtil
import Data.Maybe
import Control.Monad
import StateSpace
import Lang.Lambda.Data
import Monads
import AAI
import Util
import qualified Data.Map as Map
import Analyses
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import SExpKit

data Val addr =
    Num Integer
  | Nat
  | Clo String Expr (Env String addr)
  deriving (Eq, Ord, Show)

instance (FPretty addr) => FPretty (Val addr) where
  fpretty (Num i) = litColor $ fpretty i
  fpretty Nat = PP.magenta $ PP.text "Nat"
  fpretty (Clo x e env) = prettyAngle $
    [ fpretty $ LamE x e
    , fpretty env
    ]

type AbstractMonad dom time addr m =
  ( MonadPlus m
  , MonadEnvReader (Env String addr) m
  , MonadStoreState (Store dom addr (Val addr)) m
  , MonadTimeState time m
  , Addressable addr String time
  , Pointed dom
  , Functor dom
  , Promote dom m
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
  localEnv (Map.insert x i) $ do
    eval e
eval eval (LetRecE f x b e) = do
  i <- alloc f
  localEnv (Map.insert f i) $ do
    env <- askEnv
    let v = Clo x b env
    modifyStore (updateStore i v)
    eval e
eval eval (IfZE c tb fb) = do
  cv <- promote =<< eval c
  case cv of
    Num 0 -> eval tb
    Num _ -> eval fb
    Nat -> eval tb `mplus` eval fb
    _ -> error "ill-formed if0 expression"
eval eval (AppE e1 e2) = do
  Clo x e env' <- promote =<< eval e1
  vD <- eval e2
  i <- alloc x
  modifyStore (updateStoreD i vD)
  localEnv (const $ Map.insert x i env') $ do
    eval e
eval eval (PrimE op es) = do
  vDs <- liftM blah $ mapM eval es
  return $ fmap (delta op) vDs
  where
    blah :: [dom a] -> dom [a]
    blah = undefined

type CFAAddr_SL = CFAAddr String
type CFAVal_SL = Val CFAAddr_SL

run_zpdcfa_SL :: Expr -> ListSet (ListSet CFAVal_SL, Store ListSet CFAAddr_SL CFAVal_SL, ZCFATime)
run_zpdcfa_SL expr = driveZPDCFA eval expr
