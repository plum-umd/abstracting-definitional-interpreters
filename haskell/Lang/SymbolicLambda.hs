{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module Lang.SymbolicLambda where

import Classes
import Control.Monad.Identity
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.State
import Control.Monad

data Expr =
    Var String
  | Lam String Expr
  | App Expr Expr
  | If Expr Expr Expr
  | Primop Op [Expr]

data Op =
    Add1
  | Sub1

data Val addr =
    Nat
  | Num Integer
  | Clo String Expr (Env addr)

-- Evaluator --

delta :: Op -> [Val addr] -> Val addr
delta Add1 [Num _] = Nat
delta Add1 [Nat] = Nat
delta Sub1 [Num _] = Nat
delta Sub1 [Nat] = Nat

eval :: forall m dom addr time.
        ( MonadEnv addr m, MonadStore dom addr Val m, MonadTime time m
        , MonadPlus m
        , Addressable addr time
        , Promote dom m, Pointed dom
        , Lattice (dom (Val addr))
        , Ord addr) 
     => (Expr -> m (Val addr))
     -> Expr 
     -> m (Val addr)
eval eval (Var x) = do
  env <- askEnv
  store <- getStore
  let addr = fromJust $ Map.lookup x env
  let valD = fromJust $ Map.lookup addr store
  promote valD
eval eval (Lam x e) = do
  env <- askEnv
  return $ Clo x e env
eval eval (App e1 e2) = do
  (Clo x e env') <- eval e1
  v <- eval e2
  a <- alloc x
  localEnv (const $ Map.insert x a env') $ do
    modifyStore (joinStore a v)
    eval e
eval eval (If c tb fb) = do
  cv <- eval c
  case cv of
    Nat -> eval tb `mplus` eval fb
    Num 0 -> eval tb
    _     -> eval fb
eval eval (Primop op es) = do
  vs <- mapM eval es
  return $ delta op vs

