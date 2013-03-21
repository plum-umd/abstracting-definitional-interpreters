{-# LANGUAGE FlexibleContexts #-}

module Lang.Lambda where

import AAI
import Data.Map (Map)
import Data.Maybe
import Monads
import StateSpace
import Util
import qualified Data.Map as Map

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
    Num Integer
  | Clo String Expr (Env String addr)

-- Evaluator --

delta :: Op -> [Val addr] -> Val addr
delta Add1 [Num x] = Num (x + 1)
delta Sub1 [Num x] = Num (x - 1)

eval :: ( MonadEnv (Env String addr) m
        , MonadStore (Store dom addr (Val addr)) m
        , MonadTime time m
        , Addressable addr String time
        , Pointed dom
        , Promote dom m
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
    Num 0 -> eval tb
    _     -> eval fb
eval eval (Primop op es) = do
  vs <- mapM eval es
  return $ delta op vs
