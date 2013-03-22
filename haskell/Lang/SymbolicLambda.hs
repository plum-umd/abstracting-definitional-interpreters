{-# LANGUAGE FlexibleContexts #-}

module Lang.SymbolicLambda where

import AAI
import Analyses
import Control.Monad
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
  | IfZ Expr Expr Expr
  | Primop Op [Expr]
  | Lit Integer
  -- | LetRecL f x b e ~ let rec f = \x -> b in e
  | LetRec String String Expr Expr
  deriving (Eq, Ord, Show)

letE :: String -> Expr -> Expr -> Expr
letE x e1 e2 = App (Lam x e2) e1

data Op =
    Add1
  | Sub1
  deriving (Eq, Ord, Show)

data Val addr =
    Nat
  | Num Integer
  | Clo String Expr (Env String addr)
  deriving (Eq, Ord, Show)

-- Evaluator --

delta :: Op -> [Val addr] -> Val addr
delta Add1 [Num _] = Nat
delta Add1 [Nat] = Nat
delta Sub1 [Num _] = Nat
delta Sub1 [Nat] = Nat

eval :: ( MonadPlus m
        , MonadEnv (Env String addr) m
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
eval eval (IfZ c tb fb) = do
  cv <- eval c
  case cv of
    Nat -> eval tb `mplus` eval fb
    Num 0 -> eval tb
    _     -> eval fb
eval eval (Primop op es) = do
  vs <- mapM eval es
  return $ delta op vs
eval eval (Lit n) = return (Num n)
eval eval (LetRec f x b e) = do
  a <- alloc f
  localEnv (Map.insert f a) $ do
    env <- askEnv
    let v = Clo x b env
    modifyStore (joinStore a v)
    eval e

-- ZPDCFA

type CFAAddr_SL = CFAAddr String
type CFAVal_SL = Val CFAAddr_SL

run_zpdcfa_SL :: Expr -> ListSet (CFAVal_SL, Store ListSet CFAAddr_SL CFAVal_SL, ZCFATime)
run_zpdcfa_SL expr = driveZPDCFA eval expr

e1 :: Expr
e1 = IfZ (Primop Add1 [Lit 7]) (Lit 1) (Lit 2)

e2 :: Expr
{-
e2 = (let* ((id (lambda (x) ((lambda (q) q) x))) 
            (y (id 0)) 
            (z (id 1))) 
       y) 
-}
e2 = LetRec "id" "x" ((Lam "q" q) `App` x) $
     letE "y" (id `App` Lit 0) $
     letE "z" (id `App` Lit 1) $
     y
  where
    id = Var "id"
    x = Var "x"
    q = Var "q"
    y = Var "y"
    z = Var "z"
