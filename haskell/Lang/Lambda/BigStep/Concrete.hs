{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}

module Lang.Lambda.BigStep.Concrete where

import Monads
import Lang.Lambda.Data
import qualified Data.Map as Map
import Data.Map (Map)
import AAI
import StateSpace

data Val =
    Num Integer
  | Clo String Expr (Map String Integer)
  deriving (Eq, Ord, Show)

type ConcreteMonad m = 
  ( MonadEnvReader (Map String Integer) m
  , MonadStoreState (Map Integer Val) m
  , MonadTimeState Integer m
  )
delta :: Op -> [Val] -> Val
delta Add1 [Num x] = Num (x+1)
delta Sub1 [Num x] = Num (x-1)
delta _ _ = error "bad prim application"

eval :: (ConcreteMonad m) => (Expr -> m Val) -> Expr -> m Val
eval eval (LitE n) = return (Num n)
eval eval (VarE x) = do
  env <- askEnv
  store <- getStore
  return $ store Map.! (env Map.! x)
eval eval (LamE x e) = do
  env <- askEnv
  return $ Clo x e env
eval eval (LetRecE f x body e) = do
  i <- alloc f
  localEnv (Map.insert f i) $ do
    env <- askEnv
    modifyStore $ Map.insert i $ Clo x body env
    eval e
eval eval (IfZE c tb fb) = do
  cv <- eval c
  case cv of
    Num 0 -> eval tb
    Num _ -> eval fb
    _ -> error "ill-formed if0 expression"
eval eval (AppE e1 e2) = do
  Clo x e env' <- eval e1
  v <- eval e2
  i <- alloc x
  modifyStore $ Map.insert i v
  localEnv (const $ Map.insert x i env') $
    eval e
eval eval (PrimE op es) = do
  vs <- mapM eval es
  return $ delta op vs
