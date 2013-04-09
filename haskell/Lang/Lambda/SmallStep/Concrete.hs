{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}

module Lang.Lambda.SmallStep.Concrete where

import AAI
import Data.Map (Map)
import Lang.Lambda.Data
import Lang.Lambda.Printing
import Monads
import StateSpace
import qualified Data.Map as Map

data Val =
    Num Integer
  | Clo [String] Call (Env String Integer)
  deriving (Eq, Ord)

type ConcreteMonad m = 
  ( MonadEnvState (Map String Integer) m
  , MonadStoreState (Map Integer Val) m
  , MonadTimeState Integer m
  )

delta :: Op -> [Val] -> Val
delta Add1 [Num x] = Num (x+1)
delta Sub1 [Num x] = Num (x-1)
delta _ _ = error "bad prim application"

atomic :: (ConcreteMonad m) => Atom -> m Val
atomic (LitA i) = return $ Num i
atomic (VarA x) = do
  env <- getEnv
  store <- getStore
  return $ store Map.! (env Map.! x)
atomic (LamA x k body) = atomicClo [x,k] body
atomic (KonA x body) = atomicClo [x] body
atomic (PrimA o as) = do
  vs <- mapM atomic as
  return $ delta o vs

atomicClo :: (ConcreteMonad m) => [String] -> Call -> m Val
atomicClo xs body = do
  env <- getEnv
  return $ Clo xs body env

step :: (ConcreteMonad m) => Call -> m Call
step (LetC x a c) = do
  i <- alloc x
  v <- atomic a
  modifyEnv $ Map.insert x i
  modifyStore $ Map.insert i v
  return c
step (LetRecC f x k body c) = do
  i <- alloc f
  modifyEnv $ Map.insert f i
  vD <- atomic (LamA x k body)
  modifyStore $ Map.insert i vD
  return c
step (IfZC a tb fb) = do
  v <- atomic a
  case v of
    Num 0 -> return tb
    Num _ -> return fb
    _ -> error "ill-formed if0 expression"
step (LAppC f arg karg) = stepApply f [arg,karg]
step (KAppC k arg) = stepApply k [arg]
step (HaltC a) = return $ HaltC a

stepApply :: (ConcreteMonad m) => Atom -> [Atom] -> m Call
stepApply f args = do
  Clo xs body env <- atomic f
  vs <- mapM atomic args
  is <- mapM alloc xs
  putEnv env
  mapM_ (modifyEnv . uncurry Map.insert) $ zip xs is
  mapM_ (modifyStore . uncurry Map.insert) $ zip is vs
  return body

