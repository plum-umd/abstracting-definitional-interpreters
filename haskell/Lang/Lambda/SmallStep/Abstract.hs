{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}

module Lang.Lambda.SmallStep.Abstract where

import AAI
import Monads
import StateSpace
import Lang.Lambda.Data
import Util
import qualified Data.Map as Map
import Control.Monad

data Val addr =
    Num Integer
  | Nat
  | Clo [String] Call (Env String addr)
  deriving (Eq, Ord, Show)

type AbstractMonad dom time addr m =
  ( MonadPlus m
  , MonadEnvState (Env String addr) m
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

atomic :: (AbstractMonad dom time addr m)
       => Atom 
       -> m (dom (Val addr))
atomic (LitA i) = return $ unit $ Num i
atomic (VarA x) = do
  env <- getEnv
  store <- getStore
  return $ store Map.! (env Map.! x)
atomic (LamA x k body) = atomicClo [x,k] body
atomic (KonA x body) = atomicClo [x] body
atomic (PrimA o as) = do
  vsD <- liftM blah $ mapM atomic as
  return $ fmap (delta o) vsD
  where
    blah :: [dom a] -> dom [a]
    blah = undefined

atomicClo :: (AbstractMonad dom time addr m)
          => [String] 
          -> Call 
          -> m (dom (Val addr))
atomicClo xs body = do
  env <- getEnv
  return $ unit $ Clo xs body env

step :: (AbstractMonad dom time addr m)
     => Call 
     -> m Call
step (LetC x a c) = do
  i <- alloc x
  vD <- atomic a
  modifyEnv $ Map.insert x i
  modifyStore $ updateStoreD i vD
  return c
step (LetRecC f x k body c) = do
  i <- alloc f
  modifyEnv $ Map.insert f i
  env <- getEnv
  let v = Clo [x,k] body env
  modifyStore $ updateStore i v
  return c
step (IfZC a tb fb) = do
  v <- promote =<< atomic a
  case v of
    Num 0 -> return tb
    Num _ -> return fb
    Nat -> msum $ map return [tb, fb]
    _ -> error "ill-formed if0 expression"
step (LAppC f arg karg) = stepApply f [arg,karg]
step (KAppC k arg) = stepApply k [arg]
step (HaltC a) = return $ HaltC a

stepApply :: (AbstractMonad dom time addr m) => Atom -> [Atom] -> m Call
stepApply f args = do
  Clo xs body env <- promote =<< atomic f
  vDs <- mapM atomic args
  is <- mapM alloc xs
  putEnv env
  mapM_ (modifyEnv . uncurry Map.insert) $ zip xs is
  mapM_ (modifyStore . uncurry Map.insert) $ zip is vDs
  return body
