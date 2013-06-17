{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Analyses.Executable where

class Executable t expr env store result 
  | t -> expr, t -> env, t -> store, t -> result, t -> result 
  where
  exec :: expr -> env -> store -> t m a -> m (result a)
