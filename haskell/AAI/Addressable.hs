{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module AAI.Addressable where

import Monads

class Addressable addr var time | time -> addr, time -> var where
  tzero :: time
  tadvance :: time -> time
  talloc :: var -> time -> addr

alloc :: (MonadTimeState time m, Addressable addr var time) => var -> m addr
alloc x = do
  t <- getTime
  putTime (tadvance t)
  return (talloc x t)
