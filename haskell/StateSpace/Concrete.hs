{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module StateSpace.Concrete where

import AAI

type ConcreteAddr = Integer
type ConcreteTime = Integer

instance Addressable ConcreteAddr var ConcreteTime where
  tzero = 0
  tadvance = (+1)
  talloc _ = id
