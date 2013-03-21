{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module StateSpace.CFA where

import AAI

type CFAAddr var = var
type ZCFATime = ()

instance Addressable (CFAAddr var) var ZCFATime where
  tzero = ()
  tadvance = const ()
  talloc x _ = x
