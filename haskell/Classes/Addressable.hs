{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Classes.Addressable where

class Addressable addr time | time -> addr where
  tzero :: time
  tadvance :: time -> time
  talloc :: String -> time -> addr
