module Util.Pointed where

class Pointed m where
  unit :: a -> m a

instance Pointed [] where
  unit = (:[])

