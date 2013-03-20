module Classes.Pointed where

class Pointed m where
  unit :: a -> m a

