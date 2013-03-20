module Classes 
  ( module Classes.Addressable
  , module Classes.Lattice
  , module Classes.MonadEnv
  , module Classes.MonadStore
  , module Classes.MonadTime
  , module Classes.Pointed
  , module Classes.Promote
  , alloc
  ) where

import Classes.Addressable
import Classes.Lattice
import Classes.MonadEnv
import Classes.MonadStore
import Classes.MonadTime
import Classes.Pointed
import Classes.Promote

alloc :: (Addressable addr time, MonadTime time m) 
      => String 
      -> m addr
alloc x = do
  t <- getTime
  putTime (tadvance t)
  return (talloc x t)
