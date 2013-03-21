{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Monads.Classes.Promote where

import Control.Monad.State
import Control.Monad.Reader

class Promote small large where
  promote :: small a -> large a

-- plumbing

instance (Monad n, Promote m n) => Promote m (ReaderT r n) where
  promote = lift . promote

instance (Monad n, Promote m n) => Promote m (StateT s n) where
  promote = lift . promote
