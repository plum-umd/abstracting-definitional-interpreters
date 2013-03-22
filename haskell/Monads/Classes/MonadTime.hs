{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

module Monads.Classes.MonadTime where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.List
import Util.ListSet

class (Monad m) => MonadTime time m | m -> time where
  getTime :: m time
  putTime :: time -> m ()

modifyTime :: (MonadTime time m) => (time -> time) -> m ()
modifyTime f = do
  s <- getTime
  putTime (f s)

-- plumbing

instance (MonadTime time m) => MonadTime time (ListSetT m) where
  getTime = lift getTime
  putTime = lift . putTime

instance (MonadTime time m) => MonadTime time (ReaderT r m) where
  getTime = lift getTime
  putTime = lift . putTime

instance (MonadTime time m) => MonadTime time (StateT s m) where
  getTime = lift getTime
  putTime = lift . putTime
