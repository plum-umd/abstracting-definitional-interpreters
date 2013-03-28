{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

module Monads.Classes.MonadTimeState where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.List
import Util.ListSet

class (Monad m) => MonadTimeState time m | m -> time where
  getTime :: m time
  putTime :: time -> m ()

modifyTime :: (MonadTimeState time m) => (time -> time) -> m ()
modifyTime f = do
  s <- getTime
  putTime $ f s

-- plumbing

instance (MonadTimeState time m) => MonadTimeState time (ListSetT m) where
  getTime = lift getTime
  putTime = lift . putTime

instance (MonadTimeState time m) => MonadTimeState time (ReaderT r m) where
  getTime = lift getTime
  putTime = lift . putTime

instance (MonadTimeState time m) => MonadTimeState time (StateT s m) where
  getTime = lift getTime
  putTime = lift . putTime
