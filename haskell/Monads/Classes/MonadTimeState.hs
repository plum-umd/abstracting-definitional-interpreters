{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

module Monads.Classes.MonadTimeState where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.List
import Util

class (Monad m) => MonadTimeState time m | m -> time where
  getTime :: m time
  putTime :: time -> m ()

modifyTime :: (MonadTimeState time m) => (time -> time) -> m ()
modifyTime f = do
  s <- getTime
  putTime $ f s

monadGetTime :: (MonadTimeState time m, MonadTrans t) => t m time
monadGetTime = lift getTime

monadPutTime :: (MonadTimeState time m, MonadTrans t) => time -> t m ()
monadPutTime = lift . putTime
