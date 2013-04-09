{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Monads.Classes.MonadTimeState where

import Control.Monad.Trans

class (Monad m) => MonadTimeState time m | m -> time where
  getTime :: m time
  putTime :: time -> m ()

modifyTime :: (MonadTimeState time m) => (time -> time) -> m ()
modifyTime f = do
  s <- getTime
  putTime $ f s

mGetTime :: (MonadTimeState time m, MonadTrans t) => t m time
mGetTime = lift getTime

mPutTime :: (MonadTimeState time m, MonadTrans t) => time -> t m ()
mPutTime = lift . putTime
