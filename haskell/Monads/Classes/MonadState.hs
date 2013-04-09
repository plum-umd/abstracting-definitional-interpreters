module Monads.Classes.MonadState where

import Control.Monad.State

mGet :: (MonadState s m, MonadTrans t) => t m s
mGet = lift get

mPut :: (MonadState s m, MonadTrans t) => s -> t m ()
mPut = lift . put

