module Monads.Classes.MonadReader where

import Control.Monad.Reader
import Util.MFunctor

mAsk :: (MonadReader r m, MonadTrans t) => t m r
mAsk = lift ask

mLocal :: (MonadReader r m, MFunctor t) => (r -> r) -> t m a -> t m a
mLocal f = mFmap $ local f

