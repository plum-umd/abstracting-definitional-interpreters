{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Monads.Classes.MonadStoreState where

import Control.Monad.Trans

class (Monad m) => MonadStoreState store m | m -> store where
  getStore :: m store
  putStore :: store -> m ()

modifyStore :: (MonadStoreState store m) => (store -> store) -> m ()
modifyStore f = do
  s <- getStore
  putStore $ f s

mGetStore :: (MonadStoreState store m, MonadTrans t) => t m store
mGetStore = lift getStore

mPutStore :: (MonadStoreState store m, MonadTrans t) => store -> t m ()
mPutStore = lift . putStore
