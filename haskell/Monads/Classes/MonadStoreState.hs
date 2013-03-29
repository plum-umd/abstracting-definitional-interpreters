{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

module Monads.Classes.MonadStoreState where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.List
import Util

class (Monad m) => MonadStoreState store m | m -> store where
  getStore :: m store
  putStore :: store -> m ()

modifyStore :: (MonadStoreState store m) => (store -> store) -> m ()
modifyStore f = do
  s <- getStore
  putStore $ f s

monadGetStore :: (MonadStoreState store m, MonadTrans t) => t m store
monadGetStore = lift getStore

monadPutStore :: (MonadStoreState store m, MonadTrans t) => store -> t m ()
monadPutStore = lift . putStore
