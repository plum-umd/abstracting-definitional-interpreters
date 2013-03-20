{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Classes.MonadStore where

import Data.Map (Map)
import qualified Data.Map as Map
import Classes.Pointed
import Classes.Lattice

type Store dom addr val = Map addr (dom (val addr))

class (Monad m) => MonadStore dom addr val m | m -> dom, m -> addr, m -> val where
  getStore :: m (Store dom addr val)
  putStore :: Store dom addr val -> m ()

modifyStore :: (MonadStore dom addr val m) 
            => (Store dom addr val -> Store dom addr val)
            -> m ()
modifyStore f = do
  s <- getStore
  putStore (f s)

joinStore :: (Pointed dom, Lattice (dom (val addr)), Ord addr) 
            => addr 
            -> val addr 
            -> Store dom addr val 
            -> Store dom addr val
joinStore a v s =
  join (Map.singleton a (unit v)) s
