
class (Monad m) => MonadStore dom addr m | m -> dom, m -> addr where
  getStore :: m (Store dom addr)
  putStore :: Store dom addr -> m ()

modifyStore :: (MonadStore dom addr m) 
            => (Store dom addr -> Store dom addr) 
            -> m ()
modifyStore f = do
  s <- getStore
  putStore (f s)

