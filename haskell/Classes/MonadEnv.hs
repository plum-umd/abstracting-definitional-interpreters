
class (Monad m) => MonadEnv addr m | m -> addr where
  getEnv :: m (Env addr)
  putEnv :: Env addr -> m ()

