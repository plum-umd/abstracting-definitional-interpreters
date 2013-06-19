{-# LANGUAGE RankNTypes, TypeFamilies, MultiParamTypeClasses, FunctionalDependencies #-}

module StateSpace.Semantics where

class Inject t where
  inject :: a -> t a

class SmallStep m where
  data SS m :: * -> *
  runSS :: (a -> m b) -> SS m a -> SS m b

class BigStep t where
  data InBS t :: * -> *
  data OutBS t :: * -> *
  askInBS :: forall m a. (Monad m) => a -> t m (InBS t a)
  runBS :: forall m a b. (Monad m) => (a -> t m b) -> InBS t a -> m (OutBS t b)
