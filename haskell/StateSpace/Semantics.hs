{-# LANGUAGE RankNTypes, TypeFamilies, MultiParamTypeClasses, FunctionalDependencies, TypeOperators #-}

module StateSpace.Semantics where

class Inject t where
  inject :: a -> t a

newtype (:.:) g f x = Compose { unCompose :: g (f x) }

class SmallStep m where
  type SS m :: * -> *
  runSS :: (a -> m b) -> SS m a -> SS m b

-- class SmallStepT t where
--   data SST t :: (* -> *) -> * -> *
--   runSST :: forall m a. (SmallStep m) => (a -> t m a) -> SST t (SS m) a -> SST t (SS m) a

class BigStep t where
  type InBS t :: * -> *
  type OutBS t :: * -> *
  askInBS :: forall m a. (Monad m) => a -> t m (InBS t a)
  runBS :: forall m a b. (Monad m) => (a -> t m b) -> InBS t a -> m (OutBS t b)

