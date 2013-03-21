{-# LANGUAGE MultiParamTypeClasses #-}

module Util.Lens where

-- View is a comonad
-- taken from Kmett; he calls it Store
-- "view structure [s] inside [a]"
data View s a = View { viewK :: s -> a, viewP :: s }

runView :: View s a -> a
runView (View k s) = k s

viewUpdate :: (s -> s) -> View s a -> View s a
viewUpdate f (View k s) = View k (f s)

viewSet :: s -> View s a -> View s a
viewSet s = viewUpdate $ const s

-- Lens is taken from O'Connor, Kmett and Morris; data-lens package
-- "modifications of [a] viewing [s]"
newtype Lens a s = Lens { unLens :: a -> View s a }

class HasLens a s where
  getLens :: Lens a s

runLens :: Lens a s -> a -> a
runLens (Lens f) a = runView $ f a

lens :: (a -> s) -> (s -> a -> a) -> Lens a s
lens get set = Lens $ \a -> View (\s -> set s a) (get a)

isoLens :: (b -> a) -> (a -> b) -> Lens a b
isoLens from to = Lens $ View from . to

lcompose :: Lens b c -> Lens a b -> Lens a c
lcompose (Lens lg) (Lens lf) = Lens $ \a ->
  let (View f sb) = lf a
      (View g sc) = lg sb
  in View (f . g) sc

lget :: Lens a s -> a -> s
lget l a = viewP $ unLens l a

laccess :: a -> Lens a s -> s
laccess = flip lget

lmodify :: (s -> s) -> Lens a s -> Lens a s
lmodify f l = Lens $ viewUpdate f . unLens l

lset :: s -> Lens a s -> Lens a s
lset s = lmodify (const s)
