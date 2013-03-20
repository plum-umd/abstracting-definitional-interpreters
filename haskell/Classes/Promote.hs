{-# LANGUAGE MultiParamTypeClasses #-}

module Classes.Promote where

class Promote small large where
  promote :: small a -> large a


