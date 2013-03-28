module Util.ExtTB where

data ExtBT a =
    ExtBot
  | Ext a
  | ExtTop
  deriving (Eq, Ord)

instance Lattice (ExtBT a) where
  lbot = ExtBot
  ltop = ExtTop
  ljoin ExtTop _ = ExtTop
  ljoin _ ExtTop = ExtTop
  ljoin ExtBot a = a
  ljoin a ExtBot = a
  ljoin (Ext _) (Ext _) = ExtTop
  lmeet ExtBot _ = ExtBot
  lmeet _ ExtBot = ExtBot
  lmeet a ExtTop = a
  lmeet ExtTop a = a
  lmeet (Ext _) (Ext _) = ExtBot
  lrefines ExtTop _ = False
  lrefines _ ExtTop = True
  lrefines ExtBot _ = True
  lrefines _ ExtBot = False
  lrefines (Ext _) (Ext _) = False
