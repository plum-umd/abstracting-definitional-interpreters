module Lang.Lambda.AST where

import Data.PartialOrder

data Op =
    Add1
  | Sub1
  deriving (Eq, Ord)

data Expr =
    LitE Integer
  | VarE String
  | LamE String Expr
  | LetE String Expr Expr
  | LetRecE String String Expr Expr
  | IfZE Expr Expr Expr
  | AppE Expr Expr
  | PrimE Op [Expr]
  deriving (Eq, Ord)

data Atom =
    LitA Integer
  | VarA String
  | LamA String String Call
  | KonA String Call
  | PrimA Op [Atom]
  deriving (Eq, Ord)

data Call =
    LetC String Atom Call
  | LetRecC String String String Call Call
  | IfZC Atom Call Call
  | LAppC Atom Atom Atom
  | KAppC Atom Atom
  | HaltC Atom
  deriving (Eq, Ord)
