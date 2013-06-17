module Lang.Lambda.Printing where

import Lang.Lambda.Data
import Text.MPretty

-- A larger lambda language purely for printing purposes
data PExpr =
    LitP Integer
  | VarP String
  | LamP [String] PExpr
  | LetP String PExpr PExpr
  | LetRecP String [String] PExpr PExpr
  | IfZP PExpr PExpr PExpr
  | AppP PExpr [PExpr]
  | PrimP Op [PExpr]
  | HaltP PExpr
  deriving (Eq, Ord)

-- To PExpr Conversion

appToList :: Expr -> Expr -> [Expr]
appToList e1 e2 = appToList' e1 [e2]
  where
    appToList' (AppE e1 e2) es = appToList' e1 (e2:es)
    appToList' e es = e:es

eToP :: Expr -> PExpr
eToP (LitE n) = LitP n
eToP (VarE x) = VarP x
eToP (LamE x body) = LamP [x] $ eToP body
eToP (LetE x e1 e2) = LetP x (eToP e1) $ eToP e2
eToP (LetRecE f x body e) = LetRecP f [x] (eToP body) $ eToP e
eToP (IfZE c tb fb) = IfZP (eToP c) (eToP tb) (eToP fb)
eToP (AppE e1 e2) = 
  let e:es = appToList e1 e2
  in AppP (eToP e) $ map eToP es
eToP (PrimE op es) = PrimP op $ map eToP es

aToP :: Atom -> PExpr
aToP (LitA n) = LitP n
aToP (VarA x) = VarP x
aToP (LamA x k body) = LamP [x,k] $ cToP body
aToP (KonA x body) = LamP [x] $ cToP body
aToP (PrimA op as) = PrimP op $ map aToP as

cToP :: Call -> PExpr
cToP (LetC x a c) = LetP x (aToP a) $ cToP c
cToP (LetRecC f x k body c) = LetRecP f [x,k] (cToP body) $ cToP c
cToP (IfZC c tb fb) = IfZP (aToP c) (cToP tb) (cToP fb)
cToP (LAppC f x k) = AppP (aToP f) $ map aToP [x, k]
cToP (KAppC k x) = AppP (aToP k) $ map aToP [x]
cToP (HaltC a) = HaltP $ aToP a

-- PExpr Pretty Printing

instance IsPretty PExpr where
  pretty (LitP l) = pretty l
  pretty (VarP x) = string x
  pretty (LamP xs body) = 
    group $ buffer $ indentWidth 1 $ parenthesize $ hsep
      [ keyword $ string "lam"
      , parenthesize $ buffer $ hsep $ map (binder . string) xs
      , dropIndent $ pretty body
      ]
  pretty (LetP x e1 e2) = 
    group $ buffer $ indentWidth 1 $ parenthesize $ hsep
      [ keyword $ string "let"
      , parenthesize $ hsep 
          [ binder $ string x
          , align $ pretty e1
          ]
      , dropIndent $ pretty e2
      ]
  pretty (LetRecP f xs body e) = 
    group $ buffer $ indentWidth 1 $ parenthesize $ hsep
    [ keyword $ string "letrec"
    , parenthesize $ hsep $ concat
        [ [ binder $ string f ]
        , map (binder . string) xs
        , [ align $ pretty body ]
        ]
    , dropIndent $ pretty e
    ]
  pretty (IfZP c tb fb) =
    group $ buffer $ indentWidth 1 $ parenthesize $ hsep
      [ keyword $ string "if0"
      , align $ pretty c
      , dropIndent $ pretty tb
      , dropIndent $ pretty fb
      ]
  pretty (AppP e es) =
    sexpList $ pretty e : map pretty es
  pretty (PrimP op es) =
    let opName = case op of
          Add1 -> "add1"
          Sub1 -> "sub1"
    in sexpList $ keyword (string opName) : map pretty es
  pretty (HaltP e) =
    sexpList [keyword $ string "halt", pretty e]

-- Pretty Printing and Show Instances

instance IsPretty Expr where
  pretty = pretty . eToP
instance IsPretty Atom where
  pretty = pretty . aToP
instance IsPretty Call where
  pretty = pretty . cToP

instance Show Expr where
  show = showFromPretty
instance Show Atom where
  show = showFromPretty
instance Show Call where
  show = showFromPretty
