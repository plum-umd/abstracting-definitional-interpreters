module Lang.Lambda.Printing where

import Lang.Lambda.Data
import PrettyUtil
import qualified Text.PrettyPrint.ANSI.Leijen as PP

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

inParens :: PP.Doc -> PP.Doc
inParens x = PP.hcat 
  [ puncColor $ PP.char '('
  , PP.align x
  , puncColor $ PP.char ')'
  ]

binderColor = PP.dullcyan
keywordColor = PP.underline
primColor = PP.bold . litColor

instance FPretty PExpr where
  fpretty (LitP l) = litColor $ fpretty l
  fpretty (VarP x) = PP.text x
  fpretty (LamP xs body) = 
    PP.group $ inParens $ PP.vsep
    [ PP.hsep
      [ keywordColor $ PP.text "lam"
      , inParens $ binderColor $ PP.hsep $ map PP.text xs
      ]
    , fpretty body
    ]
  fpretty (LetP x e1 e2) =
    inParens $ PP.vsep
    [ PP.hsep
      [ keywordColor $ PP.text "let"
      , inParens $ PP.hsep
        [ binderColor $ PP.text x
        , PP.align $ fpretty e1
        ]
      ]
    , fpretty e2
    ]
  fpretty (LetRecP f xs body e) =
    inParens $ PP.vsep
    [ PP.hsep
      [ keywordColor $ PP.text "letrec"
      , PP.align $ PP.group $ inParens $ PP.vsep
        [ PP.hsep
          [ binderColor $ PP.text f
          , inParens $ binderColor $ PP.hsep $ map PP.text xs
          ]
        , fpretty body
        ]
      ]
    , fpretty e
    ]
  fpretty (IfZP c tb fb) =
    inParens $ PP.vsep
    [ PP.hsep
      [ keywordColor $ PP.text "if0"
      , PP.align $ fpretty c
      ]
    , fpretty tb
    , fpretty fb
    ]
  fpretty (AppP e es) =
    PP.group $ inParens $ PP.vsep 
    $ map fpretty $ e:es
  fpretty (PrimP op es) =
    let opName =
          case op of
            Add1 -> "add1"
            Sub1 -> "sub1"
    in
    PP.group $ inParens $ PP.vsep 
    $ primColor (PP.text opName) : map fpretty es
  fpretty (HaltP e) =
    PP.group $ inParens $ PP.vsep
    [ keywordColor $ PP.text "<halt>"
    , fpretty e
    ]

-- Pretty Printing and Show Instances

instance FPretty Expr where
  fpretty = fpretty . eToP
instance FPretty Atom where
  fpretty = fpretty . aToP
instance FPretty Call where
  fpretty = fpretty . cToP

instance Show Expr where
  show = showFromPretty
instance Show Atom where
  show = showFromPretty
instance Show Call where
  show = showFromPretty
