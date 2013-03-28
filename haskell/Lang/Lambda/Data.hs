{-# LANGUAGE QuasiQuotes #-}

module Lang.Lambda.Data where

import Control.Monad.State
import PrettyUtil
import SExpKit
import qualified Text.PrettyPrint.ANSI.Leijen as PP

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

-- CPS conversion --

fresh :: String -> State Integer String
fresh x = do
  n <- get
  modify (+1)
  return $ x ++ "$" ++ show n

type MetaK = Either Atom (Atom -> State Integer Call)

objectK :: Atom -> MetaK
objectK = Left

reifyMetaK :: MetaK -> State Integer Atom
reifyMetaK (Left a) = return a
reifyMetaK (Right k) = do
  x <- fresh "x"
  let ox = VarA x
  kBody <- k ox
  return $ KonA x kBody

metaK :: (Atom -> State Integer Call) -> MetaK
metaK = Right

callMetaK :: MetaK -> (Atom -> State Integer Call)
callMetaK (Left ka) a = return $ KAppC ka a
callMetaK (Right k) a = k a

cps :: Expr -> Call
cps e = evalState (cpsMK e $ metaK (return . HaltC)) 0

cpsMK :: Expr -> MetaK -> State Integer Call
cpsMK (LitE n) mk = callMetaK mk $ LitA n
cpsMK (VarE x) mk = callMetaK mk $ VarA x
cpsMK (LamE x body) mk = do
  k <- fresh "k"
  let ok = VarA k
  bodyCall <- cpsMK body $ objectK ok
  callMetaK mk $ LamA x k bodyCall
cpsMK (LetE x e1 e2) mk =
  cpsMK e1 $ metaK $ \ e1a -> do
    e2Call <- cpsMK e2 mk
    return $ LetC x e1a e2Call  
cpsMK (LetRecE f x body e) mk = do
  k <- fresh "k"
  let ok = VarA k
  bodyCall <- cpsMK body $ objectK ok
  cpsMK e $ metaK $ \ ea -> do
    eCall <- callMetaK mk ea
    return $ LetRecC f x k bodyCall eCall
cpsMK (IfZE c tb fb) mk =
  cpsMK c $ metaK $ \ ca -> do
    ok <- reifyMetaK mk
    k <- fresh "k"
    tbCall <- cpsMK tb $ objectK $ VarA k
    fbCall <- cpsMK fb $ objectK $ VarA k
    return $ LetC k ok $ IfZC ca tbCall fbCall
cpsMK (AppE f e) mk =
  cpsMK f $ metaK $ \ fa ->
    cpsMK e $ metaK $ \ ea -> do
      ok <- reifyMetaK mk
      return $ LAppC fa ea ok
cpsMK (PrimE op es) mk =
  mapCPS es $ \ eas ->
    callMetaK mk $ PrimA op eas

mapCPS :: [Expr] -> ([Atom] -> State Integer Call) -> State Integer Call
mapCPS [] k = k []
mapCPS (e:es) k =
  cpsMK e $ metaK $ \ a ->
    mapCPS es $ \ as -> k (a:as)

-- from SExp conversion --

eFromS :: SExp -> Expr
eFromS [sexp| @Lit$n |] = LitE n
eFromS [sexp| @Sym$x |] = VarE x
eFromS [sexp| (lam (@Sym$x) @$body) |] = LamE x (eFromS body)
eFromS [sexp| (let (@Sym$x @$e1) @$e2) |] = LetE x (eFromS e1) (eFromS e2)
eFromS [sexp| (letrec (@Sym$f (@Sym$x) @$body) @$e) |] = LetRecE f x (eFromS body) (eFromS e)
eFromS [sexp| (if0 @$c @$tb @$fb) |] = IfZE (eFromS c) (eFromS tb) (eFromS fb)
eFromS [sexp| (add1 @$e) |] = PrimE Add1 [eFromS e]
eFromS [sexp| (sub1 @$e) |] = PrimE Sub1 [eFromS e]
eFromS [sexp| (@$e1 @$e2) |] = AppE (eFromS e1) (eFromS e2)
eFromS [sexp| (@$e1 @$e2 . @$es) |] = eFromS [sexp| ((@$e1 @$e2) . @$es) |]
eFromS _ = error "could not convert from S"

aFromS :: SExp -> Atom
aFromS [sexp| @Lit$n |] = LitA n
aFromS [sexp| @Sym$x |] = VarA x
aFromS [sexp| (lam (@Sym$x @Sym$k) @$body) |] = LamA x k (cFromS body)
aFromS [sexp| (kon (@Sym$x) @$body) |] = KonA x (cFromS body)
aFromS [sexp| (add1 @$a) |] = PrimA Add1 [aFromS a]
aFromS [sexp| (sub1 @$a) |] = PrimA Sub1 [aFromS a]

cFromS :: SExp -> Call
cFromS [sexp| (let (@Sym$x @$a) @$c) |] = LetC x (aFromS a) (cFromS c)
cFromS [sexp| (letrec (@Sym$f (@Sym$x @Sym$k) @$body) @$c) |] = LetRecC f x k (cFromS body) (cFromS c)
cFromS [sexp| (if0 @$ca @$tb @$fb) |] = IfZC (aFromS ca) (cFromS tb) (cFromS fb)
cFromS [sexp| (<halt> @$a) |] = HaltC (aFromS a)
cFromS [sexp| (@$a1 @$a2 @$a3) |] = LAppC (aFromS a1) (aFromS a2) (aFromS a3)
cFromS [sexp| (@$a1 @$a2) |] = KAppC (aFromS a1) (aFromS a2)

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
    [ keywordColor $ PP.text "HALT"
    , fpretty e
    ]

instance FPretty Expr where
  fpretty = fpretty . eToP
instance FPretty Atom where
  fpretty = fpretty . aToP
instance FPretty Call where
  fpretty = fpretty . cToP

instance Show Expr where
  show = ($ []) . PP.displayS . PP.renderPretty 1.0 1000 . PP.plain . PP.group . fpretty
instance Show Atom where
  show = ($ []) . PP.displayS . PP.renderPretty 1.0 1000 . PP.plain . PP.group . fpretty
instance Show Call where
  show = ($ []) . PP.displayS . PP.renderPretty 1.0 1000 . PP.plain . PP.group . fpretty
