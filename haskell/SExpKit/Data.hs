{-# LANGUAGE TemplateHaskell #-}

module SExpKit.Data where

import Control.Monad
import Data.List
import Language.Haskell.TH
import PrettyUtil
import qualified Text.PrettyPrint.ANSI.Leijen as PP

data PreSExpG lit =
    PreLit lit
  | PreSym String
  | PreNull
  | PreCons (PreSExpG lit) (PreSExpG lit)
  | PreAnti Anti (Maybe String)
  deriving (Eq, Ord, Show)

data Anti =
    AntiVal
  | AntiLit
  | AntiSym
  deriving (Eq, Ord, Show, Enum)

data SExpG lit = 
    Lit lit
  | Sym String
  | Cons (SExpG lit) (SExpG lit)
  | Null
  deriving (Eq, Ord)

type PreSExp = SExpG Integer
type SExp = SExpG Integer

-- Show

consToList :: SExpG lit -> SExpG lit -> Either [SExpG lit] ([SExpG lit], SExpG lit)
consToList sl Null = Left [sl]
consToList sl sr =
  case sr of
    Cons sl' sr' ->
      case consToList sl' sr' of
        Left ss -> Left (sl:ss)
        Right (ss,s) -> Right (sl:ss,s)
    _ -> Right ([sl],sr)

listToCons :: [SExpG lit] -> SExpG lit
listToCons = foldr Cons Null

instance (Show lit) => Show (SExpG lit) where
  show (Lit l) = show l
  show (Sym s) = s
  show (Cons sl sr) =
    case consToList sl sr of
      Left ss -> "(" ++ unwords (map show ss) ++ ")"
      Right (ss,s) -> "(" ++ unwords (map show ss) ++ " . " ++ show s ++ ")"
  show Null = "()"

instance (FPretty lit) => FPretty (SExpG lit) where
  fpretty (Lit l) = litColor $ fpretty l
  fpretty (Sym s) = PP.text s
  fpretty (Cons sl sr) = 
    let ds = case consToList sl sr of
          Left ss ->
            map fpretty ss
          Right (ss,s) ->
            map fpretty ss
            ++
            [ PP.hsep 
              [ puncColor $ PP.char '.'
              , fpretty s
              ]
            ]
            
    in prettySExp ds
  fpretty Null = puncColor $ PP.text "()"

-- QQ

class ToQQ a where
  toQQE :: a -> Q Exp
  toQQP :: a -> Q Pat

instance ToQQ Char where
  toQQE = litE . charL
  toQQP = litP . charL

instance ToQQ Integer where
  toQQE = litE . integerL
  toQQP = litP . integerL

instance (ToQQ a) => ToQQ [a] where
  toQQE = liftM ListE . mapM toQQE
  toQQP = liftM ListP . mapM toQQP

instance (ToQQ lit) => ToQQ (PreSExpG lit) where
  toQQE (PreLit l) = conE 'Lit `appE` toQQE l
  toQQE (PreSym s) = conE 'Sym `appE` toQQE s
  toQQE PreNull = conE 'Null
  toQQE (PreCons e1 e2) = conE 'Cons `appE` toQQE e1 `appE` toQQE e2
  toQQE (PreAnti anti nameM) = do
    let nameVarE = mkNameE nameM
    case anti of
      AntiVal -> nameVarE
      AntiLit -> conE 'Lit `appE` nameVarE
      AntiSym -> conE 'Sym `appE` nameVarE

  toQQP (PreLit l) = conP 'Lit [toQQP l]
  toQQP (PreSym s) = conP 'Sym [toQQP s]
  toQQP PreNull = conP 'Null []
  toQQP (PreCons e1 e2) = conP 'Cons [toQQP e1, toQQP e2]
  toQQP (PreAnti anti nameM) = do
    let nameVarP = mkNameP nameM
    case anti of
      AntiVal -> nameVarP
      AntiLit -> conP 'Lit [nameVarP]
      AntiSym -> conP 'Sym [nameVarP]
  

mkNameE :: Maybe String -> Q Exp
mkNameE Nothing = fail "wildcard can only appear in pattern"
mkNameE (Just name) = varE $ mkName name

mkNameP :: Maybe String -> Q Pat
mkNameP Nothing = wildP
mkNameP (Just name) = varP $ mkName name
