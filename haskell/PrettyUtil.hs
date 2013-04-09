module PrettyUtil where

import qualified Text.PrettyPrint.ANSI.Leijen as PP

class FPretty a where
  fpretty :: a -> PP.Doc
  fprettyList :: [a] -> PP.Doc
  fprettyList = prettySquare . map fpretty

instance (FPretty a) => FPretty [a] where
  fpretty = fprettyList

instance FPretty () where
  fpretty () = puncColor $ PP.text "()"

instance FPretty Bool where
  fpretty = litColor . PP.pretty

instance FPretty Char where
  fpretty = PP.pretty
  fprettyList = PP.prettyList

instance FPretty Int where
  fpretty = litColor . PP.pretty

instance FPretty Integer where
  fpretty = litColor . PP.pretty

instance FPretty Float where
  fpretty = litColor . PP.pretty

instance FPretty Double where
  fpretty = litColor . PP.pretty

instance (FPretty a, FPretty b) => FPretty (a,b) where
  fpretty (x,y) = prettyTuple [fpretty x, fpretty y]

instance (FPretty a, FPretty b, FPretty c) => FPretty (a,b,c) where
  fpretty (x,y,z) = prettyTuple [fpretty x, fpretty y, fpretty z]

prettySExp :: [PP.Doc] -> PP.Doc
prettySExp ds = PP.group $
  PP.hcat
  [ puncColor $ PP.char '('
  , PP.align $ PP.vsep ds
  , puncColor $ PP.char ')'
  ]

prettySepBrac :: String -> String -> String -> [PP.Doc] -> PP.Doc
prettySepBrac sep lbrac rbrac ds = 
  case ds of
    [] -> puncColor $ PP.text $ lbrac ++ rbrac
    _ ->
      let (d:ds') = ds
      in PP.group $
        PP.hsep
        [ puncColor $ PP.text lbrac
        , PP.vsep $ concat
          [ [PP.align d]
          , flip map ds' $ \x -> 
              PP.hsep 
              [ puncColor $ PP.text sep
              , PP.align x
              ]
          , [puncColor $ PP.text rbrac]
          ]
        ]

prettyTuple :: [PP.Doc] -> PP.Doc
prettyTuple = prettySepBrac "," "(" ")"

prettyCurly :: [PP.Doc] -> PP.Doc
prettyCurly = prettySepBrac "," "{" "}"

prettySquare :: [PP.Doc] -> PP.Doc
prettySquare = prettySepBrac "," "[" "]"

prettyAngle :: [PP.Doc] -> PP.Doc
prettyAngle = prettySepBrac "," "<" ">"

prettySepMapping :: String -> PP.Doc -> PP.Doc -> PP.Doc
prettySepMapping sep k v = PP.group $
  PP.vsep
  [ k
  , puncColor $ PP.text sep
  , v
  ]

prettyArrow :: PP.Doc -> PP.Doc -> PP.Doc
prettyArrow = prettySepMapping "=>"

litColor = PP.dullred
puncColor = PP.dullyellow

showFromPretty :: (FPretty a) => a -> String
showFromPretty = ($ []) . PP.displayS . PP.renderPretty 1.0 1000 . PP.plain . PP.group . fpretty
