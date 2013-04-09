module Main where

import AAI
import Analyses
import Fixpoints
import Lang
import Lang.Lambda.BigStep.Abstract
import Monads
import PrettyUtil
import StateSpace
import Util
import qualified Text.PrettyPrint.ANSI.Leijen as PP

main :: IO ()
main = do
  PP.putDoc $ fpretty $ runZPDCFA e1
  putStrLn ""
  PP.putDoc $ fpretty $ runConcrete e1
