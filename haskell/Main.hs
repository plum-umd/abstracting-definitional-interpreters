module Main where

import AAI
import Analyses
import Fixpoints
import Lang
import Monads
import StateSpace
import Util
import Text.PrettyPrint.ANSI.Leijen
import Lang.Lambda.BigStep.Abstract
import PrettyUtil

main :: IO ()
main = putDoc $ fpretty $ run_zpdcfa_SL e1
