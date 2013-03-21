module Main where

import AAI
import Analyses
import Fixpoints
import Lang
import Monads
import StateSpace
import Util

import Lang.SymbolicLambda

main :: IO ()
main = putStrLn $ show $ run_zpdcfa_SL e1
