module Main where

import AAI
import Analyses
import Fixpoints
import Lang
import qualified Lang.Lambda.BigStep.Symbolic as BS
import qualified Lang.Lambda.SmallStep.Symbolic as SS
import qualified Analyses.BigStep as BS
import qualified Analyses.SmallStep as SS
import Monads
import StateSpace
import Util
import qualified Data.Text.Lazy.IO as T
import Text.MPretty
import Data.PartialOrder
import Data.Lattice

main :: IO ()
main = do
  putStrLn "--BS Concrete--"
  ipPrintLn $ BS.runConcrete e1
  putStrLn "--BS ZPDCFA--"
  ipPrintLn $ BS.runZPDCFA e1
  putStrLn "--BS Widening--"
  ipPrintLn $ BS.runWidening e1
  putStrLn "--SS Concrete--"
  ipPrintLn $ SS.runConcrete c1
  putStrLn "--SS ZPDCFA--"
  ipPrintLn $ SS.runZPDCFA c1
