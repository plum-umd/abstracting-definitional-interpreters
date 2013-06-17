module Main where

import AAI
import Analyses
import Fixpoints
import Lang
import Lang.Lambda.BigStep.Abstract
import Monads
import StateSpace
import Util
import qualified Data.Text.Lazy.IO as T
import Text.MPretty

main :: IO ()
main = do
  T.putStrLn $ execPretty $ pretty $ runZPDCFA e1
  putStrLn ""
  T.putStrLn $ execPretty $ pretty $ runConcrete e1
