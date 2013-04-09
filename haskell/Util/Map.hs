module Util.Map where

import PrettyUtil
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Arrow

instance (FPretty k, FPretty v) => FPretty (Map k v) where
  fpretty = 
    prettyCurly 
    . map (uncurry prettyArrow . (fpretty *** fpretty)) 
    . Map.toList
