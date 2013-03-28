module Util.Map where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import SExpKit
import PrettyUtil
import Control.Arrow

instance (FPretty k, FPretty v) => FPretty (Map k v) where
  fpretty = 
    prettyCurly 
    . map (uncurry prettyArrow . (fpretty *** fpretty)) 
    . Map.toList
