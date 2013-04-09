module Util.Set where

import PrettyUtil
import Data.Set (Set)
import qualified Data.Set as Set

instance (FPretty a) => FPretty (Set a) where
  fpretty = 
    prettyCurly . map fpretty . Set.toList
