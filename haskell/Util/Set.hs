module Util.Set where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import SExpKit
import PrettyUtil

instance (FPretty a) => FPretty (Set a) where
  fpretty = 
    prettyCurly . map fpretty . Set.toList
