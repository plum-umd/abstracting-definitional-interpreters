{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}

module StateSpace.Env where

import Control.Monad.Reader
import Util.Lens
import Util.Lattice
import Data.Map (Map)

type Env var addr = Map var addr
