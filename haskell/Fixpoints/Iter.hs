{-# LANGUAGE FlexibleContexts #-}

module Fixpoints.Iter where

import StateSpace.Semantics
import Data.PartialOrder

runIterN ::
  ( SmallStep m
  , Inject (SS m)
  , PartialOrder (SS m a)
  )
  => Int
  -> (a -> m a)
  -> a
  -> (SS m a, Bool)
runIterN n step e =
  let loop 0 ss = (ss, False)
      loop n ss =
        let ss' = runSS step ss
        in if lte ss' ss
          then (ss, True)
          else loop (n-1) ss'
  in loop n $ inject e

runIter ::
  ( SmallStep m
  , Inject (SS m)
  , PartialOrder (SS m a)
  )
  => (a -> m a)
  -> a
  -> SS m a
runIter step e =
  let loop ss =
        let ss' = runSS step ss
        in if lte ss' ss
          then ss
          else loop ss'
  in loop $ inject e
