{-# LANGUAGE Rank2Types #-}

module Pos.Core.Configuration.SystemStart
       ( HasSystemStart
       , withSystemStart
       , systemStart
       ) where

import           Data.Reflection (Given (..), give)
import           Pos.Core.Types  (Timestamp)

type HasSystemStart = Given Timestamp

withSystemStart :: Timestamp -> (HasSystemStart => r) -> r
withSystemStart = give

systemStart :: HasSystemStart => Timestamp
systemStart = given
