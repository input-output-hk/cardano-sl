module Data.Types
       ( NominalDiffTime (..)
       , mkTime
       ) where

import Prelude

import Data.Generic (class Generic)

import Data.Time.Duration  (Seconds (..))

newtype NominalDiffTime = NominalDiffTime Seconds

mkTime :: Number -> NominalDiffTime
mkTime = NominalDiffTime <<< Seconds

derive instance genericNominalDiffTime :: Generic NominalDiffTime
derive instance eqNominalDiffTime :: Eq NominalDiffTime
derive instance ordNominalDiffTime :: Ord NominalDiffTime
