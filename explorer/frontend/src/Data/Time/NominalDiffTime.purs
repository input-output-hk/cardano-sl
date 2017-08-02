module Data.Time.NominalDiffTime
       ( NominalDiffTime (..)
       , mkTime
       , unwrapSeconds
       ) where

import Prelude
import Data.Generic (class Generic, gShow)
import Data.Newtype (class Newtype, unwrap)
import Data.Time.Duration (Seconds(..))

newtype NominalDiffTime = NominalDiffTime Seconds

derive instance newtypeNominalDiffTime :: Newtype NominalDiffTime _
derive instance genericNominalDiffTime :: Generic NominalDiffTime
derive instance eqNominalDiffTime :: Eq NominalDiffTime
derive instance ordNominalDiffTime :: Ord NominalDiffTime
instance showNominalDiffTime :: Show NominalDiffTime where
    show = gShow

mkTime :: Number -> NominalDiffTime
mkTime = NominalDiffTime <<< Seconds

unwrapSeconds :: NominalDiffTime -> Number
unwrapSeconds = unwrap <<< unwrap
