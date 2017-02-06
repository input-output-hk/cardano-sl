module Data.Time.NominalDiffTime.Lenses where

import Prelude as Prelude
import Data.Lens as Lens
import Data.Either as Either
import Data.Time.NominalDiffTime
import Data.Time.Duration  (Seconds (..))


_NominalDiffTime :: Lens.Iso' NominalDiffTime Seconds
_NominalDiffTime = Lens.iso unwrap NominalDiffTime
  where
    unwrap (NominalDiffTime x) = x
