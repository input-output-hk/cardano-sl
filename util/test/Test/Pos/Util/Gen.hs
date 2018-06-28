module Test.Pos.Util.Gen
        (
          genMillisecond
        ) where

import           Universum

import           Data.Time.Units (Millisecond, fromMicroseconds)
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genMillisecond :: Gen Millisecond
genMillisecond =
    fromMicroseconds <$> (toInteger <$> Gen.int Range.constantBounded)
