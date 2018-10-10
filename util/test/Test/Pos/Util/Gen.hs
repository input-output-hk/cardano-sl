module Test.Pos.Util.Gen
        ( genMillisecond
        , genHashMap
        , genHashSet
        ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.Time.Units (Millisecond, fromMicroseconds)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genMillisecond :: Gen Millisecond
genMillisecond =
    fromMicroseconds <$> (toInteger <$> Gen.int Range.constantBounded)

genHashMap
  :: (Hashable k, Eq k) => Range Int -> Gen k -> Gen v -> Gen (HM.HashMap k v)
genHashMap range keyGen valGen =
  HM.fromList <$> (Gen.list range $ (,) <$> keyGen <*> valGen)

genHashSet :: (Hashable a, Eq a) => Gen a -> Gen (HashSet a)
genHashSet = fmap HS.fromList . Gen.list (Range.linear 0 10)
