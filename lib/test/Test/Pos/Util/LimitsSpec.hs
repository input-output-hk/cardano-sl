-- | Test on functions trimming data to given limit.

module Test.Pos.Util.LimitsSpec
    ( spec
    ) where


import           Universum

import qualified Data.HashMap.Strict as HM
import           Serokell.Data.Memory.Units (fromBytes)
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (arbitrary, choose, counterexample, forAll, property, (===))

import qualified Pos.Binary.Class as Bi
import           Pos.Util.Limits (stripHashMap)

spec :: Spec
spec = describe "Limits" $ do
    describe "stripHashMap" $ do
        prop "limit more than size doesn't corrupt hashmap" $
            genByte 0 $ \limit -> genHmap $ \hm ->
            stripHashMap (limit + Bi.biSize hm) hm === Just hm
        prop "stripped map has size <= limit" $
            genByte 1 $ \limit -> genHmap $ \hm ->
            maybe (counterexample "shouldn't be Nothing" $ False)
                  (\s -> property $ Bi.biSize s <= limit)
                  (stripHashMap limit hm)
  where
    genHmap f = forAll arbitrary $ \(HM.fromList -> hm :: HashMap Word64 Bool) -> f hm
    genByte low f = forAll (choose (low, 1000)) $ \(fromBytes -> limit) -> f limit
