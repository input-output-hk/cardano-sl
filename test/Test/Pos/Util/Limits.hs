{-# LANGUAGE ScopedTypeVariables #-}

-- | Test on functions trimming data to given limit.

module Test.Pos.Util.Limits where


import           Universum

import qualified Data.HashMap.Strict        as HM
import           Serokell.Data.Memory.Units (fromBytes)
import           Test.Hspec                 (Spec, describe)
import           Test.Hspec.QuickCheck      (prop)
import           Test.QuickCheck            (arbitrary, choose, forAll, (===))

import qualified Pos.Binary.Class           as Bi
import           Pos.Util.Limits            (stripHashMap)

spec :: Spec
spec = describe "Limits" $ do
    describe "stripHashMap" $ do
        prop "limit more than size doesn't corrupt hashmap" $
            genByte 0 $ \limit -> genHmap $ \hm ->
            stripHashMap (limit + Bi.biSize hm) hm === hm
        prop "stripped map has size <= limit" $
            genByte 1 $ \limit -> genHmap $ \hm ->
            Bi.biSize (stripHashMap limit hm) <= limit
  where
    genHmap f = forAll arbitrary $ \(HM.fromList -> hm :: HashMap Word64 Bool) -> f hm
    genByte low f = forAll (choose (low, 1000)) $ \(fromBytes -> limit) -> f limit
