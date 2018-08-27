{-# LANGUAGE RankNTypes #-}
module DeltaCompressionSpecs (spec) where


import           Universum

import           Formatting (bprint)
import           Formatting.Buildable (build)

import           Test.Hspec
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Monadic (monadicIO, pick)

import           Cardano.Wallet.Kernel.DB.Checkpoints

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

data BMap k v = BMap {unMap :: Map k v} deriving (Show, Eq)

instance (Arbitrary k, Arbitrary v, Ord k) => Arbitrary (BMap k v) where
    arbitrary = BMap <$> arbitrary

instance (Show k, Show v) => Buildable (BMap k v) where
    build = bprint . show

spec :: Spec
spec = do
    describe "roundtrips for deltas -> steps" $ do
        it "Map round trips" $ monadicIO $ do
            (mp :: Map Int Int) <- unMap <$> pick arbitrary
            mp' <- unMap <$> pick arbitrary
            let d = deltaM mp mp'
            let mp'' = stepM mp' d
            return $ mp `shouldBe` mp''

        it "Map round trips, (one Map is strictly bigger)" $ monadicIO $ do
            (mp1 :: Map Int Int) <- unMap <$> pick arbitrary
            mp2 <- unMap <$> pick arbitrary
            let mp = mp1 <> mp2
            let mp' = mp1
            let d = deltaM mp mp'
            let mp'' = stepM mp' d
            return $ mp `shouldBe` mp''

        it "Map round trips, (Maps have intersection and no one is strictly bigger)" $ monadicIO $ do
            (mp1 :: Map Int Int) <- unMap <$> pick arbitrary
            mp2 <- unMap <$> pick arbitrary
            mp3 <- unMap <$> pick arbitrary
            let mp = mp1 <> mp2
            let mp' = mp2 <> mp3
            let d = deltaM mp mp'
            let mp'' = stepM mp' d
            return $ mp `shouldBe` mp''
