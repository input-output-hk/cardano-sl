{-# LANGUAGE RankNTypes #-}
module DeltaCompressionSpecs (spec) where


import           Universum hiding(take)

import           Data.Map as M hiding(take)
import qualified Data.SafeCopy as SC
import           Data.Serialize (runGet, runPut)
import           Formatting (bprint)
import           Formatting.Buildable (build)

import           Test.Hspec
import           Test.QuickCheck (Arbitrary, arbitrary, resize)
import           Test.QuickCheck.Monadic (monadicIO, pick)

import           Cardano.Wallet.Kernel.DB.Checkpoints

import           Test.Pos.Core.Arbitrary ()

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
            return $ mp'' `shouldBe` mp

        it "Map round trips, (Maps have intersection and no one is strictly bigger)" $ monadicIO $ do
            (mp1 :: Map Int Int) <- unMap <$> pick arbitrary
            mp2 <- unMap <$> pick arbitrary
            mp3 <- unMap <$> pick arbitrary
            let mp = mp1 <> mp2
            let mp' = mp2 <> mp3
            let d = deltaM mp mp'
            let mp'' = stepM mp' d
            return $ mp'' `shouldBe` mp

        it "Map round trips, (Maps have no intersection)" $ monadicIO $ do
            (mp :: Map Int Int) <- pick arbitrary
            mp1 <- pick arbitrary
            let mp' = mp1 M.\\ mp
            let d = deltaM mp mp'
            let mp'' = stepM mp' d
            return $ mp'' `shouldBe` mp

        it "Pending round trips" $ monadicIO $ do
            p <- pick arbitrary
            p' <- pick arbitrary
            let d = deltaPending p p'
            let p'' = stepPending p' d
            return $ p'' `shouldBe` p

        it "InDb Utxo round trips" $ monadicIO $ do
            u <- pick arbitrary
            u' <- pick arbitrary
            let d = deltaUtxo u u'
            let u'' = stepUtxo u' d
            return $ u'' `shouldBe` u

        it "BlockMeta round trips" $ monadicIO $ do
            bm <- pick arbitrary
            bm' <- pick arbitrary
            let d = deltaBlockMeta bm bm'
            let bm'' = stepBlockMeta bm' d
            return $ bm'' `shouldBe` bm

        it "Checkpoint round trips" $ monadicIO $ do
            c <- pick arbitrary
            c' <- pick arbitrary
            let d = deltaC c c'
            let c'' = stepC c' d
            return $ c'' `shouldBe` c

        it "Checkpoints round trips" $ monadicIO $ do
            cs <- pick $ resize 10 arbitrary -- limit here is important, because
                                             -- this creates a whole wallet state.
            let d = deltas cs
            let cs'' = steps d
            return $ cs'' `shouldBe` cs

        it "Safecopy Checkpoints round trips" $ monadicIO $ do
            (cs :: Checkpoints) <- pick $ resize 10 arbitrary
            let ret = runGet SC.safeGet (runPut (SC.safePut cs))
            return $ ret `shouldBe` (Right cs)
