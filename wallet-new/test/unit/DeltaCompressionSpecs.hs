module DeltaCompressionSpecs (spec) where

import           Universum

import           Data.Map as M
import qualified Data.SafeCopy as SC
import           Data.Serialize (runGet, runPut)

import           Test.Hspec
import           Test.QuickCheck (arbitrary, resize)
import           Test.QuickCheck.Monadic (monadicIO, pick)

import           Cardano.Wallet.Kernel.DB.Compression
import           Cardano.Wallet.Kernel.DB.Spec
import           Cardano.Wallet.Kernel.DB.Spec.Pending
import           Cardano.Wallet.Kernel.Util

import           Util.Buildable (ShowThroughBuild (..))

import           Arbitrary ()

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

spec :: Spec
spec = do
    let pick' a = do
            un <- pick $ STB <$> a
            return $ unSTB un
    describe "roundtrips for deltas -> steps" $ do
        it "Map round trips" $ monadicIO $ do
            (mp :: Map Int Int) <- pick arbitrary
            mp' <- pick arbitrary
            let d = deltaMap mp mp'
            let mp'' = stepMap mp' d
            return $ mp `shouldBe` mp''

        it "Map round trips, (one Map is strictly bigger)" $ monadicIO $ do
            (mp1 :: Map Int Int) <- pick arbitrary
            mp2 <- pick arbitrary
            let mp = mp1 <> mp2
            let mp' = mp1
            let d = deltaMap mp mp'
            let mp'' = stepMap mp' d
            return $ mp'' `shouldBe` mp

        it "Map round trips, (Maps have intersection and no one is strictly bigger)" $ monadicIO $ do
            (mp1 :: Map Int Int) <- pick arbitrary
            mp2 <- pick arbitrary
            mp3 <- pick arbitrary
            let mp = mp1 <> mp2
            let mp' = mp2 <> mp3
            let d = deltaMap mp mp'
            let mp'' = stepMap mp' d
            return $ mp'' `shouldBe` mp

        it "Map round trips, (Maps have no intersection)" $ monadicIO $ do
            (mp :: Map Int Int) <- pick arbitrary
            mp1 <- pick arbitrary
            let mp' = mp1 M.\\ mp
            let d = deltaMap mp mp'
            let mp'' = stepMap mp' d
            return $ mp'' `shouldBe` mp

        it "Pending round trips" $ monadicIO $ do
            p <- pick' $ resize 10 arbitrary
            p' <- pick' $ resize 10 arbitrary
            let d = deltaPending p p'
            let p'' = stepPending p' d
            return $ (STB p'') `shouldBe` (STB p)

        it "InDb Utxo round trips" $ monadicIO $ do
            u <- pick arbitrary
            u' <- pick arbitrary
            let d = deltaUtxo u u'
            let u'' = stepUtxo u' d
            return $ u'' `shouldBe` u

        it "BlockMeta round trips" $ monadicIO $ do
            bm <- pick' arbitrary
            bm' <- pick' arbitrary
            let d = deltaBlockMeta bm bm'
            let bm'' = stepBlockMeta bm' d
            return $ (STB bm'') `shouldBe` (STB bm)

        it "Checkpoint round trips" $ monadicIO $ do
            (c :: Checkpoint) <- pick' $ resize 30 arbitrary
            c' <- pick' $ resize 30 arbitrary
            let d = delta c c'
            let c'' = step c' d
            return $ (STB c'') `shouldBe` (STB c)

        it "Checkpoints round trips" $ monadicIO $ do
            (cs  :: Checkpoints Checkpoint) <- pick' $ resize 10 arbitrary
            -- ^ limit here is important, because
            --   this creates a whole wallet state.
            let d = findDeltas cs
            let cs'' = applyDeltas d
            return $ (STB cs'') `shouldBe` (STB cs)

        it "Safecopy Checkpoints round trips" $ monadicIO $ do
            (cs :: Checkpoints Checkpoint) <- pick' $ resize 10 arbitrary
            let ret = runGet SC.safeGet (runPut (SC.safePut cs))
            return $ (STB <$> ret) `shouldBe` (Right (STB cs))

        it "PartialCheckpoint round trips" $ monadicIO $ do
            (c :: PartialCheckpoint) <- pick' arbitrary
            c' <- pick' $ resize 30 arbitrary
            let d = delta c c'
            let c'' = step c' d
            return $ (STB c'') `shouldBe` (STB c)

        it "PartialCheckpoints round trips" $ monadicIO $ do
            (cs  :: Checkpoints PartialCheckpoint) <- pick' $ resize 10 arbitrary
            -- ^ limit here is important, because
            --   this creates a whole wallet state.
            let d = findDeltas cs
            let cs'' = applyDeltas d
            return $ (STB cs'') `shouldBe` (STB cs)

        it "Safecopy PartialCheckpoints round trips" $ monadicIO $ do
            (cs :: Checkpoints PartialCheckpoint) <- pick' $ resize 10 arbitrary
            let ret = runGet SC.safeGet (runPut (SC.safePut cs))
            return $ (STB <$> ret) `shouldBe` (Right (STB cs))
