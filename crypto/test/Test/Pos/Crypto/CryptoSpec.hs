-- | Pos.Crypto specification

module Test.Pos.Crypto.CryptoSpec
       ( spec
       ) where

import qualified Data.ByteString as BS
import           Formatting (sformat)

import           Test.Hspec (Expectation, Spec, describe, it, shouldBe, specify)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Property,
                                  (==>))

import           Universum

import           Pos.Binary.Class (Bi)

import qualified Pos.Crypto as Crypto


{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

spec :: Spec
spec = do
    randomSpec
    hashingSpec

randomSpec :: Spec
randomSpec = describe "Crypto" $ do
    describe "Random" $ do
        -- Let's protect ourselves against *accidental* random gen changes
        -- (e.g. if binary or cryptonite or some other package decide to
        -- behave differently in a new version)
        describe "random number determinism" $ do
            let seed = BS.pack [1..40]
            specify "[0,1)" $
                Crypto.deterministic seed (Crypto.randomNumber 1) `shouldBe` 0
            -- specify "[0,2)" $
            --     deterministic seed (randomNumber 2) `shouldBe` 1
            -- specify "[0,1000)" $
            --     deterministic seed (randomNumber 1000) `shouldBe` 327

hashingSpec :: Spec
hashingSpec = describe "Crypto" $ do
    describe "Hashing" $ do
        describe "hashes of different values are different" $ do
            prop "Bool" (hashInequality @Bool)
            prop "[()]" (hashInequality @[()])
            prop "[[Maybe Integer]]" (hashInequality @[[Maybe Integer]])
        -- Let's protect ourselves against *accidental* hash changes
        describe "check hash sample" $ do
            specify "1 :: Word64" $
                checkHash (1 :: Word64)
                    -- "009d179ba955ae9b0690b8f6a96a866972b1606d97b0c9d8094073a374de77b7612d4ae35ac3e38f4092aced0f1680295a0bc95722ad039253ee6aa275569848" -- Blake2b_512
                    -- "c43b29d95a3585cb5264b3223d70e853f899a82e01cb3e62b0bdd871" -- Blake2s_224
                    -- "4bd3a3255713f33d6c673f7d84048a7a8bcfc206464c85555c603ef4d72189c6" -- Blake2s_256
                    --"12dd0a6a7d0e222a97926da03adb5a7768d31cc7c5c2bd6828e14a7d25fa3a60" -- Blake2b_256, before switching to different endianness
                    --"1dbd7d0b561a41d23c2a469ad42fbd70d5438bae826f6fd607413190c37c363b" -- Before switching to CBOR
                    "ee155ace9c40292074cb6aff8c9ccdd273c81648ff1149ef36bcea6ebb8a3e25"
        describe "decodeAbstractHash" $ do
            it "decodes hash formatted using 'hashHexF'" $ do
                let someHash = Crypto.hash (True, (), False, ())
                Crypto.decodeAbstractHash (sformat Crypto.hashHexF someHash)
                    == Right someHash

hashInequality :: (Eq a, Bi a) => a -> a -> Property
hashInequality a b = a /= b ==> Crypto.hash a /= Crypto.hash b

checkHash :: Bi a => a -> Text -> Expectation
checkHash x s = sformat Crypto.hashHexF (Crypto.hash x) `shouldBe` s
