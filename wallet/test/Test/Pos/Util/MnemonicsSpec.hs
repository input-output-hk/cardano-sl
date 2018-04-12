module Test.Pos.Util.MnemonicsSpec (spec, Entropy(..)) where

import           Universum

import           Data.ByteString.Char8 (pack)
import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck (Arbitrary (..), property)
import           Test.QuickCheck.Gen (oneof, vectorOf)

import           Pos.Util.Mnemonics (fromMnemonic, toMnemonic)

spec :: Spec
spec = describe "Pos.Util.Mnemonics" $ modifyMaxSuccess (const 10000) $
    it "toMnemonic >=> fromMnemonic = Right" $ property $
        \(Entropy ent) -> (toMnemonic ent >>= fromMnemonic) == Right ent


newtype Entropy = Entropy ByteString deriving (Eq, Show)

-- | Initial seed has to be vector or length multiple of 4 bytes and shorter
-- than 64 bytes.
instance Arbitrary Entropy where
    arbitrary =
        Entropy . pack <$> oneof [ vectorOf (4 * n) arbitrary | n <- [1..16] ]
