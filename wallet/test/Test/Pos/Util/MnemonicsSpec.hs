module Test.Pos.Util.MnemonicsSpec (spec, Entropy(..)) where

import           Universum

import           Data.ByteString.Char8 (pack)
import           Data.Set (Set)
import           Test.Hspec (Spec, describe, it, shouldSatisfy, xit)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck (Arbitrary (..), property)
import           Test.QuickCheck.Gen (Gen (..), oneof, vectorOf)
import           Test.QuickCheck.Random (mkQCGen)

import           Pos.Util.BackupPhrase (BackupPhrase (..), safeKeysFromPhrase)
import           Pos.Util.Mnemonics (defMnemonic, fromMnemonic, toMnemonic)
import           Pos.Wallet.Web.ClientTypes.Functions (encToCId)
import           Pos.Wallet.Web.ClientTypes.Types (CId)

import qualified Data.Set as Set


spec :: Spec
spec = do
    describe "Pos.Util.Mnemonics" $ do
        modifyMaxSuccess (const 10000) $ it "toMnemonic >=> fromMnemonic = Right" $ property $
            \(Entropy ent) -> (toMnemonic ent >>= fromMnemonic) == Right ent

        it "No example mnemonic" $
            fromMnemonic defMnemonic `shouldSatisfy` isLeft

        it "No empty mnemonic" $
            (fromMnemonic "") `shouldSatisfy` isLeft

        it "No empty entropy" $
            (toMnemonic "") `shouldSatisfy` isLeft

        xit "entropyToWalletId is injective " $
            let
                inject :: Ord b => (a -> b) -> [a] -> Set b
                inject fn xs =
                    foldl' (\acc x -> Set.insert (fn x) acc) Set.empty xs

                genList :: Arbitrary a => Int -> [a]
                genList n =
                    unGen (vectorOf n arbitrary) (mkQCGen 14) 14

                entropyToWalletId :: Entropy -> CId w
                entropyToWalletId (Entropy ent) = cid
                  where
                    Right backupPhrase = (BackupPhrase . words) <$> toMnemonic ent
                    Right cid          = (encToCId . fst) <$> safeKeysFromPhrase mempty backupPhrase

                inputs = genList 1000000
            in
                length (inject entropyToWalletId inputs) == length inputs


newtype Entropy = Entropy ByteString deriving (Eq, Show)

-- | Initial seed has to be vector or length multiple of 4 bytes and shorter
-- than 64 bytes.
instance Arbitrary Entropy where
    arbitrary =
        Entropy . pack <$> oneof [ vectorOf (4 * n) arbitrary | n <- [1..16] ]
