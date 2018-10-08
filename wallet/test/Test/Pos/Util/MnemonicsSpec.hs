module Test.Pos.Util.MnemonicsSpec (spec, Entropy(..)) where

import           Universum

import           Data.ByteString.Char8 (pack)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Test.Hspec (Spec, describe, it, runIO, shouldSatisfy, xit)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Arbitrary (..), arbitrary, forAll, generate, property)
import           Test.QuickCheck.Gen (oneof, vectorOf)

import           Pos.Core.NetworkMagic (NetworkMagic, makeNetworkMagic)
import           Pos.Crypto (RequiresNetworkMagic (..))
import           Pos.Util.BackupPhrase (BackupPhrase (..), safeKeysFromPhrase)
import           Pos.Util.Mnemonics (defMnemonic, fromMnemonic, toMnemonic)
import           Pos.Wallet.Web.ClientTypes.Functions (encToCId)
import           Pos.Wallet.Web.ClientTypes.Types (CId)

import           Test.Pos.Crypto.Arbitrary (genProtocolMagicUniformWithRNM)

-- We run the tests this number of times, with different `ProtocolMagics`, to get increased
-- coverage. We should really do this inside of the `prop`, but it is difficult to do that
-- without significant rewriting of the testsuite.
testMultiple :: Int
testMultiple = 1

spec :: Spec
spec = do
    runWithMagic RequiresNoMagic
    runWithMagic RequiresMagic

runWithMagic :: RequiresNetworkMagic -> Spec
runWithMagic rnm = replicateM_ testMultiple $
    modifyMaxSuccess (`div` testMultiple) $ do
        pm <- runIO (generate (genProtocolMagicUniformWithRNM rnm))
        describe ("(requiresNetworkMagic=" ++ show rnm ++ ")") $
            specBody (makeNetworkMagic pm)

specBody :: NetworkMagic -> Spec
specBody nm = do
    it "No example mnemonic" $
        fromMnemonic defMnemonic `shouldSatisfy` isLeft

    it "No empty mnemonic" $
        (fromMnemonic "") `shouldSatisfy` isLeft

    it "No empty entropy" $
        (toMnemonic "") `shouldSatisfy` isLeft

    modifyMaxSuccess (const 10000) $ prop "toMnemonic >=> fromMnemonic = Right" $
        \(Entropy ent) -> (toMnemonic ent >>= fromMnemonic) == Right ent

    -- Turn xit -> it to run, and go get a looooong coffee.
    xit "entropyToWalletId is injective, (very long to run, used for investigation)"
        $ property
        $ forAll (vectorOf 1000 arbitrary)
        $ \inputs -> length (inject entropyToWalletId inputs) == length inputs
      where
        inject :: Ord b => (a -> b) -> [a] -> Set b
        inject fn =
            Set.fromList . fmap fn

        entropyToWalletId :: Entropy -> CId w
        entropyToWalletId (Entropy ent) = cid
          where
            backupPhrase = either
                (error . (<>) "Wrong arbitrary Entropy generated: " . show)
                (BackupPhrase . words)
                (toMnemonic ent)

            cid = either
                (error . (<>) "Couldn't create keys from generated BackupPhrase" . show)
                (encToCId nm . fst)
                (safeKeysFromPhrase mempty backupPhrase)


newtype Entropy = Entropy ByteString deriving (Eq, Show)

-- | Initial seed has to be vector or length multiple of 4 bytes and shorter
-- than 64 bytes.
instance Arbitrary Entropy where
    arbitrary =
        Entropy . pack <$> oneof [ vectorOf (4 * n) arbitrary | n <- [1..16] ]
