module Main where

import           Universum

import           Data.ByteString.Char8 (pack)
import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Gen (generate, oneof, vectorOf)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16


-- import           Pos.Util.BackupPhrase (BackupPhrase (..), safeKeysFromPhrase)
-- import           Pos.Util.Mnemonics (defMnemonic, fromMnemonic, toMnemonic)
-- import           Pos.Wallet.Web.ClientTypes.Functions (encToCId)
-- import           Pos.Wallet.Web.ClientTypes.Types (CId)

main :: IO ()
main = do
    seeds <-
        map (Base16.encode . getEntropy) <$> generate (vectorOf 1000000 arbitrary)

    putText $ show $ length seeds

    BS.writeFile "TEST_SEEDS" (BS.intercalate "\n" seeds)


newtype Entropy = Entropy { getEntropy :: ByteString } deriving (Eq, Show)

instance Arbitrary Entropy where
    arbitrary =
        Entropy . pack <$> oneof [ vectorOf (4 * n) arbitrary | n <- [1..8] ]
