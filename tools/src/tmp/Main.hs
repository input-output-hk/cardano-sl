module Main where

import           Universum

import           Data.ByteString.Char8 (pack)
import           Pos.Util.Mnemonics (toMnemonic)
import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Gen (oneof, vectorOf)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as B8


-- import           Pos.Util.BackupPhrase (BackupPhrase (..), safeKeysFromPhrase)
-- import           Pos.Wallet.Web.ClientTypes.Functions (encToCId)
-- import           Pos.Wallet.Web.ClientTypes.Types (CId)

main :: IO ()
main = do
    mnemonics <-
        map seedToMnemonic . B8.split '\n' <$> BS.readFile "TEST_SEEDS"

    BS.writeFile "TEST_MNEMONICS" (BS.intercalate "\n" mnemonics)
  where
    seedToMnemonic =
        encodeUtf8 . orFail . toMnemonic . fst . Base16.decode

    orFail =
        either (error . show) identity

newtype Entropy = Entropy { getEntropy :: ByteString } deriving (Eq, Show)

instance Arbitrary Entropy where
    arbitrary =
        Entropy . pack <$> oneof [ vectorOf (4 * n) arbitrary | n <- [1..16] ]
