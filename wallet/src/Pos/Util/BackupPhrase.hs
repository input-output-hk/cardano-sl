-- | Module providing restoring from backup phrase functionality

module Pos.Util.BackupPhrase
       ( BackupPhrase(..)
       , backupPhraseWordsNum
       , toSeed
       , keysFromPhrase
       , safeKeysFromPhrase
       ) where

import qualified Prelude
import           Universum

import           Crypto.Hash (Blake2b_256)
import qualified Data.ByteString as BS
import           Data.Text.Buildable (Buildable (..))
import           Test.QuickCheck (Arbitrary (..), Gen, genericShrink, vectorOf)
import           Test.QuickCheck.Instances ()

import           Pos.Binary (Bi (..), serialize')
import           Pos.Crypto (AbstractHash, EncryptedSecretKey, PassPhrase, SecretKey, VssKeyPair,
                             deterministicKeyGen, deterministicVssKeyGen, safeDeterministicKeyGen,
                             unsafeAbstractHash)
import           Pos.Util.LogSafe (SecureLog)
import           Pos.Util.Mnemonics (fromMnemonic, toMnemonic)

-- | Datatype to contain a valid backup phrase
newtype BackupPhrase = BackupPhrase
    { bpToList :: [Text]
    } deriving (Eq, Generic)

instance Arbitrary BackupPhrase where
    arbitrary = do
        em <- arbitraryMnemonic 4
        case em of
            Left _  -> arbitrary
            Right a -> pure a
    shrink    = genericShrink

-- | Generate an arbitrary mnemonic with the given number of words.
arbitraryMnemonic :: Int -> Gen (Either Text BackupPhrase)
arbitraryMnemonic len = do
    let byteCount = len * 4
    eitherMnemonic <- toMnemonic . BS.pack <$> vectorOf byteCount arbitrary
    pure . first toText $ BackupPhrase . words <$> eitherMnemonic

-- | Number of words in backup phrase
backupPhraseWordsNum :: Int
backupPhraseWordsNum = 12

instance Show BackupPhrase where
    show _ = "<backup phrase>"

instance Buildable BackupPhrase where
    build _ = "<backup phrase>"

instance Buildable (SecureLog BackupPhrase) where
    build _ = "<backup phrase>"

instance Read BackupPhrase where
    readsPrec _ str = either (const []) (pure . (, mempty) .BackupPhrase . words) $ toMnemonic =<< fromMnemonic (toText str)

toSeed :: BackupPhrase -> Either Text ByteString
toSeed = first toText . fromMnemonic . unwords . bpToList

toHashSeed :: BackupPhrase -> Either Text ByteString
toHashSeed bp = serialize' . blake2b <$> toSeed bp
  where blake2b :: Bi a => a -> AbstractHash Blake2b_256 ()
        blake2b = unsafeAbstractHash

keysFromPhrase :: BackupPhrase -> Either Text (SecretKey, VssKeyPair)
keysFromPhrase ph = (,) <$> sk <*> vss
  where hashSeed = toHashSeed ph
        sk = snd . deterministicKeyGen <$> hashSeed
        vss = deterministicVssKeyGen <$> hashSeed

safeKeysFromPhrase
    :: PassPhrase
    -> BackupPhrase
    -> Either Text (EncryptedSecretKey, VssKeyPair)
safeKeysFromPhrase pp ph = (,) <$> esk <*> vss
  where hashSeed = toHashSeed ph
        esk = snd . flip safeDeterministicKeyGen pp <$> hashSeed
        vss = deterministicVssKeyGen <$> hashSeed
