{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
-- | Module providing restoring from backup phrase functionality

module Pos.Util.BackupPhrase
       ( BackupPhrase(..)
       , MnemonicType(..)
       , BackupPhrasePaperVend
       , BackupPhraseNormal
       , toSeed
       , keysFromPhrase
       , safeKeysFromPhrase
       ) where

import qualified Prelude
import           Universum

import           Crypto.Hash (Blake2b_256)
import qualified Data.ByteString as BS
import           Data.Text.Buildable (Buildable (..))
import           Test.QuickCheck (Arbitrary (..), Gen, arbitrary, vectorOf)
import           Test.QuickCheck.Instances ()

import           Pos.Binary (Bi (..), serialize')
import           Pos.Crypto (AbstractHash, EncryptedSecretKey, PassPhrase, SecretKey, VssKeyPair,
                             deterministicKeyGen, deterministicVssKeyGen, safeDeterministicKeyGen,
                             unsafeAbstractHash)
import           Pos.Util.Mnemonics (fromMnemonic, toMnemonic)

data MnemonicType
    = PaperVendMnemonic -- 9 word mnemonic used for paper vend redemption
    | BackupMnemonic    -- 12 word mnemonic used as a wallet backup
    deriving (Bounded, Enum)

-- | Datatype to contain a valid backup phrase
newtype BackupPhrase (a :: MnemonicType) = BackupPhrase
    { bpToList :: [Text]
    } deriving (Eq, Generic)

type BackupPhrasePaperVend = BackupPhrase 'PaperVendMnemonic
type BackupPhraseNormal = BackupPhrase 'BackupMnemonic

instance Typeable a => Bi (BackupPhrase a) where
    encode = encode . bpToList
    decode = BackupPhrase <$> decode

arbitraryMnemonic :: Int -> Gen (Either Text (BackupPhrase a))
arbitraryMnemonic len = do
    eitherMnemonic <- toMnemonic . BS.pack <$> vectorOf len arbitrary
    pure . first toText $ BackupPhrase . words <$> eitherMnemonic

-- NOTE: it's guaranteed not to fail
instance Arbitrary BackupPhrasePaperVend where
    arbitrary = either error identity <$> arbitraryMnemonic 96

-- NOTE: it's guaranteed not to fail
instance Arbitrary BackupPhraseNormal where
    arbitrary = either error identity <$> arbitraryMnemonic 128

instance Show (BackupPhrase a) where
    show _ = "<backup phrase>"

instance Buildable (BackupPhrase a) where
    build _ = "<backup phrase>"

instance Read (BackupPhrase a) where
    readsPrec _ str = either fail (pure . (, mempty) .BackupPhrase . words) $ toMnemonic =<< fromMnemonic (toText str)

toSeed :: BackupPhrase a -> Either Text ByteString
toSeed = first toText . fromMnemonic . unwords . bpToList

toHashSeed :: BackupPhraseNormal -> Either Text ByteString
toHashSeed bp = serialize' . blake2b <$> toSeed bp
  where blake2b :: Bi a => a -> AbstractHash Blake2b_256 ()
        blake2b = unsafeAbstractHash

keysFromPhrase :: BackupPhraseNormal -> Either Text (SecretKey, VssKeyPair)
keysFromPhrase ph = (,) <$> sk <*> vss
  where hashSeed = toHashSeed ph
        sk = snd . deterministicKeyGen <$> hashSeed
        vss = deterministicVssKeyGen <$> hashSeed

safeKeysFromPhrase
    :: PassPhrase
    -> BackupPhraseNormal
    -> Either Text (EncryptedSecretKey, VssKeyPair)
safeKeysFromPhrase pp ph = (,) <$> esk <*> vss
  where hashSeed = toHashSeed ph
        esk = snd . flip safeDeterministicKeyGen pp <$> hashSeed
        vss = deterministicVssKeyGen <$> hashSeed
