-- | Module providing restoring from backup phrase functionality

module Pos.Util.BackupPhrase
       ( BackupPhrase
       , mkBackupPhrase12
       , mkBackupPhrase9
       , backupPhraseWordsNum
       , bpToList
       , toSeed
       , keysFromPhrase
       , safeKeysFromPhrase
       ) where

import           Data.Text.Buildable (Buildable (..))
import qualified Prelude
import           Universum

import           Crypto.Hash         (Blake2b_256)
import           Pos.Binary          (Bi (..), encodeStrict, label)
import           Pos.Crypto          (AbstractHash, EncryptedSecretKey, PassPhrase,
                                      SecretKey, VssKeyPair, deterministicKeyGen,
                                      deterministicVssKeyGen, safeDeterministicKeyGen,
                                      unsafeAbstractHash)
import           Pos.Util.Mnemonics  (fromMnemonic, toMnemonic)


-- | Datatype to contain a valid backup phrase
newtype BackupPhrase = BackupPhrase
    { bpToList :: [Text]
    } deriving (Eq, Generic)

instance Bi BackupPhrase where
    put BackupPhrase{..} = put bpToList
    get = label "BackupPhrase" $ BackupPhrase <$> get

-- | Number of words in backup phrase
backupPhraseWordsNum :: Int
backupPhraseWordsNum = 12

-- | Make backup phrase from list
mkBackupPhrase12 :: [Text] -> BackupPhrase
mkBackupPhrase12 ls
    | length ls == 12 = BackupPhrase ls
    | otherwise = error "Invalid number of words in backup phrase! Expected 12 words."

-- | Make backup phrase from list
mkBackupPhrase9 :: [Text] -> BackupPhrase
mkBackupPhrase9 ls
    | length ls == 9 = BackupPhrase ls
    | otherwise = error "Invalid number of words in backup phrase! Expected 9 words."

instance Show BackupPhrase where
    show = toString . unwords . bpToList

instance Buildable BackupPhrase where
    build = build . unwords . bpToList

instance Read BackupPhrase where
    readsPrec _ str = either fail (pure . (, mempty) .BackupPhrase . words) $ toMnemonic =<< fromMnemonic (toText str)

toSeed :: BackupPhrase -> Either Text ByteString
toSeed = first toText . fromMnemonic . unwords . bpToList

toHashSeed :: BackupPhrase -> Either Text ByteString
toHashSeed bp = encodeStrict . blake2b <$> toSeed bp
  where blake2b :: Bi a => a -> AbstractHash Blake2b_256 b
        blake2b = unsafeAbstractHash

keysFromPhrase :: BackupPhrase -> Either Text (SecretKey, VssKeyPair)
keysFromPhrase ph = (,) <$> sk <*> vss
  where hashSeed = toHashSeed ph
        errorMsg = "Pos.Util.BackupPhrase: impossible: seed is always 32-bit"
        sk = maybe (Left errorMsg) (Right . snd) . deterministicKeyGen =<< hashSeed
        vss = deterministicVssKeyGen <$> hashSeed

safeKeysFromPhrase
    :: PassPhrase
    -> BackupPhrase
    -> Either Text (EncryptedSecretKey, VssKeyPair)
safeKeysFromPhrase pp ph = (,) <$> esk <*> vss
  where hashSeed = toHashSeed ph
        errorMsg = "Pos.Util.BackupPhrase: impossible: seed is always 32-bit"
        esk = maybe (Left errorMsg) (Right . snd) . flip safeDeterministicKeyGen pp =<< hashSeed
        vss = deterministicVssKeyGen <$> hashSeed
