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
import           Data.Text.Buildable (Buildable (..))
import           Test.QuickCheck (Arbitrary (..), elements, genericShrink, vectorOf)
import           Test.QuickCheck.Instances ()

import           Pos.Binary (Bi (..), serialize')
import           Pos.Crypto (AbstractHash, EncryptedSecretKey, PassPhrase, SecretKey, VssKeyPair,
                             deterministicKeyGen, deterministicVssKeyGen, safeDeterministicKeyGen,
                             unsafeAbstractHash)
import           Pos.Util.Mnemonics (fromMnemonic, toMnemonic)

-- | Datatype to contain a valid backup phrase
newtype BackupPhrase = BackupPhrase
    { bpToList :: [Text]
    } deriving (Eq, Generic)

instance Arbitrary BackupPhrase where
    arbitrary = BackupPhrase <$> vectorOf 12 (elements englishWords)
    shrink    = genericShrink

-- | (Some) valid English words as taken from <https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki BIP-39>
englishWords :: [Text]
englishWords = [ "recycle" , "child" , "universe" , "extend" , "edge" , "tourist"
               , "swamp" , "rare" , "enhance" , "rabbit" , "blast" , "plastic" , "attitude"
               , "name" , "skull" , "merit" , "night" , "idle" , "bone" , "exact"
               , "inflict" , "legal" , "predict" , "certain" , "napkin" , "blood"
               , "color" , "screen" , "birth" , "detect" , "summer" , "palm"
               , "entry" , "swing" , "fit" , "garden" , "trick" , "timber"
               , "toss" , "atom" , "kitten" , "flush" , "master" , "transfer"
               , "success" , "worry" , "rural" , "silver" , "invest" , "mean"
               ]

-- | Number of words in backup phrase
backupPhraseWordsNum :: Int
backupPhraseWordsNum = 12

instance Show BackupPhrase where
    show _ = "<backup phrase>"

instance Buildable BackupPhrase where
    build _ = "<backup phrase>"

instance Read BackupPhrase where
    readsPrec _ str = either fail (pure . (, mempty) .BackupPhrase . words) $ toMnemonic =<< fromMnemonic (toText str)

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
