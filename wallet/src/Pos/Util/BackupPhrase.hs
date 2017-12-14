{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
-- | Module providing restoring from backup phrase functionality

module Pos.Util.BackupPhrase
       ( BackupPhrase(..)
       , MnemonicType(..)
       , BackupPhrasePaperVend
       , BackupPhraseNormal
       , backupPhraseWordsNum
       , toSeed
       , keysFromPhrase
       , safeKeysFromPhrase
       ) where

import qualified Prelude
import           Universum

import           Crypto.Hash (Blake2b_256)
import           Data.Text.Buildable (Buildable (..))
import           Test.QuickCheck (Arbitrary (..), elements, vectorOf)
import           Test.QuickCheck.Instances ()

import           Pos.Binary (Bi (..), serialize')
import           Pos.Crypto (AbstractHash, EncryptedSecretKey, PassPhrase, SecretKey, VssKeyPair,
                             deterministicKeyGen, deterministicVssKeyGen, safeDeterministicKeyGen,
                             unsafeAbstractHash)
import           Pos.Util.Mnemonics (fromMnemonic, toMnemonic)

data MnemonicType
    = PaperVendMnemonic -- 9 word mnemonic used for paper vend redemption
    | BackupMnemonic    -- 12 word mnemonic used as a wallet backup

-- | Datatype to contain a valid backup phrase
newtype BackupPhrase (a :: MnemonicType) = BackupPhrase
    { bpToList :: [Text]
    } deriving (Eq, Generic)

type BackupPhrasePaperVend = BackupPhrase 'PaperVendMnemonic
type BackupPhraseNormal = BackupPhrase 'BackupMnemonic

instance Typeable a => Bi (BackupPhrase a) where
    encode = encode . bpToList
    decode = BackupPhrase <$> decode

-- FIXME(akegalj): Fix mnemonic generation to be bip39 compatible
instance Arbitrary BackupPhrasePaperVend where
    arbitrary = BackupPhrase <$> vectorOf 9 (elements englishWords)

-- FIXME(akegalj): Fix mnemonic generation to be bip39 compatible
instance Arbitrary BackupPhrasePaperVend where
instance Arbitrary BackupPhraseNormal where
    arbitrary = BackupPhrase <$> vectorOf 12 (elements englishWords)

-- | (Some) valid English words as taken from <https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki BIP-39>
englishWords :: [Text]
englishWords = [ "recycle" , "child" , "universe" , "extend" , "edge" , "tourist"
               , "swamp" , "rare" , "enhance" , "rabbit" , "blast" , "plastic" , "attitude"
               , "name" , "skull" , "merit" , "night" , "idle" , "bone" , "exact"
               , "inflict" , "legal" , "predict" , "certain" , "napkin" , "blood"
               , "color" , "screen" , "birth" , "detect" , "summer" , "palm"
               , "entry" , "swing" , "fit" , "garden" , "trick" , "timber"
               , "toss" , "atom" , "kitten" , "flush" , "master" , "transfer"
               , "success" , "worry" , "rural" , "silver" , "invest" , "mean "
               ]

-- | Number of words in backup phrase
backupPhraseWordsNum :: Int
backupPhraseWordsNum = 12

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
