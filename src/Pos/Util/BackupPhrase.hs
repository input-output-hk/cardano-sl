-- | Module providing restoring from backup phrase functionality

module Pos.Util.BackupPhrase
       ( BackupPhrase
       , mkBackupPhrase
       , backupPhraseWordsNum
       , bpToList
       , toSeed
       , keysFromPhrase
       ) where

import           Crypto.Hash         (SHA3_256)
import           Data.Char           (isAlpha, isSpace)
import           Data.List           (span, (!!))
import           Data.Maybe          (maybeToList)
import qualified Data.Text           as T
import           Data.Text.Buildable (Buildable (..))
import           Prelude             (readsPrec, show)
import           Universum           hiding (show)

import           Pos.Binary          (Bi, encodeStrict)
import           Pos.Crypto          (AbstractHash, SecretKey, VssKeyPair,
                                      deterministicKeyGen, deterministicVssKeyGen,
                                      unsafeAbstractHash)

-- | Datatype to contain a valid backup phrase
newtype BackupPhrase = BackupPhrase
    { bpToList :: [Text]
    } deriving (Eq, Generic)

-- | Number of words in backup phrase
backupPhraseWordsNum :: Int
backupPhraseWordsNum = 12

-- | Number of hashing rounds to get seed from phrase
hashingRoundsNum :: Int
hashingRoundsNum = 10000

-- | Make backup phrase from list
mkBackupPhrase :: [Text] -> BackupPhrase
mkBackupPhrase ls
    | length ls == backupPhraseWordsNum = BackupPhrase ls
    | otherwise = panic "Invalid number of words in backup phrase!"

instance Show BackupPhrase where
    show = T.unpack . T.unwords . bpToList

instance Buildable BackupPhrase where
    build = build . T.unwords . bpToList

instance Read BackupPhrase where
    readsPrec _ =
        maybeToList . fmap (_1 %~ BackupPhrase) . takeW backupPhraseWordsNum
      where
        takeW 0 str = return ([], str)
        takeW n str = do
            let (w, rest) = span isAlpha $ dropWhile isSpace str
            if null w
                then fail "Invalid phrase"
                else over _1 (T.pack w :) <$> takeW (n - 1) rest

toSeed :: BackupPhrase -> ByteString
toSeed bp = encodeStrict $ iterate hash256 (hash256 ph) !! (hashingRoundsNum - 1)
  where ph = T.concat $ bpToList bp
        hash256 :: Bi a => a -> AbstractHash SHA3_256 b
        hash256 = unsafeAbstractHash

keysFromPhrase :: BackupPhrase -> (SecretKey, VssKeyPair)
keysFromPhrase ph = (sk, vss)
  where seed = toSeed ph
        panicMsg = "Pos.Util.BackupPhrase: impossible: seed is always 32-bit"
        sk = snd $ maybe (panic panicMsg) identity $ deterministicKeyGen seed
        vss = deterministicVssKeyGen seed
