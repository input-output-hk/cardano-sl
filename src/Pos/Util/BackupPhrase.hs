-- | Module providing restoring from backup phrase functionality

module Pos.Util.BackupPhrase
       ( BackupPhrase
       , mkBackupPhrase
       , bpToList
       , toSeed
       ) where

import           Control.Lens        (over, (%~), _1)
import qualified Data.ByteString     as BS
import           Data.Char           (isAlpha, isSpace)
import           Data.List           (span, (!!))
import           Data.Maybe          (maybeToList)
import qualified Data.Text           as T
import           Data.Text.Buildable (Buildable (..))
import           Prelude             (readsPrec, show)
import           Universum           hiding (show)

import           Pos.Binary          (encodeStrict)
import           Pos.Crypto          (unsafeHash)

-- | Datatype to contain a valid backup phrase
newtype BackupPhrase = BackupPhrase
    { bpToList :: [Text]
    } deriving (Eq)

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
    show = show . T.unwords . bpToList

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
toSeed bp = encodeStrict $ iterate unsafeHash (unsafeHash ph) !! (hashingRoundsNum - 1)
  where ph = T.concat $ bpToList bp
