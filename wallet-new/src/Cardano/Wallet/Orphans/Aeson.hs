{- | Aeson Orphans. |-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cardano.Wallet.Orphans.Aeson where

import           Universum hiding (words)

import           Data.Aeson (FromJSON (..), ToJSON (..))
import           Data.Aeson.Types (Value (..), typeMismatch)
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as BS
import           Formatting (int, sformat, (%))

import qualified Serokell.Util.Base16 as Base16

import qualified Pos.Crypto.Signing as Core
import           Pos.Util.BackupPhrase (BackupPhrase (..))
import           Pos.Wallet.Web.ClientTypes.Types (CFilePath (..))

mkPassPhrase :: Text -> Either String Core.PassPhrase
mkPassPhrase text =
    case Base16.decode text of
        Left e -> Left (toString e)
        Right bs -> do
            let bl = BS.length bs
            -- Currently passphrase may be either 32-byte long or empty (for
            -- unencrypted keys).
            if bl == 0 || bl == Core.passphraseLength
                then pure $ ByteArray.convert bs
                else fail $ toString $ sformat
                     ("Expected spending password to be of either length 0 or "%int%", not "%int)
                     Core.passphraseLength bl

instance ToJSON BackupPhrase where
    toJSON (BackupPhrase words) = toJSON words

instance FromJSON BackupPhrase where
    parseJSON (Array words) = BackupPhrase . toList <$> traverse parseJSON words
    parseJSON x             = typeMismatch "parseJSON failed for BackupPhrase" x

instance ToJSON CFilePath where
  toJSON (CFilePath c) = toJSON c

instance ToJSON Core.PassPhrase where
    toJSON = String . Base16.encode . ByteArray.convert

instance FromJSON Core.PassPhrase where
    parseJSON (String pp) = case mkPassPhrase pp of
        Left e    -> fail e
        Right pp' -> pure pp'
    parseJSON x           = typeMismatch "parseJSON failed for PassPhrase" x
