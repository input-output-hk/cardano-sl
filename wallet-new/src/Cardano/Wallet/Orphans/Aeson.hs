{- | Aeson Orphans. |-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cardano.Wallet.Orphans.Aeson where

import           Universum

import           Data.Aeson (FromJSON (..), ToJSON (..))
import           Data.Aeson.Types (Value (Array), typeMismatch)

import           Pos.Util.BackupPhrase (BackupPhrase (..))
import           Pos.Wallet.Web.ClientTypes.Types (CFilePath (..))

instance ToJSON BackupPhrase where
    toJSON (BackupPhrase words) = toJSON words

instance FromJSON BackupPhrase where
    parseJSON (Array words) = BackupPhrase . toList <$> traverse parseJSON words
    parseJSON x             = typeMismatch "parseJSON failed for BackupPhrase" x

instance ToJSON CFilePath where
  toJSON (CFilePath c) = toJSON c
