{- |
   An orphanage for all the non-interesting orphan instances.
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cardano.Wallet.Orphans where

import           Universum

import           Data.Aeson            (FromJSON (..), ToJSON (..))
import           Data.Aeson.Types      (Value (Array), typeMismatch)

import           Pos.Util.BackupPhrase (BackupPhrase (..))

instance ToJSON BackupPhrase where
    toJSON (BackupPhrase words) = toJSON words

instance FromJSON BackupPhrase where
    parseJSON (Array words) = BackupPhrase . toList <$> traverse parseJSON words
    parseJSON x             = typeMismatch "parseJSON failed for BackupPhrase" x
