{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Core.Common.StakeholderId
       ( StakeholderId
       ) where

import           Universum

import           Formatting (formatToString)
import           Text.JSON.Canonical (FromObjectKey (..), JSValue (..),
                     ReportSchemaErrors, ToObjectKey (..))

import           Pos.Core.Common.AddressHash
import           Pos.Core.Genesis.Canonical ()
import           Pos.Crypto (decodeAbstractHash, hashHexF)
import           Pos.Crypto.Signing (PublicKey)
import           Pos.Util.Json.Parse (tryParseString)


-- | Stakeholder identifier (stakeholders are identified by their public keys)
type StakeholderId = AddressHash PublicKey

instance Monad m => ToObjectKey m StakeholderId where
    toObjectKey = pure . formatToString hashHexF

instance ReportSchemaErrors m => FromObjectKey m StakeholderId where
    fromObjectKey = fmap Just . tryParseString (decodeAbstractHash) . JSString
