{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Explorer.Aeson.ClientTypes
       (
       ) where

import           Universum

import           Data.Aeson.Encoding (unsafeToEncoding)
import           Data.Aeson.TH (defaultOptions, deriveJSON, deriveToJSON)
import           Data.Aeson.Types (ToJSON (..))
import qualified Data.ByteString.Builder as BS (string8)
import           Data.Fixed (showFixed)

import           Pos.Aeson ()
import           Pos.Explorer.Web.ClientTypes (CAda (..), CAddress, CAddressSummary, CAddressType,
                                               CBlockEntry, CBlockSummary, CCoin,
                                               CGenesisAddressInfo, CGenesisSummary, CHash,
                                               CNetworkAddress, CTxBrief, CTxEntry, CTxId,
                                               CTxSummary)
import           Pos.Explorer.Web.Error (ExplorerError)

deriveJSON defaultOptions ''CHash
deriveJSON defaultOptions ''CAddress
deriveJSON defaultOptions ''CTxId

deriveToJSON defaultOptions ''CCoin
deriveToJSON defaultOptions ''ExplorerError
deriveToJSON defaultOptions ''CBlockEntry
deriveToJSON defaultOptions ''CTxEntry
deriveToJSON defaultOptions ''CTxBrief
deriveToJSON defaultOptions ''CAddressType
deriveToJSON defaultOptions ''CAddressSummary
deriveToJSON defaultOptions ''CBlockSummary
deriveToJSON defaultOptions ''CNetworkAddress
deriveToJSON defaultOptions ''CTxSummary
deriveToJSON defaultOptions ''CGenesisSummary
deriveToJSON defaultOptions ''CGenesisAddressInfo

instance ToJSON CAda where
    -- https://github.com/bos/aeson/issues/227#issuecomment-245400284
    toEncoding (CAda ada) =
        showFixed True ada & -- convert Micro to String chopping off trailing zeros
        BS.string8 &         -- convert String to ByteString using Latin1 encoding
        unsafeToEncoding     -- convert ByteString to Aeson's Encoding
