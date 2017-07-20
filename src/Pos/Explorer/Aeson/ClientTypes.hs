{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Pos.Explorer.Aeson.ClientTypes
       (
       ) where

import           Data.Aeson.TH                (defaultOptions, deriveFromJSON, deriveJSON,
                                               deriveToJSON)
import           Pos.Aeson                    ()
import           Pos.Explorer.Web.ClientTypes (CAddress, CAddressSummary, CAddressType,
                                               CBlockEntry, CBlockSummary, CCoin,
                                               CGenesisAddressInfo, CGenesisSummary,
                                               CHash, CNetworkAddress, CTxBrief, CTxEntry,
                                               CTxId, CTxSummary)
import           Pos.Explorer.Web.Error       (ExplorerError)
import           Pos.Types                    (ChainDifficulty)





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

deriveFromJSON defaultOptions ''ChainDifficulty
