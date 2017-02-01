{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Pos.Explorer.Aeson.ClientTypes
       (
       ) where

import           Data.Aeson.TH                (defaultOptions, deriveJSON, deriveToJSON)
import           Pos.Aeson                    ()
import           Pos.Explorer.Web.ClientTypes (CAddress, CBlockEntry, CHash, CTxEntry,
                                               CTxId)
import           Pos.Explorer.Web.Error       (ExplorerError)

deriveJSON defaultOptions ''CHash
deriveJSON defaultOptions ''CAddress
deriveJSON defaultOptions ''CTxId

deriveToJSON defaultOptions ''ExplorerError
deriveToJSON defaultOptions ''CBlockEntry
deriveToJSON defaultOptions ''CTxEntry
