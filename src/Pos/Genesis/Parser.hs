{-# LANGUAGE TemplateHaskell #-}

-- | Compile-time genesis data parser

module Pos.Genesis.Parser
       ( compileGenData
       ) where

import qualified Data.ByteString.Lazy as BSL
import           Data.FileEmbed       (embedFile, makeRelativeToProject)
import qualified Data.HashMap.Strict  as HM
import           Universum            hiding (lift)

import           Pos.Binary.Class     (decodeFull)
import           Pos.Binary.Genesis   ()
import           Pos.Genesis.Types    (GenesisData (..))

-- | Fetch pre-generated genesis data from /genesis.bin/ in compile time
-- Doesn't use TH with lift because it's difficult to provide 'Lift' instance
-- to 'GenesisData'
compileGenData :: GenesisData
compileGenData =
    let file = BSL.fromStrict $ $(embedFile =<< makeRelativeToProject "genesis.bin")
    in case decodeFull file of
        Left a  -> panic $ toText a
        Right d -> if null (gdAddresses d) || HM.null (gdVssCertificates d)
                   then panic "No addresses or VSS certificates in genesis data"
                   else d
