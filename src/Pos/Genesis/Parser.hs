{-# LANGUAGE TemplateHaskell #-}

-- | Compile-time genesis data parser

module Pos.Genesis.Parser
       ( compileGenData
       ) where

import qualified Data.ByteString.Lazy as BSL
import           Data.FileEmbed       (embedFile, makeRelativeToProject)
import           Universum            hiding (lift)

import           Pos.Binary.Class     (decodeFull)
import           Pos.Binary.Genesis   ()
import           Pos.Genesis.Types    (GenesisData (..))

-- | Fetch pre-generated genesis data from /genesis-general.bin/ in compile
-- time. Doesn't use TH with lift because it's difficult to provide 'Lift'
-- instance to 'GenesisData'
compileGenData :: GenesisData
compileGenData =
    let file = BSL.fromStrict $ $(embedFile =<< makeRelativeToProject "genesis-general.bin")
    in case decodeFull file of
        Left a  -> error $ toText a
        Right d -> if null (gdAddresses d)
                   then error "No addresses in genesis-general.bin"
                   else d
