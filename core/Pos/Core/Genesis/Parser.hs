
-- | Compile-time genesis data parser

module Pos.Core.Genesis.Parser
       ( compileGenSpec
       ) where

import           Universum               hiding (lift)

import           Data.FileEmbed          (embedFile, makeRelativeToProject)
import           Text.JSON.Canonical     (fromJSON, parseCanonicalJSON)
import qualified Data.ByteString.Lazy    as BSL

import           Pos.Binary.Core.Genesis ()
import           Pos.Core.Constants      (genesisBinSuffix)
import           Pos.Core.Genesis.Types  (GenesisSpec (..))

-- | Fetch pre-generated genesis data from /genesis-core.bin/ in compile
-- time. Doesn't use TH with lift because it's difficult to provide 'Lift'
-- instance to 'GenesisCoreData'
compileGenSpec :: GenesisSpec
compileGenSpec =
    --let file = $(embedFile =<< makeRelativeToProject ("init-data-" <> genesisBinSuffix <> ".json"))
    let file = $(embedFile =<< makeRelativeToProject ("genesis-core-" <> genesisBinSuffix <> ".bin"))
    in case parseCanonicalJSON (BSL.fromStrict file) of
        Left a                        ->
            error $ "Failed to read genesis: " <> toText a
        Right jsValue ->
            undefined
            --fromJSON @_ @GenesisSpec jsValue
