
-- | Compile-time genesis data parser

module Pos.Core.Genesis.Parser
       ( compileGenCoreData
       ) where

import           Data.FileEmbed          (embedFile, makeRelativeToProject)
import           Universum               hiding (lift)

import           Pos.Binary.Class        (decodeFull)
import           Pos.Binary.Core.Genesis ()
import           Pos.Core.Constants      (genesisBinSuffix)
import           Pos.Core.Genesis.Types  (GenesisCoreData (..))

-- | Fetch pre-generated genesis data from /genesis-core.bin/ in compile
-- time. Doesn't use TH with lift because it's difficult to provide 'Lift'
-- instance to 'GenesisCoreData'
compileGenCoreData :: GenesisCoreData
compileGenCoreData =
    let file = $(embedFile =<< makeRelativeToProject ("genesis-core-" <> genesisBinSuffix <> ".bin"))
    in case decodeFull file of
        Left a                        ->
            error $ "Failed to read genesis: " <> toText a
        Right d -> d
