{-# LANGUAGE TemplateHaskell #-}

-- | Compile-time genesis data parser

module Pos.Core.Genesis.Parser
       ( compileGenCoreData
       ) where

import qualified Data.ByteString.Lazy    as BSL
import           Data.FileEmbed          (embedFile, makeRelativeToProject)
import           Universum               hiding (lift)

import           Pos.Binary.Class        (decodeFull)
import           Pos.Binary.Core.Genesis ()
import           Pos.Core.Genesis.Types  (GenesisCoreData (..))

-- | Fetch pre-generated genesis data from /genesis-core.bin/ in compile
-- time. Doesn't use TH with lift because it's difficult to provide 'Lift'
-- instance to 'GenesisCoreData'
compileGenCoreData :: GenesisCoreData
compileGenCoreData =
    let file = BSL.fromStrict $ $(embedFile =<< makeRelativeToProject "genesis-core.bin")
    in case decodeFull file of
        Left a  -> error $ toText a
        Right d -> if null (gcdAddresses d)
                   then error "No addresses in genesis-core.bin"
                   else d
