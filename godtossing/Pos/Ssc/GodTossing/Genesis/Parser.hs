{-# LANGUAGE TemplateHaskell #-}

module Pos.Ssc.GodTossing.Genesis.Parser
       ( compileGenGtData
       ) where

import           Universum

import qualified Data.ByteString.Lazy             as BSL
import           Data.FileEmbed                   (embedFile, makeRelativeToProject)

import           Pos.Binary.Class                 (decodeFull)
import           Pos.Binary.GodTossing.Types      ()
import           Pos.Ssc.GodTossing.Genesis.Types (GenesisGtData (..))

-- | Fetch pre-generated genesis data from /genesis-gt.bin/ in compile time
compileGenGtData :: GenesisGtData
compileGenGtData =
    let file = BSL.fromStrict $(embedFile =<< makeRelativeToProject "genesis-gt.bin")
    in case decodeFull file of
         Left a  -> error $ toText a
         Right d -> if null (ggdVssCertificates d)
                    then error "No VSS certificates in genesis-gt.bin"
                    else d
