{-# LANGUAGE TemplateHaskell #-}

-- | Compile-time genesis data parser

module Pos.Genesis.Parser
       ( compileGenData
       ) where

import           Data.FileEmbed             (embedFile, makeRelativeToProject)
import qualified Data.HashMap.Strict        as HM
import           Language.Haskell.TH.Syntax (lift)
import           Universum                  hiding (lift)

import           Pos.Binary                 (decodeFull)
import           Pos.Genesis.Types          (GenesisData (..))

-- | Fetch pre-generated genesis data from /genesis.bin/ in compile time
compileGenData :: GenesisData
compileGenData =
    $(do let file = $(embedFile =<< makeRelativeToProject "genesis.bin")
         case decodeFull file of
             Left a  -> fail (toString a)
             Right d -> if null (gdAddresses d) || HM.null (gdVssCertificates d)
                        then fail "No addresses or VSS certificates in genesis data"
                        else lift (d :: GenesisData))
