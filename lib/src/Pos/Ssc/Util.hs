module Pos.Ssc.Util
       ( toSscBlock
       ) where

import           Universum

import           Pos.Binary.Block ()
import           Pos.Block.Core   (Block, GenesisBlock, MainBlock, mbSscPayload)
import           Pos.Core         (HasConfiguration, IsGenesisHeader, IsMainHeader,
                                   gbBody, gbHeader)
import           Pos.Core.Ssc     (SscPayload)
import           Pos.Ssc.Types    (SscBlock (..))
import           Pos.Util         (Some (..))

-- [CSL-1156] Totally need something more elegant
toSscBlock
    :: HasConfiguration
    => Block -> SscBlock
toSscBlock = SscBlock . bimap convertGenesis convertMain
  where
    convertGenesis :: GenesisBlock -> Some IsGenesisHeader
    convertGenesis = Some . view gbHeader
    convertMain :: MainBlock -> (Some IsMainHeader, SscPayload)
    convertMain blk = (Some $ blk ^. gbHeader, blk ^. gbBody . mbSscPayload)
