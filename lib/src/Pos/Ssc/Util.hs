module Pos.Ssc.Util
       ( toSscBlock
       ) where

import           Universum

import           Pos.Binary.Block      ()
import           Pos.Block.Core        (Block, GenesisBlock, MainBlock, mbSscPayload)
import           Pos.Core              (HasConfiguration, IsGenesisHeader, IsMainHeader,
                                        gbBody, gbHeader)
import           Pos.Ssc.Class.Helpers (SscHelpersClass)
import           Pos.Ssc.Class.Types   (SscBlock (..), SscPayload)
import           Pos.Ssc.GodTossing.Type (SscGodTossing)
import           Pos.Util              (Some (..))

-- [CSL-1156] Totally need something more elegant
toSscBlock
    :: forall ssc.
       (HasConfiguration, SscHelpersClass ssc, ssc ~ SscGodTossing)
    => Block -> SscBlock ssc
toSscBlock = SscBlock . bimap convertGenesis convertMain
  where
    convertGenesis :: GenesisBlock -> Some IsGenesisHeader
    convertGenesis = Some . view gbHeader
    convertMain :: MainBlock -> (Some IsMainHeader, SscPayload ssc)
    convertMain blk = (Some $ blk ^. gbHeader, blk ^. gbBody . mbSscPayload)
