{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Ssc.Util
       ( toSscBlock
       ) where

import           Universum

import           Pos.Binary.Block    ()
import           Pos.Block.Core      (Block, GenesisBlock, MainBlock, mbSscPayload)
import           Pos.Core            (IsGenesisHeader, IsMainHeader, gbBody, gbHeader)
import           Pos.Ssc.Class.Types (Ssc, SscBlock, SscPayload)
import           Pos.Util            (Some (..))

-- [CSL-1156] Totally need something more elegant
toSscBlock
    :: forall ssc.
       Ssc ssc
    => Block ssc -> SscBlock ssc
toSscBlock = bimap convertGenesis convertMain
  where
    convertGenesis :: GenesisBlock ssc -> Some IsGenesisHeader
    convertGenesis = Some . view gbHeader
    convertMain :: MainBlock ssc -> (Some IsMainHeader, SscPayload ssc)
    convertMain blk = (Some $ blk ^. gbHeader, blk ^. gbBody . mbSscPayload)
