{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Definitions of the main blockchain ('Blockchain' class and related).

module Pos.Core.Block.Main.Chain
       (
       ) where

import           Pos.Binary.Class (Bi)
import           Pos.Binary.Core.Delegation ()
import           Pos.Binary.Core.Ssc ()
import           Pos.Binary.Core.Txp ()
import           Pos.Binary.Core.Update ()
import           Pos.Core.Block.Blockchain (Blockchain (..), GenericBlockHeader (..))
import           Pos.Core.Block.Main.Types (MainBlockchain, MainBody (..), MainConsensusData,
                                            MainExtraBodyData, MainExtraHeaderData, MainProof (..))
import           Pos.Core.Block.Union.Types (Block, BlockHeader)
import           Pos.Core.Class (IsMainHeader (..))
import           Pos.Core.Ssc (mkSscProof)
import           Pos.Core.Txp (mkTxProof)
import           Pos.Core.Update (mkUpdateProof)
import           Pos.Crypto (hash)

instance ( Bi BlockHeader
         , Bi (BodyProof MainBlockchain)
         , IsMainHeader (GenericBlockHeader MainBlockchain)) =>
         Blockchain MainBlockchain where

    type BodyProof MainBlockchain = MainProof

    type ConsensusData MainBlockchain = MainConsensusData

    type BBlockHeader MainBlockchain = BlockHeader
    type ExtraHeaderData MainBlockchain = MainExtraHeaderData

    type Body MainBlockchain = MainBody

    type ExtraBodyData MainBlockchain = MainExtraBodyData
    type BBlock MainBlockchain = Block

    mkBodyProof MainBody{..} =
        MainProof
        { mpTxProof = mkTxProof _mbTxPayload
        , mpMpcProof = mkSscProof _mbSscPayload
        , mpProxySKsProof = hash _mbDlgPayload
        , mpUpdateProof = mkUpdateProof _mbUpdatePayload
        }
