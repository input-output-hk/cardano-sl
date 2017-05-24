{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Definition of 'BlockchainHelpers' for the main blockchain.

module Pos.Block.Core.Main.Helpers
       (
       ) where

import           Universum

import           Control.Monad.Except       (MonadError (throwError))

import           Pos.Binary.Block.Core      ()
import           Pos.Binary.Class           (Bi)
import           Pos.Binary.Core            ()
import           Pos.Binary.Txp             ()
import           Pos.Binary.Update          ()
import           Pos.Block.Core.Main.Chain  (Body (..), ConsensusData (..))
import           Pos.Block.Core.Main.Types  (MainBlockHeader, MainBlockchain,
                                             MainToSign (..))
import           Pos.Block.Core.Union.Types (BiHeader, BlockSignature (..))
import           Pos.Core                   (Blockchain (..), BlockchainHelpers (..),
                                             GenericBlock (..), GenericBlockHeader (..),
                                             IsMainHeader (..), SlotId (..))
import           Pos.Crypto                 (SignTag (..), checkSig, proxyVerify)
import           Pos.Ssc.Class.Helpers      (SscHelpersClass (..))
import           Pos.Ssc.Class.Types        (Ssc (..))
import           Pos.Util.Util              (Some (Some))

instance ( BiHeader ssc
         , SscHelpersClass ssc
         , IsMainHeader (GenericBlockHeader $ MainBlockchain ssc)) =>
         BlockchainHelpers (MainBlockchain ssc) where
    verifyBBlockHeader = verifyMainBlockHeader
    verifyBBlock UnsafeGenericBlock {..} =
        either (throwError . pretty) pure $
        sscVerifyPayload @ssc (Right (Some _gbHeader)) (_mbSscPayload _gbBody)

verifyMainBlockHeader ::
       (Ssc ssc, MonadError Text m, Bi $ BodyProof $ MainBlockchain ssc)
    => MainBlockHeader ssc
    -> m ()
verifyMainBlockHeader UnsafeGenericBlockHeader { _gbhConsensus = MainConsensusData { _mcdLeaderKey = leaderPk
                                                                                   , _mcdSlot = slotId
                                                                                   , _mcdDifficulty = difficulty
                                                                                   , ..
                                                                                   }
                                               , _gbhExtra = extra
                                               , ..
                                               } =
    unless (verifyBlockSignature _mcdSignature) $
    throwError "can't verify signature"
  where
    verifyBlockSignature (BlockSignature sig) =
        checkSig SignMainBlock leaderPk signature sig
    verifyBlockSignature (BlockPSignatureLight proxySig) =
        proxyVerify
            SignMainBlockLight
            leaderPk
            proxySig
            (\(epochLow, epochHigh) ->
                 epochLow <= epochId && epochId <= epochHigh)
            signature
    verifyBlockSignature (BlockPSignatureHeavy proxySig) =
        proxyVerify SignMainBlockHeavy leaderPk proxySig (const True) signature
    signature = MainToSign _gbhPrevBlock _gbhBodyProof slotId difficulty extra
    epochId = siEpoch slotId
