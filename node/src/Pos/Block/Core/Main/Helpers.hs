{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

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
import           Pos.Block.Core.Main.Lens   (mainBlockEBDataProof)
import           Pos.Block.Core.Main.Types  (MainBlockHeader, MainBlockchain,
                                             MainToSign (..))
import           Pos.Block.Core.Union.Types (BiHeader, BlockSignature (..))
import           Pos.Core                   (Blockchain (..), BlockchainHelpers (..),
                                             GenericBlock (..), GenericBlockHeader (..),
                                             HasConfiguration, IsMainHeader (..),
                                             SlotId (..), epochIndexL, gbExtra)
import           Pos.Crypto                 (ProxySignature (..), SignTag (..), checkSig,
                                             hash, isSelfSignedPsk, proxyVerify)
import           Pos.Delegation.Helpers     (dlgVerifyPayload)
import           Pos.Ssc.Class.Helpers      (SscHelpersClass (..))
import           Pos.Ssc.Class.Types        (Ssc (..))
import           Pos.Util.Util              (Some (Some))

instance ( BiHeader ssc
         , SscHelpersClass ssc
         , HasConfiguration
         , IsMainHeader (GenericBlockHeader $ MainBlockchain ssc)
         ) =>
         BlockchainHelpers (MainBlockchain ssc) where
    verifyBBlockHeader = verifyMainBlockHeader
    verifyBBlock block@UnsafeGenericBlock {..} = do
        either (throwError . pretty) pure $
            sscVerifyPayload @ssc
                (Right (Some _gbHeader))
                (_mbSscPayload _gbBody)
        dlgVerifyPayload (_gbHeader ^. epochIndexL) (_mbDlgPayload _gbBody)
        unless (hash (block ^. gbExtra) == (block ^. mainBlockEBDataProof)) $
            throwError "Hash of extra body data is not equal to it's representation in the header."

verifyMainBlockHeader ::
       (HasConfiguration, Ssc ssc, MonadError Text m, Bi $ BodyProof $ MainBlockchain ssc)
    => MainBlockHeader ssc
    -> m ()
verifyMainBlockHeader mbh = do
    when (selfSignedProxy $ _mcdSignature) $
        throwError "can't use self-signed psk to issue the block"
    unless (verifyBlockSignature _mcdSignature) $
        throwError "can't verify signature"
  where

    selfSignedProxy (BlockSignature _)                      = False
    selfSignedProxy (BlockPSignatureLight (psigPsk -> psk)) = isSelfSignedPsk psk
    selfSignedProxy (BlockPSignatureHeavy (psigPsk -> psk)) = isSelfSignedPsk psk

    verifyBlockSignature (BlockSignature sig) =
        checkSig SignMainBlock leaderPk signature sig
    verifyBlockSignature (BlockPSignatureLight proxySig) =
        proxyVerify
            SignMainBlockLight
            proxySig
            (\(epochLow, epochHigh) ->
                 epochLow <= epochId && epochId <= epochHigh)
            signature
    verifyBlockSignature (BlockPSignatureHeavy proxySig) =
        proxyVerify SignMainBlockHeavy proxySig (const True) signature
    signature = MainToSign _gbhPrevBlock _gbhBodyProof slotId difficulty extra
    epochId = siEpoch slotId
    UnsafeGenericBlockHeader {
        _gbhConsensus = MainConsensusData
            { _mcdLeaderKey = leaderPk
            , _mcdSlot = slotId
            , _mcdDifficulty = difficulty
            , ..
            }
      , _gbhExtra = extra
      , ..
      } = mbh
