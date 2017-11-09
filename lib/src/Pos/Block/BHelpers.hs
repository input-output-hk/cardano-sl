{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

-- | Definition of 'BlockchainHelpers' for the main blockchain.

module Pos.Block.BHelpers
       (
       ) where

import           Universum

import           Control.Monad.Except (MonadError (throwError))

import           Pos.Binary.Class (Bi)
import           Pos.Binary.Core ()
import           Pos.Binary.Core.Txp ()
import           Pos.Binary.Core.Update ()
import           Pos.Core.Block.Blockchain (Blockchain (..), BlockchainHelpers (..),
                                            GenericBlock (..), GenericBlockHeader (..), gbExtra)
import           Pos.Core.Block.Main.Chain (Body (..), ConsensusData (..))
import           Pos.Core.Block.Main.Lens (mainBlockEBDataProof)
import           Pos.Core.Block.Main.Types (MainBlockHeader, MainBlockchain, MainToSign (..))
import           Pos.Core.Block.Union.Types (BlockHeader, BlockSignature (..))
import           Pos.Core.Class (IsMainHeader (..), epochIndexL)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.Types (SlotId (..))
import           Pos.Crypto (ProxySignature (..), SignTag (..), checkSig, hash, isSelfSignedPsk,
                             proxyVerify)
import           Pos.Delegation.Helpers (dlgVerifyPayload)
import           Pos.Ssc.Functions (verifySscPayload)
import           Pos.Util.Util (Some (Some))

instance ( Bi BlockHeader
         , HasConfiguration
         , IsMainHeader MainBlockHeader
         ) =>
         BlockchainHelpers MainBlockchain where
    verifyBBlockHeader = verifyMainBlockHeader
    verifyBBlock block@UnsafeGenericBlock {..} = do
        either (throwError . pretty) pure $
            verifySscPayload
                (Right (Some _gbHeader))
                (_mbSscPayload _gbBody)
        dlgVerifyPayload (_gbHeader ^. epochIndexL) (_mbDlgPayload _gbBody)
        unless (hash (block ^. gbExtra) == (block ^. mainBlockEBDataProof)) $
            throwError "Hash of extra body data is not equal to it's representation in the header."

verifyMainBlockHeader ::
       (HasConfiguration, MonadError Text m, Bi (BodyProof MainBlockchain))
    => MainBlockHeader
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
