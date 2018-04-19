{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

-- | Definition of 'BlockchainHelpers' for the main blockchain.
--
-- FIXME rename this module to something to do with verification.

module Pos.Block.BHelpers
       ( verifyBlockHeader
       , verifyBlock
       , verifyGenesisBlock
       , verifyMainBlock
       , verifyMainBody
       , verifyMainBlockHeader
       , verifyMainConsensusData
       , verifyMainExtraHeaderData
       ) where

import           Universum

import           Control.Monad.Except (MonadError (throwError))

import           Pos.Binary.Class (Bi)
import           Pos.Binary.Core ()
import           Pos.Core.Block (Block)
import           Pos.Core.Block.Blockchain (Blockchain (..), GenericBlock (..),
                                            GenericBlockHeader (..), gbExtra)
import           Pos.Core.Block.Genesis (GenesisBlockchain)
import           Pos.Core.Block.Main (Body (..), ConsensusData (..), MainBlockHeader,
                                      MainBlockchain, MainExtraHeaderData (..), MainToSign (..),
                                      mainBlockEBDataProof)
import           Pos.Core.Block.Union (BlockHeader (..), BlockSignature (..))
import           Pos.Core.Class (IsMainHeader (..))
import           Pos.Core.Configuration (HasProtocolMagic, protocolMagic, HasProtocolConstants)
import           Pos.Core.Delegation (LightDlgIndices (..), checkDlgPayload)
import           Pos.Core.Slotting (SlotId (..))
import           Pos.Core.Ssc (checkSscPayload)
import           Pos.Core.Txp (checkTxPayload)
import           Pos.Core.Update (checkSoftwareVersion, checkUpdatePayload)
import           Pos.Crypto (ProxySignature (..), SignTag (..), checkSig, hash, isSelfSignedPsk,
                             proxyVerify)
import           Pos.Ssc.Functions (verifySscPayload)
import           Pos.Util.Some (Some (Some))

-- | Verify a BlockHeader in isolation. There is nothing to be done for
-- genesis headers.
verifyBlockHeader
    :: ( MonadError Text m, Bi (BodyProof MainBlockchain), HasProtocolMagic )
    => BlockHeader
    -> m ()
verifyBlockHeader (BlockHeaderGenesis _) = pure ()
verifyBlockHeader (BlockHeaderMain bhm)  = verifyMainBlockHeader bhm

-- | Verify a Block in isolation.
verifyBlock
    :: ( MonadError Text m
       , Bi BlockHeader
       , Bi (BodyProof MainBlockchain)
       , IsMainHeader MainBlockHeader
       , HasProtocolConstants
       , HasProtocolMagic
       )
    => Block
    -> m ()
verifyBlock = either verifyGenesisBlock verifyMainBlock

-- | To verify a genesis block we only have to check the body proof.
verifyGenesisBlock
    :: ( MonadError Text m )
    => GenericBlock GenesisBlockchain
    -> m ()
verifyGenesisBlock UnsafeGenericBlock {..} =
    checkBodyProof _gbBody (_gbhBodyProof _gbHeader)

verifyMainBlock
    :: ( MonadError Text m
       , Bi BlockHeader
       , Bi (BodyProof MainBlockchain)
       , IsMainHeader MainBlockHeader
       , HasProtocolConstants
       , HasProtocolMagic
       )
    => GenericBlock MainBlockchain
    -> m ()
verifyMainBlock block@UnsafeGenericBlock {..} = do
    verifyMainBlockHeader _gbHeader
    verifyMainBody _gbBody
    -- No need to verify the main extra body data. It's an 'Attributes ()'
    -- which is valid whenever it's well-formed.
    --
    -- Check internal consistency: the body proofs are all correct.
    checkBodyProof _gbBody (_gbhBodyProof _gbHeader)
    -- Check that the headers' extra body data hash is correct.
    -- This isn't subsumed by the body proof check.
    unless (hash (block ^. gbExtra) == (block ^. mainBlockEBDataProof)) $
        throwError "Hash of extra body data is not equal to its representation in the header."
    -- Ssc and Dlg consistency checks which require the header, and so can't
    -- be done in 'verifyMainBody'.
    either (throwError . pretty) pure $
        verifySscPayload
            (Right (Some _gbHeader))
            (_mbSscPayload _gbBody)

-- | Verify the body of a block. There are no internal consistency checks,
-- it's just a verification of its sub-components (payloads).
verifyMainBody
    :: ( MonadError Text m, HasProtocolMagic )
    => Body MainBlockchain
    -> m ()
verifyMainBody MainBody {..} = do
    checkTxPayload _mbTxPayload
    checkSscPayload protocolMagic _mbSscPayload
    checkDlgPayload protocolMagic _mbDlgPayload
    checkUpdatePayload protocolMagic _mbUpdatePayload

-- | Verify a main block header in isolation.
verifyMainBlockHeader
    :: ( MonadError Text m, Bi (BodyProof MainBlockchain), HasProtocolMagic )
    => GenericBlockHeader MainBlockchain
    -> m ()
verifyMainBlockHeader UnsafeGenericBlockHeader {..} = do
    -- Previous header hash is always valid.
    -- Body proof is just a bunch of hashes, which is always valid (although
    -- must be checked against the actual body, in verifyMainBlock.
    -- Consensus data and extra header data require validation.
    verifyMainConsensusData _gbhConsensus
    verifyMainExtraHeaderData _gbhExtra
    -- Internal consistency: is the signature in the consensus data really for
    -- this block?
    unless (verifyBlockSignature _mcdSignature) $
        throwError "can't verify signature"

  where

    verifyBlockSignature (BlockSignature sig) =
        checkSig protocolMagic SignMainBlock leaderPk signature sig
    verifyBlockSignature (BlockPSignatureLight proxySig) =
        proxyVerify
            protocolMagic
            SignMainBlockLight
            proxySig
            (\(LightDlgIndices (epochLow, epochHigh)) ->
                 epochLow <= epochId && epochId <= epochHigh)
            signature
    verifyBlockSignature (BlockPSignatureHeavy proxySig) =
        proxyVerify protocolMagic SignMainBlockHeavy proxySig (const True) signature
    signature = MainToSign _gbhPrevBlock _gbhBodyProof slotId difficulty _gbhExtra
    epochId = siEpoch slotId
    MainConsensusData
        { _mcdLeaderKey = leaderPk
        , _mcdSlot = slotId
        , _mcdDifficulty = difficulty
        , ..
        } = _gbhConsensus

-- | Verify the consensus data in isolation.
verifyMainConsensusData
    :: ( MonadError Text m )
    => ConsensusData MainBlockchain
    -> m ()
verifyMainConsensusData MainConsensusData {..} = do
    when (selfSignedProxy _mcdSignature) $
        throwError "can't use self-signed psk to issue the block"
  where
    selfSignedProxy (BlockSignature _)                      = False
    selfSignedProxy (BlockPSignatureLight (psigPsk -> psk)) = isSelfSignedPsk psk
    selfSignedProxy (BlockPSignatureHeavy (psigPsk -> psk)) = isSelfSignedPsk psk

verifyMainExtraHeaderData
    :: ( MonadError Text m )
    => ExtraHeaderData MainBlockchain
    -> m ()
verifyMainExtraHeaderData MainExtraHeaderData {..} = do
    checkSoftwareVersion _mehSoftwareVersion
