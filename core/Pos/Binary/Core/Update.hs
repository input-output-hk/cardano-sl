-- | Binary serialization of core Update types.

module Pos.Binary.Core.Update
       (
       ) where

import           Universum

import           Data.Time.Units (Millisecond)
import           Serokell.Data.Memory.Units (Byte)

import           Pos.Binary.Class (Bi (..), Cons (..), Field (..), Raw, deriveSimpleBi,
                                   deriveSimpleBiCxt, encodeListLen, enforceSize)
import           Pos.Binary.Core.Script ()
import           Pos.Binary.Core.Types ()
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.Fee (TxFeePolicy)
import           Pos.Core.Types (BlockVersion, CoinPortion, EpochIndex, FlatSlotId, ScriptVersion,
                                 SoftforkRule, SoftwareVersion)
import qualified Pos.Core.Update as U
import           Pos.Crypto (Hash, SignTag (SignUSVote), checkSig)

instance Bi U.SystemTag where
    encode = encode . U.getSystemTag
    decode = decode >>= \decoded -> case U.mkSystemTag decoded of
        Left e   -> fail e
        Right st -> pure st

deriveSimpleBi ''U.UpdateData [
    Cons 'U.UpdateData [
        Field [| U.udAppDiffHash  :: Hash Raw |],
        Field [| U.udPkgHash      :: Hash Raw |],
        Field [| U.udUpdaterHash  :: Hash Raw |],
        Field [| U.udMetadataHash :: Hash Raw |]
    ]]

deriveSimpleBi ''U.BlockVersionModifier [
    Cons 'U.BlockVersionModifier [
        Field [| U.bvmScriptVersion     :: Maybe ScriptVersion |],
        Field [| U.bvmSlotDuration      :: Maybe Millisecond   |],
        Field [| U.bvmMaxBlockSize      :: Maybe Byte          |],
        Field [| U.bvmMaxHeaderSize     :: Maybe Byte          |],
        Field [| U.bvmMaxTxSize         :: Maybe Byte          |],
        Field [| U.bvmMaxProposalSize   :: Maybe Byte          |],
        Field [| U.bvmMpcThd            :: Maybe CoinPortion   |],
        Field [| U.bvmHeavyDelThd       :: Maybe CoinPortion   |],
        Field [| U.bvmUpdateVoteThd     :: Maybe CoinPortion   |],
        Field [| U.bvmUpdateProposalThd :: Maybe CoinPortion   |],
        Field [| U.bvmUpdateImplicit    :: Maybe FlatSlotId    |],
        Field [| U.bvmSoftforkRule      :: Maybe SoftforkRule  |],
        Field [| U.bvmTxFeePolicy       :: Maybe TxFeePolicy   |],
        Field [| U.bvmUnlockStakeEpoch  :: Maybe EpochIndex    |]
    ]]

deriveSimpleBi ''U.UpdateProposalToSign [
    Cons 'U.UpdateProposalToSign [
        Field [| U.upsBV   :: BlockVersion                     |],
        Field [| U.upsBVM  :: U.BlockVersionModifier           |],
        Field [| U.upsSV   :: SoftwareVersion                  |],
        Field [| U.upsData :: HashMap U.SystemTag U.UpdateData |],
        Field [| U.upsAttr :: U.UpAttributes                   |]
    ]]

instance HasConfiguration => Bi U.UpdateProposal where
    encode up = encodeListLen 7
            <> encode (U.upBlockVersion up)
            <> encode (U.upBlockVersionMod up)
            <> encode (U.upSoftwareVersion up)
            <> encode (U.upData up)
            <> encode (U.upAttributes up)
            <> encode (U.upFrom up)
            <> encode (U.upSignature up)
    decode = do
        enforceSize "UpdateProposal" 7
        up <- U.mkUpdateProposal <$> decode
                                <*> decode
                                <*> decode
                                <*> decode
                                <*> decode
                                <*> decode
                                <*> decode
        case up of
            Left e  -> fail e
            Right p -> pure p

instance HasConfiguration => Bi U.UpdateVote where
    encode uv =  encodeListLen 4
            <> encode (U.uvKey uv)
            <> encode (U.uvProposalId uv)
            <> encode (U.uvDecision uv)
            <> encode (U.uvSignature uv)
    decode = do
        enforceSize "UpdateVote" 4
        k <- decode
        p <- decode
        d <- decode
        s <- decode
        let sigValid = checkSig SignUSVote k (p, d) s
        unless sigValid $ fail "Pos.Binary.Update: UpdateVote: invalid signature"
        pure $ U.UpdateVote k p d s

deriveSimpleBiCxt [t|HasConfiguration|] ''U.UpdatePayload [
    Cons 'U.UpdatePayload [
        Field [| U.upProposal :: Maybe U.UpdateProposal |],
        Field [| U.upVotes    :: [U.UpdateVote]         |]
    ]]
