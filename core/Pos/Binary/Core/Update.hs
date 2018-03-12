-- | Binary serialization of core Update types.

module Pos.Binary.Core.Update
       (
       ) where

import           Universum

import           Data.Time.Units (Millisecond)
import           Serokell.Data.Memory.Units (Byte)

import           Pos.Binary.Class (Bi (..), Cons (..), Field (..), Raw, deriveSimpleBi,
                                   deriveSimpleBiCxt, encodeListLen, enforceSize)
import           Pos.Binary.Core.Common ()
import           Pos.Binary.Core.Fee ()
import           Pos.Binary.Core.Script ()
import           Pos.Core.Common (CoinPortion, ScriptVersion, TxFeePolicy)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.Slotting.Types (EpochIndex, FlatSlotId)
import qualified Pos.Core.Update as U
import           Pos.Core.Update.Types (BlockVersion, BlockVersionData (..), SoftforkRule (..),
                                        SoftwareVersion)
import           Pos.Crypto (Hash)

instance Bi U.ApplicationName where
    encode appName = encode (U.getApplicationName appName)
    decode = U.UncheckedApplicationName <$> decode

deriveSimpleBi ''U.BlockVersion [
    Cons 'U.BlockVersion [
        Field [| U.bvMajor :: Word16 |],
        Field [| U.bvMinor :: Word16 |],
        Field [| U.bvAlt   :: Word8  |]
    ]]

deriveSimpleBi ''U.SoftwareVersion [
    Cons 'U.SoftwareVersion [
        Field [| U.svAppName :: U.ApplicationName    |],
        Field [| U.svNumber  :: U.NumSoftwareVersion |]
    ]]

deriveSimpleBi ''SoftforkRule [
    Cons 'SoftforkRule [
        Field [| srInitThd      :: CoinPortion |],
        Field [| srMinThd       :: CoinPortion |],
        Field [| srThdDecrement :: CoinPortion |]
    ]]

deriveSimpleBi ''BlockVersionData [
    Cons 'BlockVersionData [
        Field [| bvdScriptVersion     :: ScriptVersion |],
        Field [| bvdSlotDuration      :: Millisecond   |],
        Field [| bvdMaxBlockSize      :: Byte          |],
        Field [| bvdMaxHeaderSize     :: Byte          |],
        Field [| bvdMaxTxSize         :: Byte          |],
        Field [| bvdMaxProposalSize   :: Byte          |],
        Field [| bvdMpcThd            :: CoinPortion   |],
        Field [| bvdHeavyDelThd       :: CoinPortion   |],
        Field [| bvdUpdateVoteThd     :: CoinPortion   |],
        Field [| bvdUpdateProposalThd :: CoinPortion   |],
        Field [| bvdUpdateImplicit    :: FlatSlotId    |],
        Field [| bvdSoftforkRule      :: SoftforkRule  |],
        Field [| bvdTxFeePolicy       :: TxFeePolicy   |],
        Field [| bvdUnlockStakeEpoch  :: EpochIndex    |]
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

instance Bi U.SystemTag where
    encode = encode . U.getSystemTag
    decode = U.UncheckedSystemTag <$> decode

deriveSimpleBi ''U.UpdateData [
    Cons 'U.UpdateData [
        Field [| U.udAppDiffHash  :: Hash Raw |],
        Field [| U.udPkgHash      :: Hash Raw |],
        Field [| U.udUpdaterHash  :: Hash Raw |],
        Field [| U.udMetadataHash :: Hash Raw |]
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
        U.UncheckedUpdateProposal <$> decode
                               <*> decode
                               <*> decode
                               <*> decode
                               <*> decode
                               <*> decode
                               <*> decode

instance HasConfiguration => Bi U.UpdateVote where
    encode uv =  encodeListLen 4
            <> encode (U.uvKey uv)
            <> encode (U.uvProposalId uv)
            <> encode (U.uvDecision uv)
            <> encode (U.uvSignature uv)
    decode = do
        enforceSize "UpdateVote" 4
        uvKey        <- decode
        uvProposalId <- decode
        uvDecision   <- decode
        uvSignature  <- decode
        pure U.UncheckedUpdateVote{..}

deriveSimpleBiCxt [t|HasConfiguration|] ''U.UpdatePayload [
    Cons 'U.UpdatePayload [
        Field [| U.upProposal :: Maybe U.UpdateProposal |],
        Field [| U.upVotes    :: [U.UpdateVote]         |]
    ]]
