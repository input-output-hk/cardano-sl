{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Binary serialization of core Update types.

module Pos.Binary.Core.Update
       (
       ) where

import           Universum

import           Data.Time.Units (Millisecond)
import           Serokell.Data.Memory.Units (Byte)

import           Pos.Binary.Class (Bi (..), Cons (..), Field (..), Raw, deriveSimpleBi,
                                   encodeListLen, enforceSize)
import           Pos.Binary.Core.Script ()
import           Pos.Core.Common (CoinPortion, ScriptVersion, TxFeePolicy)
import           Pos.Core.Slotting (EpochIndex, FlatSlotId)
import           Pos.Core.Update (ApplicationName (..), BlockVersion (..), BlockVersionData (..),
                                  BlockVersionModifier (..), NumSoftwareVersion, SoftforkRule (..),
                                  SoftwareVersion (..), SystemTag (..), UpAttributes,
                                  UpdateData (..), UpdatePayload (..), UpdateProposal (..),
                                  UpdateProposalToSign (..), UpdateVote (..))
import           Pos.Crypto (Hash)

instance Bi ApplicationName where
    encode appName = encode (getApplicationName appName)
    decode = ApplicationName <$> decode

deriveSimpleBi ''BlockVersion [
    Cons 'BlockVersion [
        Field [| bvMajor :: Word16 |],
        Field [| bvMinor :: Word16 |],
        Field [| bvAlt   :: Word8  |]
    ]]

deriveSimpleBi ''SoftwareVersion [
    Cons 'SoftwareVersion [
        Field [| svAppName :: ApplicationName    |],
        Field [| svNumber  :: NumSoftwareVersion |]
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

deriveSimpleBi ''BlockVersionModifier [
    Cons 'BlockVersionModifier [
        Field [| bvmScriptVersion     :: Maybe ScriptVersion |],
        Field [| bvmSlotDuration      :: Maybe Millisecond   |],
        Field [| bvmMaxBlockSize      :: Maybe Byte          |],
        Field [| bvmMaxHeaderSize     :: Maybe Byte          |],
        Field [| bvmMaxTxSize         :: Maybe Byte          |],
        Field [| bvmMaxProposalSize   :: Maybe Byte          |],
        Field [| bvmMpcThd            :: Maybe CoinPortion   |],
        Field [| bvmHeavyDelThd       :: Maybe CoinPortion   |],
        Field [| bvmUpdateVoteThd     :: Maybe CoinPortion   |],
        Field [| bvmUpdateProposalThd :: Maybe CoinPortion   |],
        Field [| bvmUpdateImplicit    :: Maybe FlatSlotId    |],
        Field [| bvmSoftforkRule      :: Maybe SoftforkRule  |],
        Field [| bvmTxFeePolicy       :: Maybe TxFeePolicy   |],
        Field [| bvmUnlockStakeEpoch  :: Maybe EpochIndex    |]
    ]]

instance Bi SystemTag where
    encode = encode . getSystemTag
    decode = SystemTag <$> decode

deriveSimpleBi ''UpdateData [
    Cons 'UpdateData [
        Field [| udAppDiffHash  :: Hash Raw |],
        Field [| udPkgHash      :: Hash Raw |],
        Field [| udUpdaterHash  :: Hash Raw |],
        Field [| udMetadataHash :: Hash Raw |]
    ]]

deriveSimpleBi ''UpdateProposalToSign [
    Cons 'UpdateProposalToSign [
        Field [| upsBV   :: BlockVersion                     |],
        Field [| upsBVM  :: BlockVersionModifier           |],
        Field [| upsSV   :: SoftwareVersion                  |],
        Field [| upsData :: HashMap SystemTag UpdateData |],
        Field [| upsAttr :: UpAttributes                   |]
    ]]

instance Bi UpdateProposal where
    encode up = encodeListLen 7
            <> encode (upBlockVersion up)
            <> encode (upBlockVersionMod up)
            <> encode (upSoftwareVersion up)
            <> encode (upData up)
            <> encode (upAttributes up)
            <> encode (upFrom up)
            <> encode (upSignature up)
    decode = do
        enforceSize "UpdateProposal" 7
        UnsafeUpdateProposal <$> decode
                               <*> decode
                               <*> decode
                               <*> decode
                               <*> decode
                               <*> decode
                               <*> decode

instance Bi UpdateVote where
    encode uv =  encodeListLen 4
            <> encode (uvKey uv)
            <> encode (uvProposalId uv)
            <> encode (uvDecision uv)
            <> encode (uvSignature uv)
    decode = do
        enforceSize "UpdateVote" 4
        uvKey        <- decode
        uvProposalId <- decode
        uvDecision   <- decode
        uvSignature  <- decode
        pure UnsafeUpdateVote{..}

deriveSimpleBi ''UpdatePayload [
    Cons 'UpdatePayload [
        Field [| upProposal :: Maybe UpdateProposal |],
        Field [| upVotes    :: [UpdateVote]         |]
    ]]
