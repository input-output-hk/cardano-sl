{-# LANGUAGE RecordWildCards #-}

module Pos.Chain.Update.BlockVersionData
       ( BlockVersionData (..)
       , isBootstrapEraBVD
       , ConsensusEra (..)
       , consensusEraBVD
       ) where

import           Universum

import           Control.Monad.Except (MonadError)
import qualified Data.Aeson.Options as S (defaultOptions)
import           Data.Aeson.TH (deriveJSON)
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Data.Time.Units (Millisecond)
import           Formatting (bprint, build, int, (%))
import qualified Formatting.Buildable as Buildable
import           Serokell.Data.Memory.Units (Byte, memory)
import           Text.JSON.Canonical (FromJSON (..), ToJSON (..), fromJSField,
                     mkObject)

import           Pos.Binary.Class (Cons (..), Field (..), deriveSimpleBi)
import           Pos.Chain.Update.SoftforkRule
import           Pos.Core.Binary ()
import           Pos.Core.Common (CoinPortion, ScriptVersion, TxFeePolicy)
import           Pos.Core.Slotting (EpochIndex (..), FlatSlotId, isBootstrapEra)
import           Pos.Util.Json.Canonical (SchemaError)
import           Pos.Util.Orphans ()

-- | Data which is associated with 'BlockVersion'.
data BlockVersionData = BlockVersionData
    { bvdScriptVersion     :: !ScriptVersion
    , bvdSlotDuration      :: !Millisecond
    , bvdMaxBlockSize      :: !Byte
    , bvdMaxHeaderSize     :: !Byte
    , bvdMaxTxSize         :: !Byte
    , bvdMaxProposalSize   :: !Byte
    , bvdMpcThd            :: !CoinPortion
    , bvdHeavyDelThd       :: !CoinPortion
    , bvdUpdateVoteThd     :: !CoinPortion
    , bvdUpdateProposalThd :: !CoinPortion
    , bvdUpdateImplicit    :: !FlatSlotId
    , bvdSoftforkRule      :: !SoftforkRule
    , bvdTxFeePolicy       :: !TxFeePolicy
    , bvdUnlockStakeEpoch  :: !EpochIndex
    } deriving (Show, Eq, Ord, Generic, Typeable)

instance NFData BlockVersionData where

instance Buildable BlockVersionData where
    build BlockVersionData {..} =
      bprint ("{ script version: "%build%
              ", slot duration: "%int%" mcs"%
              ", block size limit: "%memory%
              ", header size limit: "%memory%
              ", tx size limit: "%memory%
              ", proposal size limit: "%memory%
              ", mpc threshold: "%build%
              ", heavyweight delegation threshold: "%build%
              ", update vote threshold: "%build%
              ", update proposal threshold: "%build%
              ", update implicit period: "%int%" slots"%
              ", softfork rule: "%build%
              ", tx fee policy: "%build%
              ", unlock stake epoch: "%build%
              " }")
        bvdScriptVersion
        bvdSlotDuration
        bvdMaxBlockSize
        bvdMaxHeaderSize
        bvdMaxTxSize
        bvdMaxProposalSize
        bvdMpcThd
        bvdHeavyDelThd
        bvdUpdateVoteThd
        bvdUpdateProposalThd
        bvdUpdateImplicit
        bvdSoftforkRule
        bvdTxFeePolicy
        bvdUnlockStakeEpoch

instance Monad m => ToJSON m BlockVersionData where
    toJSON (BlockVersionData scriptVersion slotDuration maxBlockSize maxHeaderSize maxTxSize maxProposalSize mpcThd heavyDelThd updateVoteThd updateProposalThd updateImplicit softforkRule txFeePolicy unlockStakeEpoch) =
        mkObject
            [ ("scriptVersion", toJSON scriptVersion)
            , ("slotDuration", toJSON slotDuration)
            , ("maxBlockSize", toJSON maxBlockSize)
            , ("maxHeaderSize", toJSON maxHeaderSize)
            , ("maxTxSize", toJSON maxTxSize)
            , ("maxProposalSize", toJSON maxProposalSize)
            , ("mpcThd", toJSON mpcThd)
            , ("heavyDelThd", toJSON heavyDelThd)
            , ("updateVoteThd", toJSON updateVoteThd)
            , ("updateProposalThd", toJSON updateProposalThd)
            , ("updateImplicit", toJSON updateImplicit)
            , ("softforkRule", toJSON softforkRule)
            , ("txFeePolicy", toJSON txFeePolicy)
            , ("unlockStakeEpoch", toJSON unlockStakeEpoch)
            ]

instance MonadError SchemaError m => FromJSON m BlockVersionData where
    fromJSON obj = do
        bvdScriptVersion <- fromJSField obj "scriptVersion"
        bvdSlotDuration <- fromJSField obj "slotDuration"
        bvdMaxBlockSize <- fromJSField obj "maxBlockSize"
        bvdMaxHeaderSize <- fromJSField obj "maxHeaderSize"
        bvdMaxTxSize <- fromJSField obj "maxTxSize"
        bvdMaxProposalSize <- fromJSField obj "maxProposalSize"
        bvdMpcThd <- fromJSField obj "mpcThd"
        bvdHeavyDelThd <- fromJSField obj "heavyDelThd"
        bvdUpdateVoteThd <- fromJSField obj "updateVoteThd"
        bvdUpdateProposalThd <- fromJSField obj "updateProposalThd"
        bvdUpdateImplicit <- fromJSField obj "updateImplicit"
        bvdSoftforkRule <- fromJSField obj "softforkRule"
        bvdTxFeePolicy <- fromJSField obj "txFeePolicy"
        bvdUnlockStakeEpoch <- fromJSField obj "unlockStakeEpoch"
        return BlockVersionData {..}

deriveJSON S.defaultOptions ''BlockVersionData

-- | Version of 'isBootstrapEra' which takes 'BlockVersionData'
-- instead of unlock stake epoch.
isBootstrapEraBVD :: BlockVersionData -> EpochIndex -> Bool
isBootstrapEraBVD adoptedBVD = isBootstrapEra (bvdUnlockStakeEpoch adoptedBVD)

-- | This is a magic value which signifies that we are in the OBFT era.
-- Since we are no longer decentralizing this codebase, we can repurpose
-- this field to signify the shift to OBFT.
obftEraFlagValue :: EpochIndex
obftEraFlagValue = EpochIndex 9999999999999999999

data ConsensusEra = Original | OBFT
    deriving (Show)

-- | This function uses the repurposed field `bvdUnlockStakeEpoch` to
-- tell us whether we are in the Original or OBFT consensus eras.
consensusEraBVD :: BlockVersionData -> ConsensusEra
consensusEraBVD bvd = if obftEraFlagValue == bvdUnlockStakeEpoch bvd
                         then OBFT
                         else Original

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

deriveSafeCopySimple 0 'base ''BlockVersionData
