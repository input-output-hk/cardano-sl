module Pos.Core.Update.BlockVersionModifier
       ( BlockVersionModifier (..)
       , checkBlockVersionModifier
       ) where

import           Universum

import           Control.Monad.Except (MonadError)
import           Data.Default (Default (..))
import           Data.SafeCopy (base, deriveSafeCopySimple)
import qualified Data.Text.Buildable as Buildable
import           Data.Text.Lazy.Builder (Builder)
import           Data.Time.Units (Millisecond)
import           Formatting (Format, bprint, build, int, later, (%))
import           Serokell.Data.Memory.Units (Byte, memory)

import           Pos.Binary.Class (Cons (..), Field (..), deriveSimpleBi)
import           Pos.Core.Binary ()
import           Pos.Core.Common (CoinPortion, ScriptVersion, TxFeePolicy,
                     checkCoinPortion)
import           Pos.Core.Slotting (EpochIndex, FlatSlotId)
import           Pos.Util.Orphans ()

import           Pos.Core.Update.SoftforkRule

-- | Data which represents modifications of block (aka protocol) version.
data BlockVersionModifier = BlockVersionModifier
    { bvmScriptVersion     :: !(Maybe ScriptVersion)
    , bvmSlotDuration      :: !(Maybe Millisecond)
    , bvmMaxBlockSize      :: !(Maybe Byte)
    , bvmMaxHeaderSize     :: !(Maybe Byte)
    , bvmMaxTxSize         :: !(Maybe Byte)
    , bvmMaxProposalSize   :: !(Maybe Byte)
    , bvmMpcThd            :: !(Maybe CoinPortion)
    , bvmHeavyDelThd       :: !(Maybe CoinPortion)
    , bvmUpdateVoteThd     :: !(Maybe CoinPortion)
    , bvmUpdateProposalThd :: !(Maybe CoinPortion)
    , bvmUpdateImplicit    :: !(Maybe FlatSlotId)
    , bvmSoftforkRule      :: !(Maybe SoftforkRule)
    , bvmTxFeePolicy       :: !(Maybe TxFeePolicy)
    , bvmUnlockStakeEpoch  :: !(Maybe EpochIndex)
    } deriving (Show, Eq, Ord, Generic, Typeable)

instance NFData BlockVersionModifier
instance Hashable BlockVersionModifier

instance Default BlockVersionModifier where
    def = BlockVersionModifier
        { bvmScriptVersion     = Nothing
        , bvmSlotDuration      = Nothing
        , bvmMaxBlockSize      = Nothing
        , bvmMaxHeaderSize     = Nothing
        , bvmMaxTxSize         = Nothing
        , bvmMaxProposalSize   = Nothing
        , bvmMpcThd            = Nothing
        , bvmHeavyDelThd       = Nothing
        , bvmUpdateVoteThd     = Nothing
        , bvmUpdateProposalThd = Nothing
        , bvmUpdateImplicit    = Nothing
        , bvmSoftforkRule      = Nothing
        , bvmTxFeePolicy       = Nothing
        , bvmUnlockStakeEpoch  = Nothing
        }

instance Buildable BlockVersionModifier where
    build BlockVersionModifier {..} =
      bprint ("{ script version: "%bmodifier build%
              ", slot duration (mcs): "%bmodifier int%
              ", block size limit: "%bmodifier memory%
              ", header size limit: "%bmodifier memory%
              ", tx size limit: "%bmodifier memory%
              ", proposal size limit: "%bmodifier memory%
              ", mpc threshold: "%bmodifier build%
              ", heavyweight delegation threshold: "%bmodifier build%
              ", update vote threshold: "%bmodifier build%
              ", update proposal threshold: "%bmodifier build%
              ", update implicit period (slots): "%bmodifier int%
              ", softfork rule: "%bmodifier build%
              ", tx fee policy: "%bmodifier build%
              ", unlock stake epoch: "%bmodifier build%
              " }")
        bvmScriptVersion
        bvmSlotDuration
        bvmMaxBlockSize
        bvmMaxHeaderSize
        bvmMaxTxSize
        bvmMaxProposalSize
        bvmMpcThd
        bvmHeavyDelThd
        bvmUpdateVoteThd
        bvmUpdateProposalThd
        bvmUpdateImplicit
        bvmSoftforkRule
        bvmTxFeePolicy
        bvmUnlockStakeEpoch
      where
        bmodifier :: Format Builder (a -> Builder) -> Format r (Maybe a -> r)
        bmodifier b = later $ maybe "no change" (bprint b)

checkBlockVersionModifier
    :: (MonadError Text m)
    => BlockVersionModifier
    -> m ()
checkBlockVersionModifier BlockVersionModifier {..} = do
    whenJust bvmMpcThd checkCoinPortion
    whenJust bvmHeavyDelThd checkCoinPortion
    whenJust bvmUpdateVoteThd checkCoinPortion
    whenJust bvmUpdateProposalThd checkCoinPortion
    whenJust bvmSoftforkRule checkSoftforkRule

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

deriveSafeCopySimple 0 'base ''BlockVersionModifier
