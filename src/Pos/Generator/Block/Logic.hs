-- | Blockchain generation logic.

module Pos.Generator.Block.Logic
       ( genBlocks
       ) where

import           Universum

import           Control.Lens              (at, ix)

import           Pos.Block.Core            (Block)
import           Pos.Block.Logic           (createMainBlockInternal)
import           Pos.Core                  (EpochOrSlot (..), HasPrimaryKey (..),
                                            SlotId (..), getEpochOrSlot, getSlotIndex)
import           Pos.DB.DB                 (getTipHeader)
import           Pos.Generator.Block.Error (BlockGenError (..))
import           Pos.Generator.Block.Mode  (BlockGenMode, MonadBlockGen,
                                            MonadBlockGenBase, mkBlockGenContext)
import           Pos.Generator.Block.Param (BlockGenParams, HasAllSecrets (..),
                                            HasBlockGenParams (..))
import           Pos.Lrc.Context           (lrcActionOnEpochReason)
import qualified Pos.Lrc.DB                as LrcDB
import           Pos.Ssc.GodTossing        (SscGodTossing)
import           Pos.Util.Chrono           (OldestFirst (..))
import           Pos.Util.Util             (maybeThrow)

----------------------------------------------------------------------------
-- Block generation
----------------------------------------------------------------------------

-- TODO: the blocks are also applied, but usually we shouldn't modify
-- the state.  The idea is to use 'DBProxyT' (which doesn't exist yet)
-- akin to 'PollT', 'TossT', etc. to put all modifications into a
-- temporary state instead of applying them to the given DB directly.
--
-- | Generate an arbitrary sequence of valid blocks. The blocks are
-- valid with respect to the global state right before this function
-- call.
genBlocks ::
       MonadBlockGen ctx m
    => BlockGenParams
    -> m (OldestFirst [] (Block SscGodTossing))
genBlocks params = do
    ctx <- mkBlockGenContext params
    runReaderT genBlocksDo ctx
  where
    genBlocksDo =
        OldestFirst <$> do
            let numberOfBlocks = params ^. bgpBlockCount
            tipEOS <- getEpochOrSlot <$> getTipHeader @SscGodTossing
            let startEOS = succ tipEOS
            let finishEOS =
                    toEnum $ fromEnum tipEOS + fromIntegral numberOfBlocks
            mapM genBlock [startEOS .. finishEOS]

-- Generate a valid 'Block' for the given epoch or slot (genesis block
-- in the former case and main block the latter case) and apply it.
genBlock ::
       (MonadBlockGenBase m)
    => EpochOrSlot
    -> BlockGenMode m (Block SscGodTossing)
genBlock (EpochOrSlot (Left epoch)) = undefined epoch
genBlock (EpochOrSlot (Right slot@SlotId {..})) = do
    genPayload slot
    -- We need to know leader's secret key to create a block.
    leaders <- lrcActionOnEpochReason siEpoch "genBlock" LrcDB.getLeaders
    leader <-
        maybeThrow
            (BGInternal "no leader")
            (leaders ^? ix (fromIntegral $ getSlotIndex siSlot))
    secrets <- view asSecretKeys <$> view blockGenParams
    leaderSK <- maybeThrow (BGUnknownSecret leader) (secrets ^. at leader)
    -- When we know the secret key we can proceed to the actual creation.
    local (set primaryKey leaderSK) genBlockDo
  where
    genBlockDo =
        (createMainBlockInternal @SscGodTossing slot Nothing) >>= \case
            Left err -> throwM (BGFailedToCreate err)
        -- apply, but probably reduced version (e. g. no slotting)
            Right mainBlock -> undefined mainBlock

----------------------------------------------------------------------------
-- Payload generation
----------------------------------------------------------------------------

-- Generate random payload which is valid with respect to the current
-- global state and mempool and add it to mempool.  Currently we are
-- concerned only about tx payload, later we can add more stuff.
genPayload :: Monad m => SlotId -> m ()
genPayload _ = genTxPayload

-- TODO: add randomness, implement, move to txp, think how to unite it
-- with 'Pos.Txp.Arbitrary'.
-- Generate valid 'TxPayload' using current global state.
genTxPayload :: Monad m => m ()
genTxPayload = pass
