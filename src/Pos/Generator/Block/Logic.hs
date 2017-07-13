-- | Blockchain generation logic.

module Pos.Generator.Block.Logic
       ( genBlocks
       ) where

import           Universum

import           Control.Lens              (at, ix, _Wrapped)

import           Pos.Block.Core            (mkGenesisBlock)
import           Pos.Block.Logic           (applyBlocksUnsafe, createMainBlockInternal,
                                            verifyBlocksPrefix)
import           Pos.Block.Types           (Blund)
import           Pos.Core                  (EpochOrSlot (..), SlotId (..), epochIndexL,
                                            getEpochOrSlot, getSlotIndex)
import           Pos.DB.DB                 (getTipHeader)
import           Pos.Generator.Block.Error (BlockGenError (..))
import           Pos.Generator.Block.Mode  (BlockGenMode, MonadBlockGen,
                                            MonadBlockGenBase, mkBlockGenContext,
                                            usingPrimaryKey, withCurrentSlot)
import           Pos.Generator.Block.Param (BlockGenParams, HasAllSecrets (..),
                                            HasBlockGenParams (..))
import           Pos.Lrc.Context           (lrcActionOnEpochReason)
import qualified Pos.Lrc.DB                as LrcDB
import           Pos.Ssc.GodTossing        (SscGodTossing)
import           Pos.Util.Chrono           (OldestFirst (..))
import           Pos.Util.Util             (maybeThrow, _neHead)

----------------------------------------------------------------------------
-- Block generation
----------------------------------------------------------------------------

-- | Generate an arbitrary sequence of valid blocks. The blocks are
-- valid with respect to the global state right before this function
-- call.
genBlocks ::
       MonadBlockGen ctx m
    => BlockGenParams
    -> m (OldestFirst [] (Blund SscGodTossing))
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
    -> BlockGenMode m (Blund SscGodTossing)
genBlock eos = do
    -- We need to know leaders to create any block.
    leaders <-
        lrcActionOnEpochReason (eos ^. epochIndexL) "genBlock" LrcDB.getLeaders
    case eos of
        EpochOrSlot (Left epoch) -> do
            tipHeader <- getTipHeader @SscGodTossing
            let slot0 = SlotId epoch minBound
            let genesisBlock = mkGenesisBlock (Just tipHeader) epoch leaders
            withCurrentSlot slot0 $ verifyAndApply (Left genesisBlock)
        EpochOrSlot (Right slot@SlotId {..}) -> do
            genPayload slot
                    -- We need to know leader's secret key to create a block.
            leader <-
                maybeThrow
                    (BGInternal "no leader")
                    (leaders ^? ix (fromIntegral $ getSlotIndex siSlot))
            secrets <- view asSecretKeys <$> view blockGenParams
            leaderSK <-
                maybeThrow (BGUnknownSecret leader) (secrets ^. at leader)
                    -- When we know the secret key we can proceed to the actual creation.
            withCurrentSlot slot $ usingPrimaryKey leaderSK (genMainBlock slot)
  where
    genMainBlock slot =
        createMainBlockInternal @SscGodTossing slot Nothing >>= \case
            Left err -> throwM (BGFailedToCreate err)
            Right mainBlock -> verifyAndApply $ Right mainBlock
    verifyAndApply block =
        verifyBlocksPrefix (one block) >>= \case
            Left err -> throwM (BGCreatedInvalid err)
            Right (undos, pollModifier) ->
                let undo = undos ^. _Wrapped . _neHead
                    blund = (block, undo)
                in blund <$ applyBlocksUnsafe (one blund) (Just pollModifier)

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
