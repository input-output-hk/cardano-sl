{-# LANGUAGE RankNTypes #-}

-- | Blockchain generation logic.

module Pos.Generator.Block.Logic
       ( genBlocks
       ) where

import           Universum

import           Control.Lens                (at, ix, _Wrapped)
import           Control.Monad.Random.Strict (RandT, mapRandT)
import           Formatting                  (build, sformat, (%))
import           System.Random               (RandomGen (..))
import           System.Wlog                 (logWarning)

import           Pos.AllSecrets              (HasAllSecrets (..), unInvSecretsMap)
import           Pos.Block.Core              (mkGenesisBlock)
import           Pos.Block.Logic             (applyBlocksUnsafe, createMainBlockInternal,
                                              normalizeMempool, verifyBlocksPrefix)
import           Pos.Block.Types             (Blund)
import           Pos.Core                    (EpochOrSlot (..), SlotId (..), epochIndexL,
                                              getEpochOrSlot, getSlotIndex)
import           Pos.DB.DB                   (getTipHeader)
import           Pos.Generator.Block.Error   (BlockGenError (..))
import           Pos.Generator.Block.Mode    (BlockGenRandMode, MonadBlockGen,
                                              mkBlockGenContext, usingPrimaryKey,
                                              withCurrentSlot)
import           Pos.Generator.Block.Param   (BlockGenParams, HasBlockGenParams (..))
import           Pos.Generator.Block.Payload (genPayload)
import           Pos.Lrc                     (lrcSingleShot)
import           Pos.Lrc.Context             (lrcActionOnEpochReason)
import qualified Pos.Lrc.DB                  as LrcDB
import           Pos.Ssc.GodTossing          (SscGodTossing)
import           Pos.Util.Chrono             (OldestFirst (..))
import           Pos.Util.Util               (maybeThrow, _neHead)

----------------------------------------------------------------------------
-- Block generation
----------------------------------------------------------------------------

-- | Generate an arbitrary sequence of valid blocks. The blocks are
-- valid with respect to the global state right before this function
-- call.
genBlocks ::
       (MonadBlockGen ctx m, RandomGen g)
    => BlockGenParams
    -> RandT g m (OldestFirst [] (Blund SscGodTossing))
genBlocks params = do
    ctx <- lift $ mkBlockGenContext params
    mapRandT (`runReaderT` ctx) genBlocksDo
  where
    genBlocksDo =
        OldestFirst <$> do
            let numberOfBlocks = params ^. bgpBlockCount
            tipEOS <- getEpochOrSlot <$> lift (getTipHeader @SscGodTossing)
            let startEOS = succ tipEOS
            let finishEOS =
                    toEnum $ fromEnum tipEOS + fromIntegral numberOfBlocks
            catMaybes <$> mapM genBlock [startEOS .. finishEOS]

-- Generate a valid 'Block' for the given epoch or slot (genesis block
-- in the former case and main block the latter case) and apply it.
genBlock ::
       forall ctx m g. (RandomGen g, MonadBlockGen ctx m)
    => EpochOrSlot
    -> BlockGenRandMode g m (Maybe (Blund SscGodTossing))
genBlock eos = do
    let epoch = eos ^. epochIndexL
    unlessM ((epoch ==) <$> lift LrcDB.getEpoch) $
        lift $ lrcSingleShot epoch
    -- We need to know leaders to create any block.
    leaders <- lift $ lrcActionOnEpochReason epoch "genBlock" LrcDB.getLeadersForEpoch
    case eos of
        EpochOrSlot (Left _) -> do
            tipHeader <- lift $ getTipHeader @SscGodTossing
            let slot0 = SlotId epoch minBound
            let genesisBlock = mkGenesisBlock (Just tipHeader) epoch leaders
            fmap Just $ withCurrentSlot slot0 $ lift $ verifyAndApply (Left genesisBlock)
        EpochOrSlot (Right slot@SlotId {..}) -> withCurrentSlot slot $ do
            genPayload slot
            -- We need to know leader's secret key to create a block.
            leader <-
                lift $ maybeThrow
                    (BGInternal "no leader")
                    (leaders ^? ix (fromIntegral $ getSlotIndex siSlot))
            secrets <-
                unInvSecretsMap . view asSecretKeys <$> view blockGenParams
            let maybeLeader = secrets ^. at leader
            canSkip <- view bgpSkipNoKey
            case (maybeLeader, canSkip) of
                (Nothing,True)     -> do
                    lift $ logWarning $
                        sformat ("Skipping block creation for leader "%build%
                                 " as no related key was found")
                                leader
                    pure Nothing
                (Nothing,False)    ->
                    throwM $ BGUnknownSecret leader
                (Just leaderSK, _) ->
                    -- When we know the secret key we can proceed to the actual creation.
                    Just <$> usingPrimaryKey leaderSK (genMainBlock slot)
  where
    genMainBlock slot =
        lift $ createMainBlockInternal @SscGodTossing slot Nothing >>= \case
            Left err -> throwM (BGFailedToCreate err)
            Right mainBlock -> verifyAndApply $ Right mainBlock
    verifyAndApply block =
        verifyBlocksPrefix (one block) >>= \case
            Left err -> throwM (BGCreatedInvalid err)
            Right (undos, pollModifier) -> do
                let undo = undos ^. _Wrapped . _neHead
                    blund = (block, undo)
                applyBlocksUnsafe (one blund) (Just pollModifier)
                normalizeMempool
                pure blund
