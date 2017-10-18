{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

-- | Blockchain generation logic.

module Pos.Generator.Block.Logic
       ( BlockTxpGenMode
       , genBlocks
       ) where

import           Universum

import           Control.Lens                (at, ix, _Wrapped)
import           Control.Monad.Random.Strict (RandT, mapRandT)
import           Data.Default                (Default (def))
import           Formatting                  (build, sformat, (%))
import           System.Random               (RandomGen (..))
import           System.Wlog                 (logWarning)

import           Pos.AllSecrets              (HasAllSecrets (..), unInvSecretsMap)
import           Pos.Block.Core              (Block, mkGenesisBlock)
import           Pos.Block.Logic             (applyBlocksUnsafe, createMainBlockInternal,
                                              normalizeMempool, verifyBlocksPrefix)
import           Pos.Block.Slog              (ShouldCallBListener (..))
import           Pos.Block.Types             (Blund)
import           Pos.Core                    (EpochOrSlot (..), SlotId (..), addressHash,
                                              epochIndexL, getEpochOrSlot, getSlotIndex)
import           Pos.Crypto                  (pskDelegatePk)
import           Pos.DB.DB                   (getTipHeader)
import           Pos.Delegation.Logic        (getDlgTransPsk)
import           Pos.Delegation.Types        (ProxySKBlockInfo)
import           Pos.Generator.Block.Error   (BlockGenError (..))
import           Pos.Generator.Block.Mode    (BlockGenMode, BlockGenRandMode,
                                              MonadBlockGen, mkBlockGenContext,
                                              usingPrimaryKey, withCurrentSlot)
import           Pos.Generator.Block.Param   (BlockGenParams, HasBlockGenParams (..))
import           Pos.Generator.Block.Payload (genPayload)
import           Pos.Lrc                     (lrcSingleShot)
import           Pos.Lrc.Context             (lrcActionOnEpochReason)
import qualified Pos.Lrc.DB                  as LrcDB
import           Pos.Ssc.GodTossing          (SscGodTossing)
import           Pos.Txp                     (MempoolExt, MonadTxpLocal)
import           Pos.Util.Chrono             (OldestFirst (..))
import           Pos.Util.CompileInfo        (HasCompileInfo, withCompileInfo)
import           Pos.Util.Util               (maybeThrow, _neHead)

----------------------------------------------------------------------------
-- Block generation
----------------------------------------------------------------------------

type BlockTxpGenMode g ctx m =
    ( RandomGen g
    , MonadBlockGen ctx m
    , Default (MempoolExt m)
    , MonadTxpLocal (BlockGenMode (MempoolExt m) m)
    )

-- | Generate an arbitrary sequence of valid blocks. The blocks are
-- valid with respect to the global state right before this function
-- call.
genBlocks ::
       forall g ctx m . BlockTxpGenMode g ctx m
    => BlockGenParams
    -> RandT g m (OldestFirst [] (Blund SscGodTossing))
genBlocks params = do
    ctx <- lift $ mkBlockGenContext @(MempoolExt m) params
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
       forall g ctx m . BlockTxpGenMode g ctx m
    => EpochOrSlot
    -> BlockGenRandMode (MempoolExt m) g m (Maybe (Blund SscGodTossing))
genBlock eos = withCompileInfo def $ do
    let epoch = eos ^. epochIndexL
    lift $ unlessM ((epoch ==) <$> LrcDB.getEpoch) (lrcSingleShot epoch)
    -- We need to know leaders to create any block.
    leaders <- lift $ lrcActionOnEpochReason epoch "genBlock" LrcDB.getLeaders
    case eos of
        EpochOrSlot (Left _) -> do
            tipHeader <- lift $ getTipHeader @SscGodTossing
            let slot0 = SlotId epoch minBound
            let genesisBlock = mkGenesisBlock (Just tipHeader) epoch leaders
            fmap Just $ withCurrentSlot slot0 $ lift $ verifyAndApply (Left genesisBlock)
        EpochOrSlot (Right slot@SlotId {..}) -> withCurrentSlot slot $ do
            genPayload slot
            leader <-
                lift $ maybeThrow
                    (BGInternal "no leader")
                    (leaders ^? ix (fromIntegral $ getSlotIndex siSlot))
            secrets <-
                unInvSecretsMap . view asSecretKeys <$> view blockGenParams
            transCert <- lift $ getDlgTransPsk leader
            let creator = maybe leader (addressHash . pskDelegatePk . snd) transCert
            let maybeLeader = secrets ^. at creator
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
                    Just <$> usingPrimaryKey leaderSK
                             (lift $ genMainBlock slot (Right . swap <$> transCert))
  where
    genMainBlock ::
        HasCompileInfo =>
        SlotId ->
        ProxySKBlockInfo ->
        BlockGenMode (MempoolExt m) m (Blund SscGodTossing)
    genMainBlock slot proxySkInfo =
        createMainBlockInternal @SscGodTossing slot proxySkInfo >>= \case
            Left err -> throwM (BGFailedToCreate err)
            Right mainBlock -> verifyAndApply $ Right mainBlock
    verifyAndApply ::
        HasCompileInfo =>
        Block SscGodTossing -> BlockGenMode (MempoolExt m) m (Blund SscGodTossing)
    verifyAndApply block =
        verifyBlocksPrefix (one block) >>= \case
            Left err -> throwM (BGCreatedInvalid err)
            Right (undos, pollModifier) -> do
                let undo = undos ^. _Wrapped . _neHead
                    blund = (block, undo)
                applyBlocksUnsafe (ShouldCallBListener True) (one blund) (Just pollModifier)
                normalizeMempool
                pure blund
