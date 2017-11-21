{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

-- | Blockchain generation logic.

module Pos.Generator.Block.Logic
       ( BlockTxpGenMode
       , genBlocks
       ) where

import           Universum

import           Control.Lens (at, ix, _Wrapped)
import           Control.Monad.Random.Strict (RandT, mapRandT)
import           Data.Default (Default (def))
import           Formatting (build, sformat, (%))
import           System.Random (RandomGen (..))
import           System.Wlog (logWarning)

import           Pos.AllSecrets (HasAllSecrets (..), unInvSecretsMap)
import           Pos.Block.Base (mkGenesisBlock)
import           Pos.Block.Logic (applyBlocksUnsafe, createMainBlockInternal, normalizeMempool,
                                  verifyBlocksPrefix)
import           Pos.Block.Slog (ShouldCallBListener (..))
import           Pos.Block.Types (Blund)
import           Pos.Communication.Limits ()
import           Pos.Communication.Message ()
import           Pos.Communication.Limits (HasSscLimits)
import           Pos.Core (EpochOrSlot (..), SlotId (..), addressHash, epochIndexL, getEpochOrSlot,
                           getSlotIndex)
import           Pos.Core.Block (Block)
import           Pos.Crypto (pskDelegatePk)
import qualified Pos.DB.BlockIndex as DB
import           Pos.Delegation.Logic (getDlgTransPsk)
import           Pos.Delegation.Types (ProxySKBlockInfo)
import           Pos.Generator.Block.Error (BlockGenError (..))
import           Pos.Generator.Block.Mode (BlockGenMode, BlockGenRandMode, MonadBlockGen,
                                           MonadBlockGenInit, mkBlockGenContext, usingPrimaryKey,
                                           withCurrentSlot)
import           Pos.Generator.Block.Param (BlockGenParams, HasBlockGenParams (..))
import           Pos.Generator.Block.Payload (genPayload)
import           Pos.Lrc (lrcSingleShot)
import           Pos.Lrc.Context (lrcActionOnEpochReason)
import qualified Pos.Lrc.DB as LrcDB
import           Pos.Txp (MempoolExt, MonadTxpLocal, TxpGlobalSettings)
import           Pos.Util (HasLens', maybeThrow, _neHead)
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)

----------------------------------------------------------------------------
-- Block generation
----------------------------------------------------------------------------

type BlockTxpGenMode g ctx m =
    ( RandomGen g
    , MonadBlockGenInit ctx m
    , HasLens' ctx TxpGlobalSettings
    , Default (MempoolExt m)
    , MonadTxpLocal (BlockGenMode (MempoolExt m) m)
    , HasSscLimits (BlockGenMode (MempoolExt m) m)
    )

-- | Generate an arbitrary sequence of valid blocks. The blocks are
-- valid with respect to the global state right before this function
-- call.
-- The blocks themselves can be combined and retained according to some monoid.
-- Intermediate results will be forced. Blocks can be generated, written to
-- disk, then collected by using '()' as the monoid and 'const ()' as the
-- injector, for example.
genBlocks ::
       forall g ctx m t . (BlockTxpGenMode g ctx m, Monoid t)
    => BlockGenParams
    -> (Maybe Blund -> t)
    -> RandT g m t
genBlocks params inj = do
    ctx <- lift $ mkBlockGenContext @(MempoolExt m) params
    mapRandT (`runReaderT` ctx) genBlocksDo
  where
    genBlocksDo = do
        let numberOfBlocks = params ^. bgpBlockCount
        tipEOS <- getEpochOrSlot <$> lift DB.getTipHeader
        let startEOS = succ tipEOS
        let finishEOS = toEnum $ fromEnum tipEOS + fromIntegral numberOfBlocks
        foldM' genOneBlock mempty [startEOS .. finishEOS]

    genOneBlock t eos = ((t <>) . inj) <$> genBlock eos

    foldM' combine = go
      where
      go !base []     = return base
      go !base (x:xs) = combine base x >>= flip go xs

-- Generate a valid 'Block' for the given epoch or slot (genesis block
-- in the former case and main block the latter case) and apply it.
genBlock ::
       forall g ctx m.
       ( RandomGen g
       , MonadBlockGen ctx m
       , Default (MempoolExt m)
       , MonadTxpLocal (BlockGenMode (MempoolExt m) m)
       , HasSscLimits (BlockGenMode (MempoolExt m) m)
       )
    => EpochOrSlot
    -> BlockGenRandMode (MempoolExt m) g m (Maybe Blund)
genBlock eos = withCompileInfo def $ do
    let epoch = eos ^. epochIndexL
    lift $ unlessM ((epoch ==) <$> LrcDB.getEpoch) (lrcSingleShot epoch)
    -- We need to know leaders to create any block.
    leaders <- lift $ lrcActionOnEpochReason epoch "genBlock" LrcDB.getLeadersForEpoch
    case eos of
        EpochOrSlot (Left _) -> do
            tipHeader <- lift DB.getTipHeader
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
                             (lift $ genMainBlock slot (swap <$> transCert))
  where
    genMainBlock ::
        HasCompileInfo =>
        SlotId ->
        ProxySKBlockInfo ->
        BlockGenMode (MempoolExt m) m Blund
    genMainBlock slot proxySkInfo =
        createMainBlockInternal slot proxySkInfo >>= \case
            Left err -> throwM (BGFailedToCreate err)
            Right mainBlock -> verifyAndApply $ Right mainBlock
    verifyAndApply ::
        HasCompileInfo =>
        Block -> BlockGenMode (MempoolExt m) m Blund
    verifyAndApply block =
        verifyBlocksPrefix (one block) >>= \case
            Left err -> throwM (BGCreatedInvalid err)
            Right (undos, pollModifier) -> do
                let undo = undos ^. _Wrapped . _neHead
                    blund = (block, undo)
                applyBlocksUnsafe (ShouldCallBListener True) (one blund) (Just pollModifier)
                normalizeMempool
                pure blund
