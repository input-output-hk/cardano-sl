{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
-- Ignore the Semigroup + Monoid constraints, that ghc8.4 complains about.
{-# OPTIONS_GHC -Wno-redundant-constraints    #-}

-- | Blockchain generation logic.

module Pos.Generator.Block.Logic
       ( BlockTxpGenMode
       , genBlockNoApply
       , genBlocks
       ) where

import           Universum

import           Control.Lens (at, ix, _Wrapped)
import           Control.Monad.Random.Strict (RandT, mapRandT)
import           Data.Default (Default)
import           Formatting (build, sformat, (%))
import           System.Random (RandomGen (..))
import           System.Wlog (logWarning)

import           Pos.AllSecrets (HasAllSecrets (..), unInvSecretsMap)
import           Pos.Chain.Block (Blund)
import           Pos.Chain.Delegation (ProxySKBlockInfo)
import           Pos.Chain.Txp (HasTxpConfiguration)
import           Pos.Core (EpochOrSlot (..), SlotId (..), addressHash,
                     epochIndexL, getEpochOrSlot, getSlotIndex)
import           Pos.Core.Block (Block, BlockHeader)
import           Pos.Core.Block.Constructors (mkGenesisBlock)
import           Pos.Crypto (ProtocolMagic, pskDelegatePk)
import           Pos.DB.Block (ShouldCallBListener (..),
                     VerifyBlocksContext (..), applyBlocksUnsafe,
                     createMainBlockInternal, getVerifyBlocksContext,
                     getVerifyBlocksContext', lrcSingleShot, normalizeMempool,
                     verifyBlocksPrefix)
import qualified Pos.DB.BlockIndex as DB
import           Pos.DB.Delegation (getDlgTransPsk)
import           Pos.DB.Lrc (lrcActionOnEpochReason)
import qualified Pos.DB.Lrc as LrcDB
import           Pos.DB.Txp (MempoolExt, MonadTxpLocal, TxpGlobalSettings)
import           Pos.Generator.Block.Error (BlockGenError (..))
import           Pos.Generator.Block.Mode (BlockGenMode, BlockGenRandMode,
                     MonadBlockGen, MonadBlockGenInit, mkBlockGenContext,
                     usingPrimaryKey, withCurrentSlot)
import           Pos.Generator.Block.Param (BlockGenParams,
                     HasBlockGenParams (..))
import           Pos.Generator.Block.Payload (genPayload)
import           Pos.Util (HasLens', maybeThrow, _neHead)

----------------------------------------------------------------------------
-- Block generation
----------------------------------------------------------------------------

type BlockTxpGenMode g ctx m =
    ( RandomGen g
    , MonadBlockGenInit ctx m
    , HasLens' ctx TxpGlobalSettings
    , Default (MempoolExt m)
    , MonadTxpLocal (BlockGenMode (MempoolExt m) m)
    )

foldM' :: forall a t m. Monad m => (a -> t -> m a) -> a -> [t] -> m a
foldM' combine = go
    where
    go !base []     = return base
    go !base (x:xs) = combine base x >>= flip go xs

-- | Generate an arbitrary sequence of valid blocks. The blocks are
-- valid with respect to the global state right before this function
-- call.
-- The blocks themselves can be combined and retained according to some monoid.
-- Intermediate results will be forced. Blocks can be generated, written to
-- disk, then collected by using '()' as the monoid and 'const ()' as the
-- injector, for example.
genBlocks ::
       forall g ctx m t . (HasTxpConfiguration, BlockTxpGenMode g ctx m, Semigroup t, Monoid t)
    => ProtocolMagic
    -> BlockGenParams
    -> (Maybe Blund -> t)
    -> RandT g m t
genBlocks pm params inj = do
    ctx <- lift $ mkBlockGenContext @(MempoolExt m) params
    mapRandT (`runReaderT` ctx) genBlocksDo
  where
    genBlocksDo :: RandT g (BlockGenMode (MempoolExt m) m) t
    genBlocksDo = do
        let numberOfBlocks = params ^. bgpBlockCount
        tipEOS <- getEpochOrSlot <$> lift DB.getTipHeader
        let startEOS = succ tipEOS
        let finishEOS = toEnum $ fromEnum tipEOS + fromIntegral numberOfBlocks
        foldM' genOneBlock mempty [startEOS .. finishEOS]

    genOneBlock
        :: t
        -> EpochOrSlot
        -> RandT g (BlockGenMode (MempoolExt m) m) t
    genOneBlock t eos = ((t <>) . inj) <$> genBlock pm eos

-- | Generate a 'Block' for the given epoch or slot (geneis block in the formet
-- case and main block in the latter case) and do not apply it.
genBlockNoApply
    :: forall g ctx m.
       ( RandomGen g
       , MonadBlockGen ctx m
       , Default (MempoolExt m)
       , MonadTxpLocal (BlockGenMode (MempoolExt m) m)
       , HasTxpConfiguration
       )
    => ProtocolMagic
    -> EpochOrSlot
    -> BlockHeader -- ^ previoud block header
    -> BlockGenRandMode (MempoolExt m) g m (Maybe Block)
genBlockNoApply pm eos header = do
    let epoch = eos ^. epochIndexL
    lift $ unlessM ((epoch ==) <$> LrcDB.getEpoch) (lrcSingleShot pm epoch)
    -- We need to know leaders to create any block.
    leaders <- lift $ lrcActionOnEpochReason epoch "genBlock" LrcDB.getLeadersForEpoch
    case eos of
        EpochOrSlot (Left _) -> do
            let genesisBlock = mkGenesisBlock pm (Right header) epoch leaders
            return $ Just $ Left genesisBlock
        EpochOrSlot (Right slot@SlotId {..}) -> withCurrentSlot slot $ do
            genPayload pm slot
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
        SlotId ->
        ProxySKBlockInfo ->
        BlockGenMode (MempoolExt m) m Block
    genMainBlock slot proxySkInfo =
        createMainBlockInternal pm slot proxySkInfo >>= \case
            Left err -> throwM (BGFailedToCreate err)
            Right mainBlock -> return $ Right mainBlock

-- | Generate a valid 'Block' for the given epoch or slot (genesis block
-- in the former case and main block the latter case) and apply it.
genBlock ::
       forall g ctx m.
       ( RandomGen g
       , MonadBlockGen ctx m
       , Default (MempoolExt m)
       , MonadTxpLocal (BlockGenMode (MempoolExt m) m)
       , HasTxpConfiguration
       )
    => ProtocolMagic
    -> EpochOrSlot
    -> BlockGenRandMode (MempoolExt m) g m (Maybe Blund)
genBlock pm eos = do
    let epoch = eos ^. epochIndexL
    tipHeader <- lift DB.getTipHeader
    genBlockNoApply pm eos tipHeader >>= \case
        Just block@Left{}   -> do
            let slot0 = SlotId epoch minBound
            ctx <- getVerifyBlocksContext' (Just slot0)
            fmap Just $ withCurrentSlot slot0 $ lift $ verifyAndApply ctx block
        Just block@Right {} -> do
            ctx <- getVerifyBlocksContext
            fmap Just $ lift $ verifyAndApply ctx block
        Nothing -> return Nothing
    where
    verifyAndApply
        :: VerifyBlocksContext
        -> Block
        -> BlockGenMode (MempoolExt m) m Blund
    verifyAndApply ctx block =
        verifyBlocksPrefix pm ctx (one block) >>= \case
            Left err -> throwM (BGCreatedInvalid err)
            Right (undos, pollModifier) -> do
                let undo = undos ^. _Wrapped . _neHead
                    blund = (block, undo)
                applyBlocksUnsafe pm
                    (vbcBlockVersion ctx)
                    (vbcBlockVersionData ctx)
                    (ShouldCallBListener True)
                    (one blund)
                    (Just pollModifier)
                normalizeMempool pm
                pure blund
