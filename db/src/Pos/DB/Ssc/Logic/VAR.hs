{-# LANGUAGE RankNTypes #-}

-- | Block verification, application, and rollback: processing of SSC-related payload.

module Pos.DB.Ssc.Logic.VAR
       (
         sscVerifyBlocks
       , sscApplyBlocks
       , sscVerifyAndApplyBlocks
       , sscRollbackBlocks
       ) where

import           Control.Lens ((.=), _Wrapped)
import           Control.Monad.Except (MonadError (throwError), runExceptT)
import           Control.Monad.Morph (hoist)
import qualified Crypto.Random as Rand
import qualified Data.HashMap.Strict as HM
import           Formatting (build, int, sformat, (%))
import           Serokell.Util (listJson)
import           Universum

import           Pos.Chain.Block (ComponentBlock (..), HeaderHash, headerHash)
import           Pos.Chain.Lrc (RichmenStakes)
import           Pos.Chain.Ssc (HasSscConfiguration, MonadSscMem,
                     MultiRichmenStakes, PureToss, SscBlock,
                     SscGlobalState (..), SscGlobalUpdate, SscVerifyError (..),
                     applyGenesisBlock, askSscMem, pureTossTrace,
                     pureTossWithEnvTrace, rollbackSsc, runPureTossWithLogger,
                     sscGlobal, sscIsCriticalVerifyError, sscRunGlobalUpdate,
                     supplyPureTossEnv, verifyAndApplySscPayload)
import           Pos.Core (HasCoreConfiguration, HasGenesisData,
                     HasProtocolConstants, epochIndexL, epochOrSlotG)
import           Pos.Core.Chrono (NE, NewestFirst (..), OldestFirst (..))
import           Pos.Core.Exception (assertionFailed)
import           Pos.Core.Reporting (MonadReporting, reportError)
import           Pos.Core.Ssc (SscPayload (..))
import           Pos.Core.Update (BlockVersionData)
import           Pos.Crypto (ProtocolMagic)
import           Pos.DB (MonadDBRead, MonadGState, SomeBatchOp (..))
import           Pos.DB.Lrc (HasLrcContext, getSscRichmen)
import qualified Pos.DB.Ssc.GState as DB
import           Pos.Util.AssertMode (inAssertMode)
import           Pos.Util.Lens (_neHead, _neLast)
import           Pos.Util.Trace (noTrace)
import           Pos.Util.Trace.Named (TraceNamed, logDebug, natTrace)

----------------------------------------------------------------------------
-- Modes
----------------------------------------------------------------------------

type SscVerifyMode m =
    ( MonadState SscGlobalState m
    , MonadError SscVerifyError m
    , Rand.MonadRandom m
    )

-- 'MonadIO' is needed only for 'TVar' (@gromak hopes).
-- 'MonadRandom' is needed for crypto (@neongreen hopes).
type SscGlobalVerifyMode ctx m =
    (HasSscConfiguration,
     MonadSscMem ctx m,
     MonadReader ctx m, HasLrcContext ctx,
     MonadDBRead m, MonadGState m, MonadReporting m,
     MonadIO m, Rand.MonadRandom m)

type SscGlobalApplyMode ctx m = SscGlobalVerifyMode ctx m

----------------------------------------------------------------------------
-- Verify
----------------------------------------------------------------------------

-- | Verify sequence of blocks and return global state which
-- corresponds to application of given blocks. If blocks are invalid,
-- this function will return 'Left' with appropriate error.
-- All blocks must be from the same epoch.
sscVerifyBlocks
    :: SscGlobalVerifyMode ctx m
    => TraceNamed m
    -> ProtocolMagic
    -> BlockVersionData
    -> OldestFirst NE SscBlock
    -> m (Either SscVerifyError SscGlobalState)
sscVerifyBlocks logTrace pm bvd blocks = do
    let epoch = blocks ^. _Wrapped . _neHead . epochIndexL
    let lastEpoch = blocks ^. _Wrapped . _neLast . epochIndexL
    let differentEpochsMsg =
            sformat
                ("sscVerifyBlocks: different epochs ("%int%", "%int%")")
                epoch
                lastEpoch
    inAssertMode $ unless (epoch == lastEpoch) $
        assertionFailed logTrace differentEpochsMsg
    richmenSet <- getSscRichmen "sscVerifyBlocks" epoch
    globalVar <- sscGlobal <$> askSscMem
    gs <- readTVarIO globalVar
    res <-
        runExceptT
            (execStateT (sscVerifyAndApplyBlocks (natTrace (lift . lift) logTrace) pm richmenSet bvd blocks) gs)
    case res of
        Left e
            | sscIsCriticalVerifyError e ->
                reportError $ sformat ("Critical error in ssc: "%build) e
        _ -> pass
    return res

----------------------------------------------------------------------------
-- Apply
----------------------------------------------------------------------------

-- | Apply sequence of definitely valid blocks. Global state which is
-- result of application of these blocks can be optionally passed as
-- argument (it can be calculated in advance using 'sscVerifyBlocks').
sscApplyBlocks
    :: SscGlobalApplyMode ctx m
    => TraceNamed m
    -> ProtocolMagic
    -> BlockVersionData
    -> OldestFirst NE SscBlock
    -> Maybe SscGlobalState
    -> m [SomeBatchOp]
sscApplyBlocks logTrace pm bvd blocks (Just newState) = do
    inAssertMode $ do
        let hashes = map headerHash blocks
        expectedState <- sscVerifyValidBlocks logTrace pm bvd blocks
        if | newState == expectedState -> pass
           | otherwise -> onUnexpectedVerify logTrace hashes
    sscApplyBlocksFinish logTrace newState
sscApplyBlocks logTrace pm bvd blocks Nothing =
    sscApplyBlocksFinish logTrace =<< sscVerifyValidBlocks logTrace pm bvd blocks

sscApplyBlocksFinish
    :: SscGlobalApplyMode ctx m
    => TraceNamed m -> SscGlobalState -> m [SomeBatchOp]
sscApplyBlocksFinish logTrace gs = do
    sscRunGlobalUpdate logTrace (put gs)
    inAssertMode $
        logDebug logTrace $
            sformat ("After applying blocks SSC global state is:\n"%build) gs
    pure $ sscGlobalStateToBatch gs

sscVerifyValidBlocks
    :: SscGlobalApplyMode ctx m
    => TraceNamed m
    -> ProtocolMagic
    -> BlockVersionData
    -> OldestFirst NE SscBlock
    -> m SscGlobalState
sscVerifyValidBlocks logTrace pm bvd blocks =
    sscVerifyBlocks logTrace pm bvd blocks >>= \case
        Left e -> onVerifyFailedInApply logTrace hashes e
        Right newState -> return newState
  where
    hashes = map headerHash blocks

onUnexpectedVerify
    :: forall m a.
       (MonadThrow m)
    => TraceNamed m -> OldestFirst NE HeaderHash -> m a
onUnexpectedVerify logTrace hashes =
    assertionFailed logTrace msg
  where
    fmt =
        "sscApplyBlocks: verfication of blocks "%listJson%
        " returned unexpected state"
    msg = sformat fmt hashes

onVerifyFailedInApply
    :: forall m a.
       (MonadThrow m)
    => TraceNamed m -> OldestFirst NE HeaderHash -> SscVerifyError -> m a
onVerifyFailedInApply logTrace hashes e = assertionFailed logTrace msg
  where
    fmt =
        "sscApplyBlocks: verification of blocks "%listJson%" failed: "%build
    msg = sformat fmt hashes e

----------------------------------------------------------------------------
-- Verify and apply
----------------------------------------------------------------------------

-- | Verify SSC-related part of given blocks with respect to current GState
-- and apply them on success. Blocks must be from the same epoch.
sscVerifyAndApplyBlocks
    :: (SscVerifyMode m, HasProtocolConstants, HasGenesisData)
    => TraceNamed m
    -> ProtocolMagic
    -> RichmenStakes
    -> BlockVersionData
    -> OldestFirst NE SscBlock
    -> m ()
sscVerifyAndApplyBlocks logTrace pm richmenStake bvd blocks =
    verifyAndApplyMultiRichmen logTrace pm False (richmenData, bvd) blocks
  where
    epoch = blocks ^. _Wrapped . _neHead . epochIndexL
    richmenData = HM.fromList [(epoch, richmenStake)]

verifyAndApplyMultiRichmen
    :: (SscVerifyMode m, HasProtocolConstants, HasGenesisData)
    => TraceNamed m
    -> ProtocolMagic
    -> Bool
    -> (MultiRichmenStakes, BlockVersionData)
    -> OldestFirst NE SscBlock
    -> m ()
verifyAndApplyMultiRichmen logTrace pm onlyCerts env =
    tossToVerifier logTrace . hoist (supplyPureTossEnv env) .
    mapM_ verifyAndApplyDo
  where
    verifyAndApplyDo (ComponentBlockGenesis header) = applyGenesisBlock $ header ^. epochIndexL
    verifyAndApplyDo (ComponentBlockMain header payload) =
        verifyAndApplySscPayload (natTrace lift pureTossWithEnvTrace) pm (Right header) $
            filterPayload payload
    filterPayload payload
        | onlyCerts = leaveOnlyCerts payload
        | otherwise = payload
    leaveOnlyCerts (CommitmentsPayload _ certs) =
        CommitmentsPayload mempty certs
    leaveOnlyCerts (OpeningsPayload _ certs) = OpeningsPayload mempty certs
    leaveOnlyCerts (SharesPayload _ certs) = SharesPayload mempty certs
    leaveOnlyCerts c@(CertificatesPayload _) = c

----------------------------------------------------------------------------
-- Rollback
----------------------------------------------------------------------------

-- | Rollback application of given sequence of blocks. Bad things can
-- happen if these blocks haven't been applied before.
sscRollbackBlocks
    :: SscGlobalApplyMode ctx m
    => TraceNamed m -> NewestFirst NE SscBlock -> m [SomeBatchOp]
sscRollbackBlocks logTrace blocks = sscRunGlobalUpdate logTrace $ do
    sscRollbackU blocks
    sscGlobalStateToBatch <$> get

sscRollbackU
  :: (HasProtocolConstants, HasGenesisData)
  => NewestFirst NE SscBlock -> SscGlobalUpdate ()
sscRollbackU blocks = tossToUpdate $ rollbackSsc pureTossTrace oldestEOS payloads
  where
    oldestEOS = blocks ^. _Wrapped . _neLast . epochOrSlotG
    payloads = NewestFirst $ mapMaybe extractPayload $ toList blocks
    extractPayload (ComponentBlockMain _ a)  = Just a
    extractPayload (ComponentBlockGenesis _) = Nothing

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

tossToUpdate :: PureToss a -> SscGlobalUpdate a
tossToUpdate action = do
    oldState <- use identity
    (res, newState) <- runPureTossWithLogger noTrace oldState action
    (identity .= newState) $> res

tossToVerifier
    :: SscVerifyMode m
    => TraceNamed m
    -> ExceptT SscVerifyError PureToss a
    -> m a
tossToVerifier logTrace action = do
    oldState <- use identity
    (resOrErr, newState) <-
        runPureTossWithLogger logTrace oldState $ runExceptT action
    case resOrErr of
        Left e    -> throwError e
        Right res -> (identity .= newState) $> res

-- | Dump global state to DB.
sscGlobalStateToBatch :: (HasCoreConfiguration) => SscGlobalState -> [SomeBatchOp]
sscGlobalStateToBatch = one . SomeBatchOp . DB.sscGlobalStateToBatch
