{-# LANGUAGE RankNTypes #-}

-- | Block verification, application, and rollback: processing of SSC-related payload.

module Pos.Ssc.Logic.VAR
       (
         sscVerifyBlocks
       , sscApplyBlocks
       , sscVerifyAndApplyBlocks
       , sscRollbackBlocks
       ) where

import           Control.Lens ((.=), _Wrapped)
import           Control.Monad.Writer (tell)
import           Control.Monad.Except (MonadError (throwError), runExceptT)
import           Control.Monad.Morph (hoist)
import qualified Crypto.Random as Rand
import qualified Data.HashMap.Strict as HM
import           Data.Functor.Contravariant (contramap)
import           Formatting (build, int, sformat, (%))
import           Serokell.Util (listJson)
import           Universum

import           Pos.Binary.Ssc ()
import           Pos.Core (BlockVersionData, ComponentBlock (..), HasCoreConfiguration, HasGenesisData,
                           HasProtocolConstants, HasProtocolMagic, HeaderHash, epochIndexL,
                           epochOrSlotG, headerHash)
import           Pos.Core.Ssc (SscPayload (..))
import           Pos.DB (MonadDBRead, MonadGState, SomeBatchOp (..), gsAdoptedBVData)
import           Pos.Exception (assertionFailed)
import           Pos.Lrc.Consumer.Ssc (getSscRichmen)
import           Pos.Lrc.Context (HasLrcContext)
import           Pos.Lrc.Types (RichmenStakes)
import           Pos.Reporting.Methods (MonadReporting, reportError)
import           Pos.Ssc.Configuration (HasSscConfiguration)
import qualified Pos.Ssc.DB as DB
import           Pos.Ssc.Error (SscVerifyError (..), sscIsCriticalVerifyError)
import           Pos.Ssc.Mem (MonadSscMem, SscGlobalUpdate, askSscMem, sscRunGlobalUpdate)
import           Pos.Ssc.Toss (MultiRichmenStakes, PureToss, applyGenesisBlock, rollbackSsc,
                               runPureToss, supplyPureTossEnv, verifyAndApplySscPayload,
                               pureTossTrace, pureTossWithEnvTrace)
import           Pos.Ssc.Types (SscBlock, SscGlobalState (..), sscGlobal)
import           Pos.Util.AssertMode (inAssertMode)
import           Pos.Util.Chrono (NE, NewestFirst (..), OldestFirst (..))
import           Pos.Util.Lens (_neHead, _neLast)
import           Pos.Util.Trace (Trace, traceWith, natTrace)
import           Pos.Util.Trace.Unstructured (LogItem, logDebug, publicPrivateLogItem)

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
sscVerifyBlocks :: SscGlobalVerifyMode ctx m
    => Trace m LogItem
    -> OldestFirst NE SscBlock
    -> m (Either SscVerifyError SscGlobalState)
sscVerifyBlocks logTrace blocks = do
    let epoch = blocks ^. _Wrapped . _neHead . epochIndexL
    let lastEpoch = blocks ^. _Wrapped . _neLast . epochIndexL
    let differentEpochsMsg =
            sformat
                ("sscVerifyBlocks: different epochs ("%int%", "%int%")")
                epoch
                lastEpoch
    inAssertMode $ unless (epoch == lastEpoch) $
        assertionFailed (contramap publicPrivateLogItem logTrace) differentEpochsMsg
    richmenSet <- getSscRichmen "sscVerifyBlocks" epoch
    bvd <- gsAdoptedBVData
    globalVar <- sscGlobal <$> askSscMem
    gs <- atomically $ readTVar globalVar
    res <-
        runExceptT
            (execStateT (sscVerifyAndApplyBlocks (natTrace (lift . lift) logTrace) richmenSet bvd blocks) gs)
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
    => Trace m LogItem
    -> OldestFirst NE SscBlock
    -> Maybe SscGlobalState
    -> m [SomeBatchOp]
sscApplyBlocks logTrace blocks (Just newState) = do
    inAssertMode $ do
        let hashes = map headerHash blocks
        expectedState <- sscVerifyValidBlocks logTrace blocks
        if | newState == expectedState -> pass
           | otherwise -> onUnexpectedVerify logTrace hashes
    sscApplyBlocksFinish logTrace newState
sscApplyBlocks logTrace blocks Nothing =
    sscApplyBlocksFinish logTrace =<< sscVerifyValidBlocks logTrace blocks

sscApplyBlocksFinish
    :: (SscGlobalApplyMode ctx m)
    => Trace m LogItem -> SscGlobalState -> m [SomeBatchOp]
sscApplyBlocksFinish logTrace gs = do
    sscRunGlobalUpdate logTrace (put gs)
    inAssertMode $
        logDebug logTrace $ 
        sformat ("After applying blocks SSC global state is:\n"%build) gs
    pure $ sscGlobalStateToBatch gs

sscVerifyValidBlocks
    :: SscGlobalApplyMode ctx m
    => Trace m LogItem -> OldestFirst NE SscBlock -> m SscGlobalState
sscVerifyValidBlocks logTrace blocks =
    sscVerifyBlocks logTrace blocks >>= \case
        Left e -> onVerifyFailedInApply logTrace hashes e
        Right newState -> return newState
  where
    hashes = map headerHash blocks

onUnexpectedVerify
    :: forall m a.
       (MonadThrow m)
    => Trace m LogItem -> OldestFirst NE HeaderHash -> m a
onUnexpectedVerify logTrace hashes = 
    assertionFailed (contramap publicPrivateLogItem logTrace) msg
  where
    fmt =
        "sscApplyBlocks: verfication of blocks "%listJson%
        " returned unexpected state"
    msg = sformat fmt hashes

onVerifyFailedInApply
    :: forall m a.
       (MonadThrow m)
    => Trace m LogItem -> OldestFirst NE HeaderHash -> SscVerifyError -> m a
onVerifyFailedInApply logTrace hashes e = 
    assertionFailed (contramap publicPrivateLogItem logTrace) msg
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
    :: (SscVerifyMode m, HasProtocolConstants, HasProtocolMagic, HasGenesisData)
    => Trace m LogItem
    -> RichmenStakes
    -> BlockVersionData
    -> OldestFirst NE SscBlock
    -> m ()
sscVerifyAndApplyBlocks logTrace richmenStake bvd blocks =
    verifyAndApplyMultiRichmen logTrace False (richmenData, bvd) blocks
  where
    epoch = blocks ^. _Wrapped . _neHead . epochIndexL
    richmenData = HM.fromList [(epoch, richmenStake)]

verifyAndApplyMultiRichmen
    :: (SscVerifyMode m, HasProtocolConstants, HasProtocolMagic, HasGenesisData)
    => Trace m LogItem
    -> Bool
    -> (MultiRichmenStakes, BlockVersionData)
    -> OldestFirst NE SscBlock
    -> m ()
verifyAndApplyMultiRichmen logTrace onlyCerts env =
    tossToVerifier logTrace . hoist (supplyPureTossEnv env) .
    mapM_ verifyAndApplyDo
  where
    verifyAndApplyDo (ComponentBlockGenesis header) = applyGenesisBlock $ header ^. epochIndexL
    verifyAndApplyDo (ComponentBlockMain header payload) =
        verifyAndApplySscPayload (natTrace lift pureTossWithEnvTrace) (Right header) $
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
    => Trace m LogItem -> NewestFirst NE SscBlock -> m [SomeBatchOp]
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
    (res, newState, logItems) <- runPureToss oldState action
    tell logItems
    (identity .= newState) $> res

tossToVerifier
    :: SscVerifyMode m
    => Trace m LogItem
    -> ExceptT SscVerifyError PureToss a
    -> m a
tossToVerifier logTrace action = do
    oldState <- use identity
    (resOrErr, newState, logItems) <-
        runPureToss oldState $ runExceptT action
    forM_ logItems (traceWith logTrace)
    case resOrErr of
        Left e    -> throwError e
        Right res -> (identity .= newState) $> res

-- | Dump global state to DB.
sscGlobalStateToBatch :: (HasCoreConfiguration) => SscGlobalState -> [SomeBatchOp]
sscGlobalStateToBatch = one . SomeBatchOp . DB.sscGlobalStateToBatch
