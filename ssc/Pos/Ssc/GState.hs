{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Working with global SSC state.

module Pos.Ssc.GState
       (
       -- * Certs
         getGlobalCerts
       , gtGetGlobalState
       , getStableCerts

       -- * Seed
       , sscCalculateSeed

       -- * Global state
       , sscLoadGlobalState
       , sscGlobalStateToBatch

       -- * Blocks
       , sscRollbackBlocks
       , sscVerifyBlocks
       , sscApplyBlocks
       , sscVerifyAndApplyBlocks
       ) where

import           Control.Lens                     ((.=), _Wrapped)
import           Control.Monad.Except             (MonadError (throwError), runExceptT)
import           Control.Monad.Morph              (hoist)
import qualified Crypto.Random                    as Rand
import qualified Data.HashMap.Strict              as HM
import           Formatting                       (build, int, sformat, (%))
import           Serokell.Util                    (listJson)
import           System.Wlog                      (WithLogger, logDebug, logInfo)
import           Universum

import           Pos.Binary.GodTossing            ()
import           Pos.Core                         (BlockVersionData, EpochIndex (..),
                                                   HasConfiguration, HeaderHash,
                                                   SharedSeed, SlotId (..),
                                                   VssCertificatesMap (..), epochIndexL,
                                                   epochOrSlotG, headerHash, vcVssKey)
import           Pos.DB                           (MonadDBRead, MonadGState,
                                                   SomeBatchOp (..), gsAdoptedBVData)
import           Pos.Exception                    (assertionFailed)
import           Pos.Lrc.Context                  (HasLrcContext)
import           Pos.Lrc.Types                    (RichmenStakes)
import           Pos.Reporting.Methods            (MonadReporting, reportError)
import           Pos.Ssc.Core                     (SscPayload (..))
import           Pos.Ssc.Configuration            (HasSscConfiguration)
import qualified Pos.Ssc.GodTossing.DB            as DB
import           Pos.Ssc.GodTossing.Functions     (getStableCertsPure)
import           Pos.Ssc.GodTossing.Seed          (calculateSeed)
import           Pos.Ssc.GodTossing.Toss          (MultiRichmenStakes, PureToss,
                                                   applyGenesisBlock, rollbackGT,
                                                   runPureTossWithLogger,
                                                   supplyPureTossEnv,
                                                   verifyAndApplySscPayload)
import qualified Pos.Ssc.GodTossing.VssCertData   as VCD
import           Pos.Ssc.Lrc                      (getSscRichmenFromLrc)
import           Pos.Ssc.Mem                      (MonadSscMem, SscGlobalQuery,
                                                   SscGlobalUpdate, askSscMem,
                                                   sscRunGlobalQuery, sscRunGlobalUpdate)
import           Pos.Ssc.SeedError                (SscSeedError)
import           Pos.Ssc.Types                    (SscBlock (..), SscGlobalState (..),
                                                   sgsCommitments, sgsOpenings, sgsShares,
                                                   sgsVssCertificates, sscGlobal)
import           Pos.Ssc.VerifyError              (SscVerifyError (..),
                                                   sscIsCriticalError)
import           Pos.Util.Chrono                  (NE, NewestFirst (..), OldestFirst (..))
import           Pos.Util.Util                    (inAssertMode, _neHead, _neLast)

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

gtGetGlobalState
    :: (MonadSscMem ctx m, MonadIO m)
    => m SscGlobalState
gtGetGlobalState = sscRunGlobalQuery ask

getGlobalCerts
    :: (MonadSscMem ctx m, MonadIO m)
    => SlotId -> m VssCertificatesMap
getGlobalCerts sl =
    sscRunGlobalQuery $
        VCD.certs .
        VCD.setLastKnownSlot sl <$>
        view sgsVssCertificates

-- | Get stable VSS certificates for given epoch.
getStableCerts
    :: (HasSscConfiguration, HasConfiguration, MonadSscMem ctx m, MonadIO m)
    => EpochIndex -> m VssCertificatesMap
getStableCerts epoch =
    getStableCertsPure epoch <$> sscRunGlobalQuery (view sgsVssCertificates)

----------------------------------------------------------------------------
-- Queries & updates
----------------------------------------------------------------------------

-- | Calculate 'SharedSeed' for given epoch using 'SscGlobalState'.
sscCalculateSeed
    :: forall ctx m.
       ( MonadSscMem ctx m
       , MonadDBRead m
       , MonadReader ctx m
       , HasLrcContext ctx
       , MonadIO m
       , WithLogger m )
    => EpochIndex
    -> m (Either SscSeedError SharedSeed)
sscCalculateSeed epoch = do
    -- We take richmen for the previous epoch because during N-th epoch we
    -- were using richmen for N-th epoch for everything – so, when we are
    -- calculating the seed for N+1-th epoch, we should still use data from
    -- N-th epoch.
    richmen <- getSscRichmenFromLrc "sscCalculateSeed" (epoch - 1)
    sscRunGlobalQuery $ sscCalculateSeedQ epoch richmen

sscCalculateSeedQ
    :: EpochIndex
    -> RichmenStakes
    -> SscGlobalQuery (Either SscSeedError SharedSeed)
sscCalculateSeedQ _epoch richmen =
    calculateSeed
    <$> view sgsCommitments
    <*> (map vcVssKey . getVssCertificatesMap . VCD.certs <$>
         view sgsVssCertificates)
    <*> view sgsOpenings
    <*> view sgsShares
    <*> pure richmen

-- | Load global state from DB by recreating it from recent blocks.
sscLoadGlobalState :: (HasConfiguration, MonadDBRead m, WithLogger m) => m SscGlobalState
sscLoadGlobalState = do
    logDebug "Loading SSC global state"
    gs <- DB.getSscGlobalState
    gs <$ logInfo (sformat ("Loaded GodTossing state: " %build) gs)

-- | Dump global state to DB.
sscGlobalStateToBatch :: HasConfiguration => SscGlobalState -> [SomeBatchOp]
sscGlobalStateToBatch = one . SomeBatchOp . DB.gtGlobalStateToBatch

-- | Rollback application of given sequence of blocks. Bad things can
-- happen if these blocks haven't been applied before.
sscRollbackBlocks
    :: forall ctx m.
       SscGlobalApplyMode ctx m
    => NewestFirst NE SscBlock -> m [SomeBatchOp]
sscRollbackBlocks blocks = sscRunGlobalUpdate $ do
    sscRollbackU blocks
    sscGlobalStateToBatch <$> get

sscRollbackU
  :: (HasSscConfiguration, HasConfiguration)
  => NewestFirst NE SscBlock -> SscGlobalUpdate ()
sscRollbackU blocks = tossToUpdate $ rollbackGT oldestEOS payloads
  where
    oldestEOS = blocks ^. _Wrapped . _neLast . epochOrSlotG
    payloads = over _Wrapped (map snd . rights . map getSscBlock . toList)
                   blocks

-- | Verify SSC-related part of given blocks with respect to current GState
-- and apply them on success. Blocks must be from the same epoch.
sscVerifyAndApplyBlocks
    :: (HasSscConfiguration, HasConfiguration)
    => RichmenStakes
    -> BlockVersionData
    -> OldestFirst NE SscBlock
    -> SscVerifier ()
sscVerifyAndApplyBlocks richmenStake bvd blocks =
    verifyAndApplyMultiRichmen False (richmenData, bvd) blocks
  where
    epoch = blocks ^. _Wrapped . _neHead . epochIndexL
    richmenData = HM.fromList [(epoch, richmenStake)]

verifyAndApplyMultiRichmen
    :: (HasSscConfiguration, HasConfiguration)
    => Bool
    -> (MultiRichmenStakes, BlockVersionData)
    -> OldestFirst NE SscBlock
    -> SscVerifier ()
verifyAndApplyMultiRichmen onlyCerts env =
    tossToVerifier . hoist (supplyPureTossEnv env) .
    mapM_ (verifyAndApplyDo . getSscBlock)
  where
    verifyAndApplyDo (Left header) = applyGenesisBlock $ header ^. epochIndexL
    verifyAndApplyDo (Right (header, payload)) =
        verifyAndApplySscPayload (Right header) $
        filterPayload payload
    filterPayload payload
        | onlyCerts = leaveOnlyCerts payload
        | otherwise = payload
    leaveOnlyCerts (CommitmentsPayload _ certs) =
        CommitmentsPayload mempty certs
    leaveOnlyCerts (OpeningsPayload _ certs) = OpeningsPayload mempty certs
    leaveOnlyCerts (SharesPayload _ certs) = SharesPayload mempty certs
    leaveOnlyCerts c@(CertificatesPayload _) = c

tossToUpdate :: PureToss a -> SscGlobalUpdate a
tossToUpdate action = do
    oldState <- use identity
    (res, newState) <- runPureTossWithLogger oldState action
    (identity .= newState) $> res

tossToVerifier
    :: ExceptT SscVerifyError PureToss a
    -> SscVerifier a
tossToVerifier action = do
    oldState <- use identity
    (resOrErr, newState) <-
        runPureTossWithLogger oldState $ runExceptT action
    case resOrErr of
        Left e    -> throwError e
        Right res -> (identity .= newState) $> res

----------------------------------------------------------------------------
-- Stuff
----------------------------------------------------------------------------

type SscVerifyMode m =
    ( MonadState SscGlobalState m
    , WithLogger m
    , MonadError SscVerifyError m
    , Rand.MonadRandom m
    )

type SscVerifier a = forall m . SscVerifyMode m => m a

----------------------------------------------------------------------------
-- More stuff
----------------------------------------------------------------------------


-- | Apply sequence of definitely valid blocks. Global state which is
-- result of application of these blocks can be optionally passed as
-- argument (it can be calculated in advance using 'sscVerifyBlocks').
sscApplyBlocks
    :: forall ctx m.
       SscGlobalApplyMode ctx m
    => OldestFirst NE SscBlock
    -> Maybe SscGlobalState
    -> m [SomeBatchOp]
sscApplyBlocks blocks (Just newState) = do
    inAssertMode $ do
        let hashes = map headerHash blocks
        expectedState <- sscVerifyValidBlocks blocks
        if | newState == expectedState -> pass
           | otherwise -> onUnexpectedVerify hashes
    sscApplyBlocksFinish newState
sscApplyBlocks blocks Nothing =
    sscApplyBlocksFinish =<< sscVerifyValidBlocks blocks

sscApplyBlocksFinish
    :: forall ctx m . SscGlobalApplyMode ctx m
    => SscGlobalState -> m [SomeBatchOp]
sscApplyBlocksFinish gs = do
    sscRunGlobalUpdate (put gs)
    inAssertMode $
        logDebug $
        sformat ("After applying blocks SSC global state is:\n"%build) gs
    pure $ sscGlobalStateToBatch gs

sscVerifyValidBlocks
    :: forall ctx m.
       SscGlobalApplyMode ctx m
    => OldestFirst NE SscBlock -> m SscGlobalState
sscVerifyValidBlocks blocks =
    sscVerifyBlocks blocks >>= \case
        Left e -> onVerifyFailedInApply hashes e
        Right newState -> return newState
  where
    hashes = map headerHash blocks

onVerifyFailedInApply
    :: forall m a.
       (WithLogger m, MonadThrow m)
    => OldestFirst NE HeaderHash -> SscVerifyError -> m a
onVerifyFailedInApply hashes e = assertionFailed msg
  where
    fmt =
        "sscApplyBlocks: verification of blocks "%listJson%" failed: "%build
    msg = sformat fmt hashes e

onUnexpectedVerify
    :: forall m a.
       (WithLogger m, MonadThrow m)
    => OldestFirst NE HeaderHash -> m a
onUnexpectedVerify hashes = assertionFailed msg
  where
    fmt =
        "sscApplyBlocks: verfication of blocks "%listJson%
        " returned unexpected state"
    msg = sformat fmt hashes

-- | Verify sequence of blocks and return global state which
-- corresponds to application of given blocks. If blocks are invalid,
-- this function will return 'Left' with appropriate error.
-- All blocks must be from the same epoch.
sscVerifyBlocks ::
       forall ctx m. SscGlobalVerifyMode ctx m
    => OldestFirst NE SscBlock
    -> m (Either SscVerifyError SscGlobalState)
sscVerifyBlocks blocks = do
    let epoch = blocks ^. _Wrapped . _neHead . epochIndexL
    let lastEpoch = blocks ^. _Wrapped . _neLast . epochIndexL
    let differentEpochsMsg =
            sformat
                ("sscVerifyBlocks: different epochs ("%int%", "%int%")")
                epoch
                lastEpoch
    inAssertMode $ unless (epoch == lastEpoch) $
        assertionFailed differentEpochsMsg
    richmenSet <- getSscRichmenFromLrc "sscVerifyBlocks" epoch
    bvd <- gsAdoptedBVData
    globalVar <- sscGlobal <$> askSscMem
    gs <- atomically $ readTVar globalVar
    res <-
        runExceptT
            (execStateT (sscVerifyAndApplyBlocks richmenSet bvd blocks) gs)
    case res of
        Left e
            | sscIsCriticalError e ->
                reportError $ sformat ("Critical error in ssc: "%build) e
        _ -> pass
    return res

----------------------------------------------------------------------------
-- GState
----------------------------------------------------------------------------

-- 'MonadIO' is needed only for 'TVar' (@gromak hopes).
-- 'MonadRandom' is needed for crypto (@neongreen hopes).
type SscGlobalVerifyMode ctx m =
    (HasSscConfiguration,
     MonadSscMem ctx m,
     MonadReader ctx m, HasLrcContext ctx,
     MonadDBRead m, MonadGState m, WithLogger m, MonadReporting ctx m,
     MonadIO m, Rand.MonadRandom m)

type SscGlobalApplyMode ctx m = SscGlobalVerifyMode ctx m
