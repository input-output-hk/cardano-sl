-- | Execution mode used by blockchain generator.

module Pos.Generator.Block.Mode
       ( MonadBlockGenBase
       , MonadBlockGen
       , BlockGenContext (..)
       , BlockGenMode
       , mkBlockGenContext

       , usingPrimaryKey
       , withCurrentSlot

       , InitBlockGenContext (..) -- useless
       ) where

import           Universum

import           Control.Lens.TH             (makeLensesWith)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Mockable                    (MonadMockable, Promise)
import           System.Wlog                 (WithLogger, logWarning)

import           Pos.Block.BListener         (MonadBListener (..), onApplyBlocksStub,
                                              onRollbackBlocksStub)
import           Pos.Block.Core              (Block, BlockHeader)
import           Pos.Block.Slog              (HasSlogContext (..), SlogContext,
                                              cloneSlogContext)
import           Pos.Block.Types             (Undo)
import           Pos.Core                    (HasPrimaryKey (..), IsHeader, SlotId (..),
                                              Timestamp, epochOrSlotToSlot,
                                              getEpochOrSlot)
import           Pos.Crypto                  (SecretKey)
import           Pos.DB                      (DBPureVar, MonadBlockDBGeneric (..),
                                              MonadBlockDBGenericWrite (..), MonadDB,
                                              MonadDBRead)
import qualified Pos.DB                      as DB
import qualified Pos.DB.Block                as BDB
import           Pos.DB.DB                   (getTipHeader, gsAdoptedBVDataDefault)
import           Pos.Delegation              (DelegationVar, mkDelegationVar)
import           Pos.Discovery               (DiscoveryContextSum (..),
                                              HasDiscoveryContextSum (..),
                                              MonadDiscovery (..), findPeersSum,
                                              getPeersSum)
import           Pos.Exception               (reportFatalError)
import           Pos.Generator.Block.Param   (BlockGenParams, HasBlockGenParams (..))
import           Pos.Launcher.Mode           (newInitFuture)
import           Pos.Lrc                     (LrcContext (..), cloneLrcContext)
import           Pos.Reporting               (HasReportingContext (..), ReportingContext,
                                              emptyReportingContext)
import           Pos.Slotting                (HasSlottingVar (..), MonadSlots (..),
                                              SlottingData, currentTimeSlottingSimple)
import           Pos.Slotting.MemState       (MonadSlotsData (..), cloneSlottingVar,
                                              getSlottingDataDefault,
                                              getSystemStartDefault,
                                              putSlottingDataDefault,
                                              waitPenultEpochEqualsDefault)
import           Pos.Ssc.Class               (SscBlock)
import           Pos.Ssc.Extra               (SscMemTag, SscState, mkSscState)
import           Pos.Ssc.GodTossing          (SscGodTossing)
import           Pos.Txp                     (GenericTxpLocalData, TxpGlobalSettings,
                                              TxpHolderTag, TxpMetrics, ignoreTxpMetrics,
                                              mkTxpLocalData, txpGlobalSettings)
import           Pos.Update.Context          (UpdateContext, mkUpdateContext)
import           Pos.Util                    (HasLens (..), Some, postfixLFields)
import           Pos.WorkMode.Class          (TxpExtra_TMP)

----------------------------------------------------------------------------
-- Constraint
----------------------------------------------------------------------------

-- | A set of constraints imposed on the base monad used for
-- arbitrary blockchain generation.
type MonadBlockGenBase m
     = ( WithLogger m
       , MonadMask m
       , MonadIO m
       , MonadMockable m
       , MonadBaseControl IO m
       , Eq (Promise m (Maybe ())) -- are you cereal boyz??1?
       )

-- | A set of constraints necessary for blockchain generation. All
-- these constraints must be satistified by the generator's caller.
type MonadBlockGen ctx m
     = ( MonadBlockGenBase m
       , MonadReader ctx m
       -- TODO: we don't really need to require pure db from the
       -- outside. We can clone real DB into a pure one. Or we can use
       -- 'DBProxyT' approach. Currently we require pure DB because
       -- it's easier.
       , HasLens DBPureVar ctx DBPureVar
       , HasSlottingVar ctx
       , HasLens LrcContext ctx LrcContext
       , HasSlogContext ctx
       )

----------------------------------------------------------------------------
-- Context
----------------------------------------------------------------------------

-- | Context used by blockchain generator.
data BlockGenContext = BlockGenContext
    { bgcPrimaryKey        :: SecretKey
    -- ^ This field is lazy on purpose. Primary key used for block
    -- generation changes frequently. We don't define it initially and
    -- modify it using 'local' when we need it. Alternative solution
    -- would be to define a type w/o primary key and another one with
    -- primary key, but it would lead to enormous amount of
    -- boilerplate. Also it could be put into mutable reference, but
    -- it's complicated too.
    , bgcDB                :: !DBPureVar
    -- ^ Pure DB used by block generation. Currently we always use
    -- pure DB and assume it always fits in memory. It allows us to
    -- simply clone existing DB.
    , bgcSlogContext       :: !SlogContext
    , bgcSystemStart       :: !Timestamp
    , bgcSlottingVar       :: !(TVar SlottingData)
    , bgcParams            :: !BlockGenParams
    , bgcDelegation        :: !DelegationVar
    , bgcLrcContext        :: !LrcContext
    , bgcTxpMem            :: !(GenericTxpLocalData TxpExtra_TMP, TxpMetrics)
    , bgcUpdateContext     :: !UpdateContext
    , bgcSscState          :: !(SscState SscGodTossing)
    , bgcSlotId            :: !(Maybe SlotId)
    -- ^ During block generation we don't want to use real time, but
    -- rather want to set current slot (fake one) by ourselves.
    , bgcTxpGlobalSettings :: !TxpGlobalSettings
    , bgcReportingContext  :: !ReportingContext
    , bgcDiscoveryContext  :: !DiscoveryContextSum
    }

makeLensesWith postfixLFields ''BlockGenContext

-- | Execution mode for blockchain generation.
type BlockGenMode m = ReaderT BlockGenContext m

----------------------------------------------------------------------------
-- Context creation
----------------------------------------------------------------------------

-- | Make new 'BlockGenContext' using data provided by 'MonadBlockGen'
-- context. Persistent data (DB) is cloned. Other mutable data is
-- recreated.
mkBlockGenContext :: MonadBlockGen ctx m => BlockGenParams -> m BlockGenContext
mkBlockGenContext bgcParams = do
    let bgcPrimaryKey = error "bgcPrimaryKey was forced before being set"
    bgcDB <- DB.cloneDBPure =<< view (lensOf @DBPureVar)
    bgcSystemStart <- view slottingTimestamp
    bgcSlottingVar <- cloneSlottingVar =<< view slottingVar
    bgcLrcContext <- cloneLrcContext =<< view (lensOf @LrcContext)
    bgcSlogContext <- cloneSlogContext =<< view slogContextL
    (initSlot, putInitSlot) <- newInitFuture
    let bgcSlotId = Nothing
    let bgcTxpGlobalSettings = txpGlobalSettings
    let bgcReportingContext = emptyReportingContext
    let bgcDiscoveryContext = DCStatic mempty
    let initCtx =
            InitBlockGenContext
                bgcDB
                bgcSystemStart
                bgcSlottingVar
                bgcLrcContext
                initSlot
    usingReaderT initCtx $ do
        tipEOS <- getEpochOrSlot <$> getTipHeader @SscGodTossing
        putInitSlot (epochOrSlotToSlot tipEOS)
        bgcSscState <- mkSscState @SscGodTossing
        bgcUpdateContext <- mkUpdateContext
        bgcTxpMem <- (,) <$> mkTxpLocalData <*> pure ignoreTxpMetrics
        bgcDelegation <- mkDelegationVar @SscGodTossing
        return BlockGenContext {..}

data InitBlockGenContext = InitBlockGenContext
    { ibgcDB          :: !DBPureVar
    , ibgcSystemStart :: !Timestamp
    , ibgcSlottingVar :: !(TVar SlottingData)
    , ibgcLrcContext  :: !LrcContext
    , ibgcSlot        :: SlotId
    -- ^ All initialization will be done assuming the current slot is
    -- this one. It's lazy to be used with future.
    }

makeLensesWith postfixLFields ''InitBlockGenContext

type InitBlockGenMode m = ReaderT InitBlockGenContext m

instance HasLens DBPureVar InitBlockGenContext DBPureVar where
    lensOf = ibgcDB_L

instance HasLens LrcContext InitBlockGenContext LrcContext where
    lensOf = ibgcLrcContext_L

instance HasSlottingVar InitBlockGenContext where
    slottingTimestamp = ibgcSystemStart_L
    slottingVar = ibgcSlottingVar_L

instance MonadBlockGenBase m => MonadDBRead (InitBlockGenMode m) where
    dbGet = DB.dbGetPureDefault
    dbIterSource = DB.dbIterSourcePureDefault

instance MonadBlockGenBase m => MonadDB (InitBlockGenMode m) where
    dbPut = DB.dbPutPureDefault
    dbWriteBatch = DB.dbWriteBatchPureDefault
    dbDelete = DB.dbDeletePureDefault

instance MonadBlockGenBase m =>
    MonadBlockDBGeneric (BlockHeader SscGodTossing) (Block SscGodTossing) Undo (InitBlockGenMode m)
  where
    dbGetBlock = BDB.dbGetBlockPureDefault @SscGodTossing
    dbGetUndo = BDB.dbGetUndoPureDefault @SscGodTossing
    dbGetHeader = BDB.dbGetHeaderPureDefault @SscGodTossing

instance MonadBlockGenBase m => MonadSlotsData (InitBlockGenMode m) where
    getSystemStart = getSystemStartDefault
    getSlottingData = getSlottingDataDefault
    waitPenultEpochEquals = waitPenultEpochEqualsDefault
    putSlottingData = putSlottingDataDefault

instance MonadBlockGenBase m => MonadSlots (InitBlockGenMode m) where
    getCurrentSlot = Just <$> view ibgcSlot_L
    getCurrentSlotBlocking = view ibgcSlot_L
    getCurrentSlotInaccurate = view ibgcSlot_L
    currentTimeSlotting = do
        logWarning "currentTimeSlotting is used in initialization"
        currentTimeSlottingSimple

----------------------------------------------------------------------------
-- Boilerplate instances
----------------------------------------------------------------------------

instance HasLens BlockGenContext BlockGenContext BlockGenContext where
    lensOf = identity

instance HasBlockGenParams BlockGenContext where
    blockGenParams = bgcParams_L

instance HasSlottingVar BlockGenContext where
    slottingTimestamp = bgcSystemStart_L
    slottingVar = bgcSlottingVar_L

instance HasLens DBPureVar BlockGenContext DBPureVar where
    lensOf = bgcDB_L

instance HasLens UpdateContext BlockGenContext UpdateContext where
    lensOf = bgcUpdateContext_L

instance HasLens DelegationVar BlockGenContext DelegationVar where
    lensOf = bgcDelegation_L

instance HasLens LrcContext BlockGenContext LrcContext where
    lensOf = bgcLrcContext_L

instance HasPrimaryKey BlockGenContext where
    primaryKey = bgcPrimaryKey_L

instance HasSlogContext BlockGenContext where
    slogContextL = bgcSlogContext_L

instance HasLens TxpHolderTag BlockGenContext (GenericTxpLocalData TxpExtra_TMP, TxpMetrics) where
    lensOf = bgcTxpMem_L

instance HasLens SscMemTag BlockGenContext (SscState SscGodTossing) where
    lensOf = bgcSscState_L

instance HasLens TxpGlobalSettings BlockGenContext TxpGlobalSettings where
    lensOf = bgcTxpGlobalSettings_L

instance HasReportingContext BlockGenContext where
    reportingContext = bgcReportingContext_L

instance HasDiscoveryContextSum BlockGenContext where
    discoveryContextSum = bgcDiscoveryContext_L

instance MonadBlockGenBase m => MonadDBRead (BlockGenMode m) where
    dbGet = DB.dbGetPureDefault
    dbIterSource = DB.dbIterSourcePureDefault

instance MonadBlockGenBase m => MonadDB (BlockGenMode m) where
    dbPut = DB.dbPutPureDefault
    dbWriteBatch = DB.dbWriteBatchPureDefault
    dbDelete = DB.dbDeletePureDefault

instance MonadBlockGenBase m =>
    MonadBlockDBGeneric (BlockHeader SscGodTossing) (Block SscGodTossing) Undo (BlockGenMode m)
  where
    dbGetBlock = BDB.dbGetBlockPureDefault @SscGodTossing
    dbGetUndo = BDB.dbGetUndoPureDefault @SscGodTossing
    dbGetHeader = BDB.dbGetHeaderPureDefault @SscGodTossing

instance MonadBlockGenBase m =>
    MonadBlockDBGeneric (Some IsHeader) (SscBlock SscGodTossing) () (BlockGenMode m)
  where
    dbGetBlock = BDB.dbGetBlockSscPureDefault @SscGodTossing
    dbGetUndo = BDB.dbGetUndoSscPureDefault @SscGodTossing
    dbGetHeader = BDB.dbGetHeaderSscPureDefault @SscGodTossing

instance MonadBlockGenBase m =>
         MonadBlockDBGenericWrite (BlockHeader SscGodTossing) (Block SscGodTossing) Undo (BlockGenMode m) where
    dbPutBlund = BDB.dbPutBlundPureDefault

instance MonadBlockGenBase m => MonadSlotsData (BlockGenMode m) where
    getSystemStart = getSystemStartDefault
    getSlottingData = getSlottingDataDefault
    waitPenultEpochEquals = waitPenultEpochEqualsDefault
    putSlottingData = putSlottingDataDefault

instance MonadBlockGenBase m => MonadSlots (BlockGenMode m) where
    getCurrentSlot = view bgcSlotId_L
    getCurrentSlotBlocking =
        view bgcSlotId_L >>= \case
            Nothing ->
                reportFatalError
                    "getCurrentSlotBlocking is used in generator when slot is unknown"
            Just slot -> pure slot
    getCurrentSlotInaccurate =
        reportFatalError
            "It hardly makes sense to use 'getCurrentSlotInaccurate' during block generation"
    currentTimeSlotting = currentTimeSlottingSimple

instance MonadBlockGenBase m => DB.MonadGState (BlockGenMode m) where
    gsAdoptedBVData = gsAdoptedBVDataDefault

instance MonadBlockGenBase m => MonadBListener (BlockGenMode m) where
    onApplyBlocks = onApplyBlocksStub
    onRollbackBlocks = onRollbackBlocksStub

instance MonadBlockGenBase m => MonadDiscovery (BlockGenMode m) where
    getPeers = getPeersSum
    findPeers = findPeersSum

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

usingPrimaryKey :: MonadReader BlockGenContext m => SecretKey -> m a -> m a
usingPrimaryKey sk = local (set primaryKey sk)

withCurrentSlot :: MonadReader BlockGenContext m => SlotId -> m a -> m a
withCurrentSlot slot = local (set bgcSlotId_L $ Just slot)
