{-# LANGUAGE CPP          #-}
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

-- | Execution mode used by blockchain generator.

module Pos.Generator.Block.Mode
       ( MonadBlockGenBase
       , MonadBlockGen
       , MonadBlockGenInit
       , BlockGenContext (..)
       , BlockGenMode
       , BlockGenRandMode
       , mkBlockGenContext

       , usingPrimaryKey
       , withCurrentSlot

       , InitBlockGenContext (..) -- useless
       ) where

import           Universum

import           Control.Lens.TH                  (makeLensesWith)
import qualified Control.Monad.Catch
import           Control.Monad.Random.Strict      (RandT)
import           Control.Monad.Trans.Control      (MonadBaseControl)
import qualified Crypto.Random                    as Rand
import           Data.Default                     (Default)
import           Mockable                         (Async, Catch, Concurrently,
                                                   CurrentTime, Delay, Mockables, Promise,
                                                   Throw)
import           System.Wlog                      (WithLogger, logWarning)

import           Pos.Block.BListener              (MonadBListener (..), onApplyBlocksStub,
                                                   onRollbackBlocksStub)
import           Pos.Block.Core                   (Block, BlockHeader)
import           Pos.Block.Slog                   (HasSlogGState (..))
import           Pos.Block.Types                  (Undo)
import           Pos.Client.Txp.Addresses         (MonadAddresses (..))
import           Pos.Configuration                (HasNodeConfiguration)
import           Pos.Core                         (Address, GenesisWStakeholders (..),
                                                   HasConfiguration, HasPrimaryKey (..),
                                                   IsHeader, SlotId (..), Timestamp,
                                                   epochOrSlotToSlot, getEpochOrSlot)
import           Pos.Crypto                       (SecretKey)
import           Pos.DB                           (DBSum, MonadBlockDBGeneric (..),
                                                   MonadBlockDBGenericWrite (..), MonadDB,
                                                   MonadDBRead)
import qualified Pos.DB                           as DB
import qualified Pos.DB.Block                     as BDB
import           Pos.DB.DB                        (getTipHeader, gsAdoptedBVDataDefault)
import           Pos.Delegation                   (DelegationVar, mkDelegationVar)
import           Pos.Exception                    (reportFatalError)
import           Pos.Generator.Block.Param        (BlockGenParams (..),
                                                   HasBlockGenParams (..),
                                                   HasTxGenParams (..))
import qualified Pos.GState                       as GS
import           Pos.Infra.Configuration          (HasInfraConfiguration)
import           Pos.KnownPeers                   (MonadFormatPeers)
import           Pos.Lrc                          (HasLrcContext, LrcContext (..))
import           Pos.Network.Types                (HasNodeType (..), NodeType (..))
import           Pos.Reporting                    (HasReportingContext (..),
                                                   ReportingContext,
                                                   emptyReportingContext)
import           Pos.Slotting                     (HasSlottingVar (..), MonadSlots (..),
                                                   MonadSlotsData, SlottingData,
                                                   currentTimeSlottingSimple)
import           Pos.Ssc.Class                    (SscBlock)
import           Pos.Ssc.Extra                    (SscMemTag, SscState, mkSscState)
import           Pos.Ssc.GodTossing               (SscGodTossing)
import           Pos.Ssc.GodTossing.Configuration (HasGtConfiguration)
import           Pos.Txp                          (GenericTxpLocalData, MempoolExt,
                                                   TxpGlobalSettings, TxpHolderTag,
                                                   mkTxpLocalData)
import           Pos.Update.Configuration         (HasUpdateConfiguration)
import           Pos.Update.Context               (UpdateContext, mkUpdateContext)
import           Pos.Util                         (HasLens (..), Some, newInitFuture,
                                                   postfixLFields)
#ifdef WITH_EXPLORER
import           Pos.Explorer                     (explorerTxpGlobalSettings)
#else
import           Pos.Txp                          (txpGlobalSettings)
#endif

-- Remove this once there's no #ifdef-ed Pos.Txp import
{-# ANN module ("HLint: ignore Use fewer imports" :: Text) #-}


----------------------------------------------------------------------------
-- Constraint
----------------------------------------------------------------------------

-- | A set of constraints imposed on the base monad used for
-- arbitrary blockchain generation.
type MonadBlockGenBase m
     = ( WithLogger m
       , MonadMask m
       , MonadIO m
       , MonadBaseControl IO m
       , MonadFormatPeers m
       , Mockables m
           [ CurrentTime
           , Async
           , Catch
           , Throw
           , Delay
           , Concurrently
           ]
       , Eq (Promise m (Maybe ())) -- are you cereal boyz??1?
       , HasConfiguration
       , HasUpdateConfiguration
       , HasInfraConfiguration
       , HasGtConfiguration
       , HasNodeConfiguration
       )

-- | A set of constraints necessary for blockchain generation. All
-- these constraints must be satistified by the generator's caller.
type MonadBlockGen ctx m
     = ( MonadBlockGenBase m
       , MonadReader ctx m
       , Rand.MonadRandom m

       , HasSlottingVar ctx
       , HasSlogGState ctx
       , HasLrcContext ctx
       )

-- | MonadBlockGen extended with the specific GStateContext.
type MonadBlockGenInit ctx m
     = ( MonadBlockGen ctx m
       , GS.HasGStateContext ctx
       )

----------------------------------------------------------------------------
-- Context
----------------------------------------------------------------------------

-- | Context used by blockchain generator.
data BlockGenContext ext = BlockGenContext
    { bgcPrimaryKey        :: SecretKey
    -- ^ This field is lazy on purpose. Primary key used for block
    -- generation changes frequently. We don't define it initially and
    -- modify it using 'local' when we need it. Alternative solution
    -- would be to define a type w/o primary key and another one with
    -- primary key, but it would lead to enormous amount of
    -- boilerplate. Also it could be put into mutable reference, but
    -- it's complicated too.
    , bgcGState            :: !GS.GStateContext
    -- ^ Currently we always use pure DB and assume it always fits in
    -- memory. It allows us to simply clone existing DB.
    , bgcSystemStart       :: !Timestamp
    , bgcParams            :: !BlockGenParams
    , bgcDelegation        :: !DelegationVar
    , bgcGenStakeholders   :: !GenesisWStakeholders
    , bgcTxpMem            :: !(GenericTxpLocalData ext)
    , bgcUpdateContext     :: !UpdateContext
    , bgcSscState          :: !(SscState SscGodTossing)
    , bgcSlotId            :: !(Maybe SlotId)
    -- ^ During block generation we don't want to use real time, but
    -- rather want to set current slot (fake one) by ourselves.
    , bgcTxpGlobalSettings :: !TxpGlobalSettings
    , bgcReportingContext  :: !ReportingContext
    }

makeLensesWith postfixLFields ''BlockGenContext

-- | Execution mode for blockchain generation.
type BlockGenMode ext m = ReaderT (BlockGenContext ext) m

-- | Block generation mode with random
type BlockGenRandMode ext g m = RandT g (BlockGenMode ext m)

instance MonadThrow m => MonadThrow (RandT g m) where
    throwM = lift . throwM

----------------------------------------------------------------------------
-- Context creation
----------------------------------------------------------------------------

-- | Make new 'BlockGenContext' using data provided by 'MonadBlockGen'
-- context. Persistent data (DB) is cloned. Other mutable data is
-- recreated.
mkBlockGenContext
    :: forall ext ctx m.
       ( MonadBlockGenInit ctx m
       , HasGtConfiguration
       , HasNodeConfiguration
       , Default ext
       )
    => BlockGenParams
    -> m (BlockGenContext ext)
mkBlockGenContext bgcParams@BlockGenParams{..} = do
    let bgcPrimaryKey = error "bgcPrimaryKey was forced before being set"
    bgcGState <- if _bgpInplaceDB
                 then view GS.gStateContext
                 else GS.cloneGStateContext =<< view GS.gStateContext
    bgcSystemStart <- view slottingTimestamp
    (initSlot, putInitSlot) <- newInitFuture "initSlot"
    let bgcSlotId = Nothing
#ifdef WITH_EXPLORER
    let bgcTxpGlobalSettings = explorerTxpGlobalSettings
#else
    let bgcTxpGlobalSettings = txpGlobalSettings
#endif
    let bgcReportingContext = emptyReportingContext
    let bgcGenStakeholders = _bgpGenStakeholders
    let initCtx =
            InitBlockGenContext
                (bgcGState ^. GS.gscDB)
                bgcSystemStart
                (bgcGState ^. GS.gscSlottingVar)
                (bgcGState ^. GS.gscLrcContext)
                initSlot
    usingReaderT initCtx $ do
        tipEOS <- getEpochOrSlot <$> getTipHeader
        putInitSlot (epochOrSlotToSlot tipEOS)
        bgcSscState <- mkSscState @SscGodTossing
        bgcUpdateContext <- mkUpdateContext
        bgcTxpMem <- mkTxpLocalData
        bgcDelegation <- mkDelegationVar
        return BlockGenContext {..}

data InitBlockGenContext = InitBlockGenContext
    { ibgcDB          :: !DBSum
    , ibgcSystemStart :: !Timestamp
    , ibgcSlottingVar :: !(TVar SlottingData)
    , ibgcLrcContext  :: !LrcContext
    , ibgcSlot        :: SlotId
    -- ^ All initialization will be done assuming the current slot is
    -- this one. It's lazy to be used with future.
    }

makeLensesWith postfixLFields ''InitBlockGenContext

type InitBlockGenMode ext m = ReaderT InitBlockGenContext m

instance HasLens DBSum InitBlockGenContext DBSum where
    lensOf = ibgcDB_L

instance HasLens LrcContext InitBlockGenContext LrcContext where
    lensOf = ibgcLrcContext_L

instance HasSlottingVar InitBlockGenContext where
    slottingTimestamp = ibgcSystemStart_L
    slottingVar = ibgcSlottingVar_L

instance MonadBlockGenBase m => MonadDBRead (InitBlockGenMode ext m) where
    dbGet = DB.dbGetSumDefault
    dbIterSource = DB.dbIterSourceSumDefault

instance MonadBlockGenBase m => MonadDB (InitBlockGenMode ext m) where
    dbPut = DB.dbPutSumDefault
    dbWriteBatch = DB.dbWriteBatchSumDefault
    dbDelete = DB.dbDeleteSumDefault

instance (HasGtConfiguration, MonadBlockGenBase m) =>
    MonadBlockDBGeneric BlockHeader Block Undo (InitBlockGenMode ext m)
  where
    dbGetBlock = BDB.dbGetBlockSumDefault
    dbGetUndo = BDB.dbGetUndoSumDefault
    dbGetHeader = BDB.dbGetHeaderSumDefault

instance (MonadBlockGenBase m, MonadSlotsData ctx (InitBlockGenMode ext m))
      => MonadSlots ctx (InitBlockGenMode ext m)
  where
    getCurrentSlot           = Just <$> view ibgcSlot_L
    getCurrentSlotBlocking   = view ibgcSlot_L
    getCurrentSlotInaccurate = view ibgcSlot_L
    currentTimeSlotting      = do
        logWarning "currentTimeSlotting is used in initialization"
        currentTimeSlottingSimple

----------------------------------------------------------------------------
-- Boilerplate instances
----------------------------------------------------------------------------

instance GS.HasGStateContext (BlockGenContext ext) where
    gStateContext = bgcGState_L

instance HasLens (BlockGenContext ext) (BlockGenContext ext) (BlockGenContext ext) where
    lensOf = identity

instance HasBlockGenParams (BlockGenContext ext) where
    blockGenParams = bgcParams_L

instance HasTxGenParams (BlockGenContext ext) where
    txGenParams = bgcParams_L . txGenParams

instance HasSlottingVar (BlockGenContext ext) where
    slottingTimestamp = bgcSystemStart_L
    slottingVar = GS.gStateContext . GS.gscSlottingVar

instance HasLens GenesisWStakeholders (BlockGenContext ext) GenesisWStakeholders where
    lensOf = bgcGenStakeholders_L

instance HasLens DBSum (BlockGenContext ext) DBSum where
    lensOf = GS.gStateContext . GS.gscDB

instance HasLens UpdateContext (BlockGenContext ext) UpdateContext where
    lensOf = bgcUpdateContext_L

instance HasLens DelegationVar (BlockGenContext ext) DelegationVar where
    lensOf = bgcDelegation_L

instance HasLens LrcContext (BlockGenContext ext) LrcContext where
    lensOf = GS.gStateContext . GS.gscLrcContext

instance HasPrimaryKey (BlockGenContext ext) where
    primaryKey = bgcPrimaryKey_L

instance HasSlogGState (BlockGenContext ext) where
    slogGState = GS.gStateContext . GS.gscSlogGState

instance HasLens TxpHolderTag (BlockGenContext ext) (GenericTxpLocalData ext) where
    lensOf = bgcTxpMem_L

instance HasLens SscMemTag (BlockGenContext ext) (SscState SscGodTossing) where
    lensOf = bgcSscState_L

instance HasLens TxpGlobalSettings (BlockGenContext ext) TxpGlobalSettings where
    lensOf = bgcTxpGlobalSettings_L

instance HasReportingContext (BlockGenContext ext) where
    reportingContext = bgcReportingContext_L

-- Let's assume that block-gen is core node, though it shouldn't
-- really matter (needed for reporting, which is not used in block-gen
-- anyway).
instance HasNodeType (BlockGenContext ext) where
    getNodeType _ = NodeCore

instance MonadBlockGenBase m => MonadDBRead (BlockGenMode ext m) where
    dbGet = DB.dbGetSumDefault
    dbIterSource = DB.dbIterSourceSumDefault

instance MonadBlockGenBase m => MonadDB (BlockGenMode ext m) where
    dbPut = DB.dbPutSumDefault
    dbWriteBatch = DB.dbWriteBatchSumDefault
    dbDelete = DB.dbDeleteSumDefault

instance (HasGtConfiguration, MonadBlockGenBase m) =>
    MonadBlockDBGeneric BlockHeader Block Undo (BlockGenMode ext m)
  where
    dbGetBlock = BDB.dbGetBlockSumDefault
    dbGetUndo = BDB.dbGetUndoSumDefault
    dbGetHeader = BDB.dbGetHeaderSumDefault

instance (HasGtConfiguration, MonadBlockGenBase m) =>
    MonadBlockDBGeneric (Some IsHeader) (SscBlock SscGodTossing) () (BlockGenMode ext m)
  where
    dbGetBlock = BDB.dbGetBlockSscSumDefault
    dbGetUndo = BDB.dbGetUndoSscSumDefault
    dbGetHeader = BDB.dbGetHeaderSscSumDefault

instance (HasGtConfiguration, MonadBlockGenBase m) =>
         MonadBlockDBGenericWrite BlockHeader Block Undo (BlockGenMode ext m) where
    dbPutBlund = BDB.dbPutBlundSumDefault

instance (MonadBlockGenBase m, MonadSlotsData ctx (BlockGenMode ext m))
      => MonadSlots ctx (BlockGenMode ext m)
  where
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

instance MonadBlockGenBase m => DB.MonadGState (BlockGenMode ext m) where
    gsAdoptedBVData = gsAdoptedBVDataDefault

instance MonadBlockGenBase m => MonadBListener (BlockGenMode ext m) where
    onApplyBlocks = onApplyBlocksStub
    onRollbackBlocks = onRollbackBlocksStub

instance Monad m => MonadAddresses (BlockGenMode ext m) where
    type AddrData (BlockGenMode ext m) = Address
    getNewAddress = pure

type instance MempoolExt (BlockGenMode ext m) = ext

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

usingPrimaryKey :: MonadReader (BlockGenContext ext) m => SecretKey -> m a -> m a
usingPrimaryKey sk = local (set primaryKey sk)

withCurrentSlot :: MonadReader (BlockGenContext ext) m => SlotId -> m a -> m a
withCurrentSlot slot = local (set bgcSlotId_L $ Just slot)
