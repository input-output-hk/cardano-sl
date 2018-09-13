{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

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

import           Control.Lens (lens)
import           Control.Lens.TH (makeLensesWith)
import           Control.Monad.Random.Strict (RandT)
import qualified Crypto.Random as Rand
import           Data.Default (Default)
import           UnliftIO (MonadUnliftIO)

import           Pos.Chain.Block (HasSlogGState (..))
import           Pos.Chain.Delegation (DelegationVar, HasDlgConfiguration)
import           Pos.Chain.Ssc (HasSscConfiguration, SscMemTag, SscState)
import           Pos.Chain.Update (HasUpdateConfiguration)
import           Pos.Client.Txp.Addresses (MonadAddresses (..))
import           Pos.Configuration (HasNodeConfiguration)
import           Pos.Core (Address, HasPrimaryKey (..), SlotCount, SlotId (..),
                     Timestamp, epochOrSlotToSlot, getEpochOrSlot,
                     largestPubKeyAddressBoot)
import           Pos.Core.Exception (reportFatalError)
import           Pos.Core.Genesis (GenesisWStakeholders (..))
import           Pos.Core.Reporting (HasMisbehaviorMetrics (..),
                     MonadReporting (..))
import           Pos.Crypto (SecretKey)
import           Pos.DB (DBSum, MonadDB, MonadDBRead)
import qualified Pos.DB as DB
import           Pos.DB.Block (MonadBListener (..))
import qualified Pos.DB.Block as DB
import           Pos.DB.DB (gsAdoptedBVDataDefault)
import           Pos.DB.Delegation (mkDelegationVar)
import           Pos.DB.Lrc (HasLrcContext, LrcContext (..))
import           Pos.DB.Ssc (mkSscState)
import           Pos.DB.Txp (GenericTxpLocalData, MempoolExt, TxpGlobalSettings,
                     TxpHolderTag, mkTxpLocalData)
import           Pos.DB.Update (UpdateContext, mkUpdateContext)
import           Pos.Generator.Block.Param (BlockGenParams (..),
                     HasBlockGenParams (..), HasTxGenParams (..))
import qualified Pos.GState as GS
import           Pos.Infra.Network.Types (HasNodeType (..), NodeType (..))
import           Pos.Infra.Slotting (HasSlottingVar (..), MonadSlots (..),
                     MonadSlotsData, currentTimeSlottingSimple)
import           Pos.Infra.Slotting.Types (SlottingData)
import           Pos.Util (HasLens (..), newInitFuture, postfixLFields)
import           Pos.Util.Wlog (WithLogger, logWarning)


----------------------------------------------------------------------------
-- Constraint
----------------------------------------------------------------------------

-- | A set of constraints imposed on the base monad used for
-- arbitrary blockchain generation.
type MonadBlockGenBase m
     = ( WithLogger m
       , MonadMask m
       , MonadIO m
       , MonadUnliftIO m
       , HasUpdateConfiguration
       , HasSscConfiguration
       , HasNodeConfiguration
       , HasDlgConfiguration
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
       , MonadBListener m
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
    , bgcSscState          :: !SscState
    , bgcSlotId            :: !(Maybe SlotId)
    -- ^ During block generation we don't want to use real time, but
    -- rather want to set current slot (fake one) by ourselves.
    , bgcTxpGlobalSettings :: !TxpGlobalSettings
    }

makeLensesWith postfixLFields ''BlockGenContext

-- | Execution mode for blockchain generation.
type BlockGenMode ext m = ReaderT (BlockGenContext ext) m

-- | Block generation mode with random
type BlockGenRandMode ext g m = RandT g (BlockGenMode ext m)

----------------------------------------------------------------------------
-- Context creation
----------------------------------------------------------------------------

-- | Make new 'BlockGenContext' using data provided by 'MonadBlockGen'
-- context. Persistent data (DB) is cloned. Other mutable data is
-- recreated.
mkBlockGenContext
    :: forall ext ctx m
     . (MonadBlockGenInit ctx m, Default ext)
    => SlotCount
    -> BlockGenParams
    -> m (BlockGenContext ext)
mkBlockGenContext epochSlots bgcParams@BlockGenParams{..} = do
    let bgcPrimaryKey = error "bgcPrimaryKey was forced before being set"
    bgcGState <- if _bgpInplaceDB
                 then view GS.gStateContext
                 else GS.cloneGStateContext =<< view GS.gStateContext
    bgcSystemStart <- view slottingTimestamp
    (initSlot, putInitSlot) <- newInitFuture "initSlot"
    let bgcSlotId = Nothing
    let bgcTxpGlobalSettings = _bgpTxpGlobalSettings
    let bgcGenStakeholders = _bgpGenStakeholders
    let initCtx =
            InitBlockGenContext
                (bgcGState ^. GS.gscDB)
                bgcSystemStart
                (bgcGState ^. GS.gscSlottingVar)
                (bgcGState ^. GS.gscLrcContext)
                initSlot
    usingReaderT initCtx $ do
        tipEOS <- getEpochOrSlot <$> DB.getTipHeader
        putInitSlot (epochOrSlotToSlot tipEOS)
        bgcSscState <- mkSscState epochSlots
        bgcUpdateContext <- mkUpdateContext epochSlots
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
    dbGetSerBlock = DB.dbGetSerBlockSumDefault
    dbGetSerUndo = DB.dbGetSerUndoSumDefault
    dbGetSerBlund = DB.dbGetSerBlundSumDefault

instance MonadBlockGenBase m => MonadDB (InitBlockGenMode ext m) where
    dbPut = DB.dbPutSumDefault
    dbWriteBatch = DB.dbWriteBatchSumDefault
    dbDelete = DB.dbDeleteSumDefault
    dbPutSerBlunds = DB.dbPutSerBlundsSumDefault

instance (MonadBlockGenBase m, MonadSlotsData ctx (InitBlockGenMode ext m))
      => MonadSlots ctx (InitBlockGenMode ext m)
  where
    getCurrentSlot _           = Just <$> view ibgcSlot_L
    getCurrentSlotBlocking _   = view ibgcSlot_L
    getCurrentSlotInaccurate _ = view ibgcSlot_L
    currentTimeSlotting        = do
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

instance HasLens SscMemTag (BlockGenContext ext) SscState where
    lensOf = bgcSscState_L

instance HasLens TxpGlobalSettings (BlockGenContext ext) TxpGlobalSettings where
    lensOf = bgcTxpGlobalSettings_L

-- Let's assume that block-gen is core node, though it shouldn't
-- really matter (needed for reporting, which is not used in block-gen
-- anyway).
instance HasNodeType (BlockGenContext ext) where
    getNodeType _ = NodeCore

-- | Ignore reports.
-- FIXME it's a bad sign that we even need this instance.
-- The pieces of the software which the block generator uses should never
-- even try to report.
instance Applicative m => MonadReporting (BlockGenMode ext m) where
    report _ = pure ()

-- | Ignore reports.
-- FIXME it's a bad sign that we even need this instance.
instance HasMisbehaviorMetrics (BlockGenContext ext) where
    misbehaviorMetrics = lens (const Nothing) const

instance MonadBlockGenBase m => MonadDBRead (BlockGenMode ext m) where
    dbGet = DB.dbGetSumDefault
    dbIterSource = DB.dbIterSourceSumDefault
    dbGetSerBlock = DB.dbGetSerBlockSumDefault
    dbGetSerUndo = DB.dbGetSerUndoSumDefault
    dbGetSerBlund = DB.dbGetSerBlundSumDefault

instance MonadBlockGenBase m => MonadDB (BlockGenMode ext m) where
    dbPut = DB.dbPutSumDefault
    dbWriteBatch = DB.dbWriteBatchSumDefault
    dbDelete = DB.dbDeleteSumDefault
    dbPutSerBlunds = DB.dbPutSerBlundsSumDefault

instance (MonadBlockGenBase m, MonadSlotsData ctx (BlockGenMode ext m))
      => MonadSlots ctx (BlockGenMode ext m)
  where
    getCurrentSlot _ = view bgcSlotId_L
    getCurrentSlotBlocking _ =
        view bgcSlotId_L >>= \case
            Nothing ->
                reportFatalError
                    "getCurrentSlotBlocking is used in generator when slot is unknown"
            Just slot -> pure slot
    getCurrentSlotInaccurate _ =
        reportFatalError
            "It hardly makes sense to use 'getCurrentSlotInaccurate' during block generation"
    currentTimeSlotting = currentTimeSlottingSimple

instance MonadBlockGenBase m => DB.MonadGState (BlockGenMode ext m) where
    gsAdoptedBVData = gsAdoptedBVDataDefault

instance MonadBListener m => MonadBListener (BlockGenMode ext m) where
    onApplyBlocks = lift . onApplyBlocks
    onRollbackBlocks pc = lift . onRollbackBlocks pc


instance Monad m => MonadAddresses (BlockGenMode ext m) where
    type AddrData (BlockGenMode ext m) = Address
    getNewAddress _ = pure
    -- It must be consistent with the way we construct address in
    -- block-gen. If it's changed, tests will fail, so we will notice
    -- it.
    -- N.B. Currently block-gen uses only PubKey addresses with BootstrapEra
    -- distribution.
    getFakeChangeAddress _ = pure largestPubKeyAddressBoot

type instance MempoolExt (BlockGenMode ext m) = ext

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

usingPrimaryKey :: MonadReader (BlockGenContext ext) m => SecretKey -> m a -> m a
usingPrimaryKey sk = local (set primaryKey sk)

withCurrentSlot :: MonadReader (BlockGenContext ext) m => SlotId -> m a -> m a
withCurrentSlot slot = local (set bgcSlotId_L $ Just slot)
