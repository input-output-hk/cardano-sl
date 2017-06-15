{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Pos.WorkMode
       ( WorkMode
       , MinWorkMode

       , TxpExtra_TMP

       -- * Actual modes
       , RealMode(..)
       ) where

import           Universum

import           Control.Monad.Base             (MonadBase)
import           Control.Monad.Fix              (MonadFix)
import           Control.Monad.Trans.Control    (MonadBaseControl (..))
import qualified Control.Monad.Trans.Lift.Local as Lift
import           Data.Coerce
import           Data.Tagged                    (Tagged)
import qualified Ether
import           Mockable                       (ChannelT, Counter, Distribution, Gauge,
                                                 MFunctor' (..), Mockable (..), Promise,
                                                 SharedAtomicT, SharedExclusiveT,
                                                 ThreadId)
import           Mockable.Production            (Production)
import           System.Wlog                    (CanLog, HasLoggerName,
                                                 LoggerNameBox (..))

import           Pos.Block.BListener            (MonadBListener(..), onApplyBlocksStub, onRollbackBlocksStub)
import           Pos.Block.Core                 (Block, BlockHeader)
import           Pos.Block.Types                (Undo)
import           Pos.Communication.PeerState    (PeerStateCtx, getPeerStateReal, clearPeerStateReal, getAllStatesReal,
                                                 PeerStateTag, WithPeerState(..))
import           Pos.Context                    (NodeContext)
import           Pos.DB                         (MonadGState(..), NodeDBs)
import           Pos.DB.Redirect                (dbGetReal, dbPutReal, dbWriteBatchReal, dbDeleteReal)
import           Pos.DB.Block                   (MonadBlockDBWrite(..),
                                                 dbGetBlockReal, dbGetUndoReal, dbGetHeaderReal, dbGetBlockReal',
                                                 dbGetUndoReal', dbGetHeaderReal', dbPutBlundReal)
import           Pos.DB.Class                   (MonadBlockDBGeneric (..), MonadDB(..),
                                                 MonadDBRead(..))
import           Pos.Core                       (IsHeader)
import           Pos.DB.DB                      (gsAdoptedBVDataDB)
import           Pos.Delegation.Class           (DelegationVar)
import           Pos.Discovery                  (MonadDiscovery(..), getPeersReal, findPeersReal)
import           Pos.Slotting.Class             (MonadSlots(..))
import           Pos.Slotting.Impl.Sum          (getCurrentSlotReal,
                                                 getCurrentSlotBlockingReal, getCurrentSlotInaccurateReal,
                                                 currentTimeSlottingReal)
import           Pos.Slotting.MemState          (MonadSlotsData(..),
                                                 getSystemStartReal, getSlottingDataReal, waitPenultEpochEqualsReal,
                                                 putSlottingDataReal)
import           Pos.Ssc.Class.Types            (SscBlock)
import           Pos.Ssc.Class.Helpers          (SscHelpersClass)
import           Pos.Ssc.Extra                  (SscMemTag, SscState)
import           Pos.Txp.MemState               (GenericTxpLocalData, TxpHolderTag)
import           Pos.Util.TimeWarp              (CanJsonLog, JsonLogT)
import           Pos.Util.Util                  (PowerLift (..))
import           Pos.WorkMode.Class             (MinWorkMode, TxpExtra_TMP, WorkMode)
import           Pos.Util                       (Some (..))

----------------------------------------------------------------------------
-- Concrete types
----------------------------------------------------------------------------

-- | RealMode is a basis for `WorkMode`s used to really run system.
type RealMode' ssc =
    Ether.ReadersT
        ( Tagged NodeDBs NodeDBs
        , Tagged SscMemTag (SscState ssc)
        , Tagged TxpHolderTag (GenericTxpLocalData TxpExtra_TMP)
        , Tagged DelegationVar DelegationVar
        , Tagged PeerStateTag (PeerStateCtx Production)
        ) (
    Ether.ReadersT (NodeContext ssc) (
    JsonLogT (
    LoggerNameBox (
    Production
    ))))

newtype RealMode ssc a = RealMode { unRealMode :: RealMode' ssc a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadBase IO
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadFix
    )

instance MonadBaseControl IO (RealMode ssc) where
    type StM (RealMode ssc) a = StM (RealMode' ssc) a
    liftBaseWith f = RealMode $ liftBaseWith $ \q -> f (q . unRealMode)
    restoreM s = RealMode $ restoreM s

type instance ThreadId (RealMode ssc) = ThreadId Production
type instance Promise (RealMode ssc) = Promise Production
type instance SharedAtomicT (RealMode ssc) = SharedAtomicT Production
type instance SharedExclusiveT (RealMode ssc) = SharedExclusiveT Production
type instance Gauge (RealMode ssc) = Gauge Production
type instance ChannelT (RealMode ssc) = ChannelT Production
type instance Distribution (RealMode ssc) = Distribution Production
type instance Counter (RealMode ssc) = Counter Production

deriving instance CanLog (RealMode ssc)
deriving instance HasLoggerName (RealMode ssc)
deriving instance CanJsonLog (RealMode ssc)

instance MonadSlotsData (RealMode ssc) where
    getSystemStart = getSystemStartReal
    getSlottingData = getSlottingDataReal
    waitPenultEpochEquals = waitPenultEpochEqualsReal
    putSlottingData = putSlottingDataReal

instance MonadSlots (RealMode ssc) where
    getCurrentSlot = getCurrentSlotReal
    getCurrentSlotBlocking = getCurrentSlotBlockingReal
    getCurrentSlotInaccurate = getCurrentSlotInaccurateReal
    currentTimeSlotting = currentTimeSlottingReal

instance MonadDiscovery (RealMode ssc) where
    getPeers = getPeersReal
    findPeers = findPeersReal

instance MonadGState (RealMode ssc) where
    gsAdoptedBVData = gsAdoptedBVDataDB

instance MonadDBRead (RealMode ssc) where
    dbGet = dbGetReal

instance MonadDB (RealMode ssc) where
    dbPut = dbPutReal
    dbWriteBatch = dbWriteBatchReal
    dbDelete = dbDeleteReal

instance SscHelpersClass ssc => MonadBlockDBWrite ssc (RealMode ssc) where
    dbPutBlund = dbPutBlundReal

instance MonadBListener (RealMode ssc) where
    onApplyBlocks = onApplyBlocksStub
    onRollbackBlocks = onRollbackBlocksStub

instance WithPeerState (RealMode ssc) where
    getPeerState = getPeerStateReal
    clearPeerState = clearPeerStateReal
    getAllStates = getAllStatesReal

instance PowerLift m (RealMode' ssc) => PowerLift m (RealMode ssc) where
    powerLift = RealMode . powerLift

instance
    SscHelpersClass ssc =>
    MonadBlockDBGeneric (BlockHeader ssc) (Block ssc) Undo (RealMode ssc)
  where
    dbGetBlock  = dbGetBlockReal @ssc
    dbGetUndo   = dbGetUndoReal @ssc
    dbGetHeader = dbGetHeaderReal @ssc

instance
    SscHelpersClass ssc =>
    MonadBlockDBGeneric (Some IsHeader) (SscBlock ssc) () (RealMode ssc)
  where
    dbGetBlock  = dbGetBlockReal' @ssc
    dbGetUndo   = dbGetUndoReal' @ssc
    dbGetHeader = dbGetHeaderReal' @ssc

instance
    ( Mockable d (RealMode' ssc)
    , MFunctor' d (RealMode ssc) (RealMode' ssc)
    )
    => Mockable d (RealMode ssc) where
    liftMockable dmt = RealMode $ liftMockable $ hoist' (\(RealMode m) -> m) dmt

instance
    Ether.MonadReader tag r (RealMode' ssc) =>
    Ether.MonadReader tag r (RealMode ssc)
  where
    ask =
        (coerce :: RealMode' ssc r -> RealMode ssc r)
        (Ether.ask @tag)
    local =
        (coerce :: forall a .
            Lift.Local r (RealMode' ssc) a ->
            Lift.Local r (RealMode ssc) a)
        (Ether.local @tag)
    reader =
        (coerce :: forall a .
            ((r -> a) -> RealMode' ssc a) ->
            ((r -> a) -> RealMode ssc a))
        (Ether.reader @tag)
