{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Pos.WorkMode
       ( WorkMode
       , MinWorkMode

       , TxpExtra_TMP

       -- * Actual modes
       , RealMode(..)
       , RealModeContext(..)
       ) where

import           Universum

import           Control.Lens                (makeLensesFor)
import           Control.Monad.Base          (MonadBase)
import           Control.Monad.Fix           (MonadFix)
import           Control.Monad.Trans.Control (MonadBaseControl (..))
import           Data.Coerce
import qualified Ether
import           Ether.Internal              (HasLens (..))
import           Mockable                    (ChannelT, Counter, Distribution, Gauge,
                                              MFunctor' (..), Mockable (..), Promise,
                                              SharedAtomicT, SharedExclusiveT, ThreadId)
import           Mockable.Production         (Production)
import           System.Wlog                 (CanLog, HasLoggerName (..), LoggerName)

import           Pos.Block.BListener         (MonadBListener (..), onApplyBlocksStub,
                                              onRollbackBlocksStub)
import           Pos.Block.Core              (Block, BlockHeader)
import           Pos.Block.Types             (Undo)
import           Pos.Communication.PeerState (PeerStateCtx, PeerStateTag,
                                              WithPeerState (..), clearPeerStateReal,
                                              getAllStatesReal, getPeerStateReal)
import           Pos.Context                 (NodeContext)
import           Pos.Core                    (IsHeader)
import           Pos.DB                      (MonadGState (..), NodeDBs)
import           Pos.DB.Block                (MonadBlockDBWrite (..), dbGetBlockReal,
                                              dbGetBlockReal', dbGetHeaderReal,
                                              dbGetHeaderReal', dbGetUndoReal,
                                              dbGetUndoReal', dbPutBlundReal)
import           Pos.DB.Class                (MonadBlockDBGeneric (..), MonadDB (..),
                                              MonadDBRead (..))
import           Pos.DB.DB                   (gsAdoptedBVDataDB)
import           Pos.DB.Redirect             (dbDeleteReal, dbGetReal, dbPutReal,
                                              dbWriteBatchReal)
import           Pos.Delegation.Class        (DelegationVar)
import           Pos.Discovery               (MonadDiscovery (..), findPeersReal,
                                              getPeersReal)
import           Pos.Slotting.Class          (MonadSlots (..))
import           Pos.Slotting.Impl.Sum       (currentTimeSlottingReal,
                                              getCurrentSlotBlockingReal,
                                              getCurrentSlotInaccurateReal,
                                              getCurrentSlotReal)
import           Pos.Slotting.MemState       (MonadSlotsData (..), getSlottingDataReal,
                                              getSystemStartReal, putSlottingDataReal,
                                              waitPenultEpochEqualsReal)
import           Pos.Ssc.Class.Helpers       (SscHelpersClass)
import           Pos.Ssc.Class.Types         (SscBlock)
import           Pos.Ssc.Extra               (SscMemTag, SscState)
import           Pos.Txp.MemState            (GenericTxpLocalData, TxpHolderTag)
import           Pos.Util                    (Some (..))
import           Pos.Util.JsonLog            (JsonLogConfig, jsonLogReal)
import           Pos.Util.TimeWarp           (CanJsonLog (..))
import           Pos.Util.Util               (PowerLift (..))
import           Pos.WorkMode.Class          (MinWorkMode, TxpExtra_TMP, WorkMode)

data RealModeContext ssc = RealModeContext
    { rmcNodeDBs       :: !NodeDBs
    , rmcSscMem        :: !(SscState ssc)
    , rmcTxpHolder     :: !(GenericTxpLocalData TxpExtra_TMP)
    , rmcDelegationVar :: !DelegationVar
    , rmcPeerState     :: !(PeerStateCtx Production)
    , rmcJsonLogConfig :: !JsonLogConfig
    , rmcLoggerName    :: !LoggerName
    , rmcNodeContext   :: !(NodeContext ssc)
    }

makeLensesFor
    [ ("rmcNodeDBs",       "rmcNodeDBsL")
    , ("rmcSscMem",        "rmcSscMemL")
    , ("rmcTxpHolder",     "rmcTxpHolderL")
    , ("rmcDelegationVar", "rmcDelegationVarL")
    , ("rmcPeerState",     "rmcPeerStateL")
    , ("rmcJsonLogConfig", "rmcJsonLogConfigL")
    , ("rmcLoggerName",    "rmcLoggerNameL")
    , ("rmcNodeContext",   "rmcNodeContextL") ]
    ''RealModeContext

instance {-# OVERLAPPABLE #-} HasLens tag (NodeContext ssc) r => HasLens tag (RealModeContext ssc) r where
    lensOf = rmcNodeContextL . lensOf @tag

instance HasLens NodeDBs (RealModeContext ssc) NodeDBs where
    lensOf = rmcNodeDBsL

instance HasLens SscMemTag (RealModeContext ssc) (SscState ssc) where
    lensOf = rmcSscMemL

instance HasLens TxpHolderTag (RealModeContext ssc) (GenericTxpLocalData TxpExtra_TMP) where
    lensOf = rmcTxpHolderL

instance HasLens DelegationVar (RealModeContext ssc) DelegationVar where
    lensOf = rmcDelegationVarL

instance r ~ PeerStateCtx Production => HasLens PeerStateTag (RealModeContext ssc) r where
    lensOf = rmcPeerStateL

instance HasLens JsonLogConfig (RealModeContext ssc) JsonLogConfig where
    lensOf = rmcJsonLogConfigL

instance HasLens LoggerName (RealModeContext ssc) LoggerName where
    lensOf = rmcLoggerNameL

----------------------------------------------------------------------------
-- Concrete types
----------------------------------------------------------------------------

data ContextTag

-- | RealMode is a basis for `WorkMode`s used to really run system.
type RealMode' ssc =
    Ether.ReaderT ContextTag (RealModeContext ssc) Production

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

instance HasLoggerName (RealMode ssc) where
    getLoggerName = Ether.ask'
    modifyLoggerName = Ether.local'

instance CanJsonLog (RealMode ssc) where
    jsonLog = jsonLogReal

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

instance HasLens tag (RealModeContext ssc) r => Ether.MonadReader tag r (RealMode ssc) where
    ask =
        (coerce :: RealMode' ssc r -> RealMode ssc r)
        (Ether.asks @ContextTag (view (lensOf @tag @(RealModeContext ssc) @r)))
    local f =
        (coerce :: forall a. (RealMode' ssc a -> RealMode' ssc a) -> (RealMode ssc a -> RealMode ssc a))
        (Ether.local @ContextTag (over (lensOf @tag @(RealModeContext ssc) @r) f))
