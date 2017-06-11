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
       , ServiceMode(..)
       ) where

import           Universum

import           Control.Monad.Base             (MonadBase)
import           Control.Monad.Fix
import           Control.Monad.Morph            (hoist)
import           Control.Monad.Trans.Control    (MonadBaseControl (..))
import qualified Control.Monad.Trans.Lift.Local as Lift
import           Control.Monad.Trans.Resource   (MonadResource, ResourceT)
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

import           Pos.Block.BListener            (BListenerStub, MonadBListener)
import           Pos.Communication.PeerState    (PeerStateCtx, PeerStateRedirect,
                                                 PeerStateTag, WithPeerState)
import           Pos.Context                    (NodeContext)
import           Pos.DB                         (DBPureRedirect, MonadGState, NodeDBs)
import           Pos.DB.Block                   (BlockDBRedirect, MonadBlockDBWrite)
import           Pos.DB.Class                   (MonadBlockDBGeneric (..), MonadDB,
                                                 MonadDBRead (..))
import           Pos.DB.DB                      (GStateCoreRedirect)
import           Pos.Delegation.Class           (DelegationVar)
import           Pos.Discovery                  (DiscoveryRedirect, MonadDiscovery)
import           Pos.Slotting.Class             (MonadSlots)
import           Pos.Slotting.MemState          (MonadSlotsData, SlottingVar)
import           Pos.Slotting.MemState.Holder   (SlotsDataRedirect)
import           Pos.Slotting.Ntp               (NtpSlottingVar, SlotsRedirect)
import           Pos.Ssc.Class.Helpers          (SscHelpersClass)
import           Pos.Ssc.Extra                  (SscMemTag, SscState)
import           Pos.Txp.MemState               (GenericTxpLocalData, TxpHolderTag)
import           Pos.Types                      (HeaderHash)
import           Pos.Util.Util                  (PowerLift (..))
import           Pos.WorkMode.Class             (MinWorkMode, TxpExtra_TMP, WorkMode)

----------------------------------------------------------------------------
-- Concrete types
----------------------------------------------------------------------------

-- | RealMode is a basis for `WorkMode`s used to really run system.
type RealMode' ssc =
    BListenerStub (
    GStateCoreRedirect (
    PeerStateRedirect (
    DiscoveryRedirect (
    SlotsRedirect (
    SlotsDataRedirect (
    BlockDBRedirect (
    DBPureRedirect (
    Ether.ReadersT
        ( Tagged NodeDBs NodeDBs
        , Tagged SlottingVar SlottingVar
        , Tagged (Bool, NtpSlottingVar) (Bool, NtpSlottingVar)
        , Tagged SscMemTag (SscState ssc)
        , Tagged TxpHolderTag (GenericTxpLocalData TxpExtra_TMP)
        , Tagged DelegationVar DelegationVar
        , Tagged PeerStateTag (PeerStateCtx Production)
        ) (
    Ether.ReadersT (NodeContext ssc) (
    LoggerNameBox (
    ResourceT Production
    )))))))))))

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
deriving instance MonadSlotsData (RealMode ssc)
deriving instance MonadSlots (RealMode ssc)
deriving instance MonadDiscovery (RealMode ssc)
deriving instance MonadGState (RealMode ssc)
deriving instance MonadDB (RealMode ssc)
deriving instance MonadResource (RealMode ssc)
instance MonadDBRead (RealMode ssc) where
    dbGet a b = RealMode $ dbGet a b
    dbIterSource t p = hoist (hoist RealMode) $ dbIterSource t p
deriving instance SscHelpersClass ssc => MonadBlockDBWrite ssc (RealMode ssc)
deriving instance MonadBListener (RealMode ssc)
-- deriving instance MonadUpdates (RealMode ssc)
-- deriving instance SscHelpersClass ssc => MonadBlockchainInfo (RealMode ssc)
-- deriving instance MonadBalances (RealMode ssc)
-- deriving instance MonadTxHistory (RealMode ssc)
deriving instance WithPeerState (RealMode ssc)

instance PowerLift m (RealMode' ssc) => PowerLift m (RealMode ssc) where
  powerLift = RealMode . powerLift

instance
    MonadBlockDBGeneric header blk undo (RealMode' ssc) =>
    MonadBlockDBGeneric header blk undo (RealMode ssc) where
    dbGetHeader = (coerce :: (HeaderHash -> RealMode' ssc (Maybe header)) ->
                             (HeaderHash -> RealMode ssc (Maybe header)))
                  (dbGetHeader @header @blk @undo)
    dbGetBlock = (coerce :: (HeaderHash -> RealMode' ssc (Maybe blk)) ->
                            (HeaderHash -> RealMode ssc (Maybe blk)))
                 (dbGetBlock @header @blk @undo)
    dbGetUndo = (coerce :: (HeaderHash -> RealMode' ssc (Maybe undo)) ->
                           (HeaderHash -> RealMode ssc (Maybe undo)))
                 (dbGetUndo @header @blk @undo)

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

-- | ServiceMode is the mode in which support nodes work.
type ServiceMode' =
    PeerStateRedirect (
    Ether.ReaderT PeerStateTag (PeerStateCtx Production) (
    LoggerNameBox Production
    ))

newtype ServiceMode a = ServiceMode (ServiceMode' a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadFix
    )
type instance ThreadId (ServiceMode) = ThreadId Production
type instance Promise (ServiceMode) = Promise Production
type instance SharedAtomicT (ServiceMode) = SharedAtomicT Production
type instance SharedExclusiveT (ServiceMode) = SharedExclusiveT Production
type instance Gauge (ServiceMode) = Gauge Production
type instance ChannelT (ServiceMode) = ChannelT Production
type instance Distribution (ServiceMode) = Distribution Production
type instance Counter (ServiceMode) = Counter Production

deriving instance CanLog (ServiceMode)
deriving instance HasLoggerName (ServiceMode)
deriving instance WithPeerState (ServiceMode)

instance PowerLift m ServiceMode' => PowerLift m (ServiceMode) where
  powerLift = ServiceMode . powerLift

instance
    ( Mockable d (ServiceMode')
    , MFunctor' d (ServiceMode) (ServiceMode')
    )
    => Mockable d (ServiceMode) where
    liftMockable dmt = ServiceMode $ liftMockable $ hoist' (\(ServiceMode m) -> m) dmt

instance
    Ether.MonadReader tag r ServiceMode' =>
    Ether.MonadReader tag r ServiceMode
  where
    ask =
        (coerce :: ServiceMode' r -> ServiceMode r)
        (Ether.ask @tag)
    local =
        (coerce :: forall a .
            Lift.Local r (ServiceMode') a ->
            Lift.Local r (ServiceMode) a)
        (Ether.local @tag)
    reader =
        (coerce :: forall a .
            ((r -> a) -> ServiceMode' a) ->
            ((r -> a) -> ServiceMode a))
        (Ether.reader @tag)
