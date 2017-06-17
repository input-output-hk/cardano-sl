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

import           Pos.Block.BListener            (BListenerStub, MonadBListener)
import           Pos.Communication.PeerState    (PeerStateCtx, PeerStateRedirect,
                                                 PeerStateTag, WithPeerState)
import           Pos.Context                    (NodeContext)
import           Pos.DB                         (DBPureRedirect, MonadGState, NodeDBs)
import           Pos.DB.Block                   (BlockDBRedirect, MonadBlockDBWrite)
import           Pos.DB.Class                   (MonadBlockDBGeneric (..), MonadDB,
                                                 MonadDBRead)
import           Pos.DB.DB                      (GStateCoreRedirect)
import           Pos.Delegation.Class           (DelegationVar)
import           Pos.Discovery                  (DiscoveryRedirect, MonadDiscovery)
import           Pos.Slotting.Class             (MonadSlots)
import           Pos.Slotting.Impl              (SlotsRedirect)
import           Pos.Slotting.MemState          (MonadSlotsData)
import           Pos.Slotting.MemState.Holder   (SlotsDataRedirect)
import           Pos.Ssc.Class.Helpers          (SscHelpersClass)
import           Pos.Ssc.Extra                  (SscMemTag, SscState)
import           Pos.Txp.MemState               (GenericTxpLocalData, TxpHolderTag)
import           Pos.Types                      (HeaderHash)
import           Pos.Util.TimeWarp              (CanJsonLog, JsonLogT)
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
        , Tagged SscMemTag (SscState ssc)
        , Tagged TxpHolderTag (GenericTxpLocalData TxpExtra_TMP)
        , Tagged DelegationVar DelegationVar
        , Tagged PeerStateTag (PeerStateCtx Production)
        ) (
    Ether.ReadersT (NodeContext ssc) (
    JsonLogT (
    LoggerNameBox (
    Production
    ))))))))))))

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
deriving instance MonadDBRead (RealMode ssc)
deriving instance MonadDB (RealMode ssc)
deriving instance SscHelpersClass ssc => MonadBlockDBWrite ssc (RealMode ssc)
deriving instance MonadBListener (RealMode ssc)
deriving instance WithPeerState (RealMode ssc)
deriving instance CanJsonLog (RealMode ssc)

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
