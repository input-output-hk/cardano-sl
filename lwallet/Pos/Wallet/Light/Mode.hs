{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Stack of monads used by light wallet.

module Pos.Wallet.Light.Mode
       ( LightWalletMode (..)
       ) where

import           Universum

import           Control.Monad.Base             (MonadBase)
import           Control.Monad.Fix              (MonadFix)
import           Control.Monad.Trans.Identity   (IdentityT (..))
import qualified Control.Monad.Trans.Lift.Local as Lift
import           Data.Coerce                    (coerce)
import           Data.Tagged                    (Tagged (..))
import qualified Ether
import           Mockable                       (ChannelT, Counter, Distribution, Gauge,
                                                 MFunctor' (..), Mockable (..),
                                                 Production, Promise, SharedAtomicT,
                                                 SharedExclusiveT, ThreadId)
import           System.Wlog                    (CanLog, HasLoggerName, LoggerNameBox)

import           Pos.Block.BListener            (BListenerStub, MonadBListener)
import           Pos.Client.Txp.Balances        (MonadBalances (..))
import           Pos.Client.Txp.History         (MonadTxHistory (..))
import           Pos.Communication.PeerState    (PeerStateCtx, PeerStateRedirect,
                                                 PeerStateTag, WithPeerState)
import           Pos.Core                       (HeaderHash)
import           Pos.DB                         (DBPureRedirect, MonadBlockDBGeneric (..),
                                                 MonadDBRead, MonadGState, NodeDBs)
import           Pos.DB.Block                   (BlockDBRedirect)
import           Pos.Discovery                  (DiscoveryConstT, MonadDiscovery)
import           Pos.Reporting.MemState         (ReportingContext)
import           Pos.Util.TimeWarp              (CanJsonLog, JsonLogT)
import           Pos.Util.Util                  (PowerLift (..))
import           Pos.Wallet.KeyStorage          (KeyData)
import           Pos.Wallet.Light.Redirect      (BalancesWalletRedirect,
                                                 BlockchainInfoNotImplemented,
                                                 TxHistoryWalletRedirect,
                                                 UpdatesNotImplemented)
import           Pos.Wallet.Light.State.Acidic  (WalletState)
import           Pos.Wallet.Light.State.Core    (GStateCoreWalletRedirect)
import           Pos.Wallet.WalletMode          (MonadBlockchainInfo, MonadUpdates)

type LightWalletMode' =
    DiscoveryConstT (
    BListenerStub (
    BlockchainInfoNotImplemented (
    UpdatesNotImplemented (
    PeerStateRedirect (
    GStateCoreWalletRedirect (
    BalancesWalletRedirect (
    TxHistoryWalletRedirect (
    BlockDBRedirect (
    DBPureRedirect (
    Ether.ReadersT
        ( Tagged PeerStateTag (PeerStateCtx Production)
        , Tagged KeyData KeyData
        , Tagged WalletState WalletState
        , Tagged ReportingContext ReportingContext
        ) (
    JsonLogT (
    LoggerNameBox (
    Production
    )))))))))))))

newtype LightWalletMode a = LightWalletMode (LightWalletMode' a)
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
type instance ThreadId (LightWalletMode) = ThreadId Production
type instance Promise (LightWalletMode) = Promise Production
type instance SharedAtomicT (LightWalletMode) = SharedAtomicT Production
type instance SharedExclusiveT (LightWalletMode) = SharedExclusiveT Production
type instance Gauge (LightWalletMode) = Gauge Production
type instance ChannelT (LightWalletMode) = ChannelT Production
type instance Distribution (LightWalletMode) = Distribution Production
type instance Counter (LightWalletMode) = Counter Production

deriving instance CanLog (LightWalletMode)
deriving instance HasLoggerName (LightWalletMode)
--deriving instance MonadSlotsData (LightWalletMode)
--deriving instance MonadSlots (LightWalletMode)
deriving instance MonadDiscovery (LightWalletMode)
deriving instance MonadGState (LightWalletMode)
deriving instance Ether.MonadReader' NodeDBs Production => MonadDBRead (LightWalletMode)
deriving instance MonadBListener (LightWalletMode)
deriving instance MonadUpdates (LightWalletMode)
deriving instance MonadBlockchainInfo (LightWalletMode)
deriving instance MonadBalances (LightWalletMode)
deriving instance MonadTxHistory (LightWalletMode)
deriving instance WithPeerState (LightWalletMode)
deriving instance CanJsonLog (LightWalletMode)

instance PowerLift m (LightWalletMode') => PowerLift m (LightWalletMode) where
  powerLift = LightWalletMode . powerLift

instance
    ( Ether.MonadReader' NodeDBs Production
    , MonadBlockDBGeneric header blk undo (LightWalletMode') ) =>
    MonadBlockDBGeneric header blk undo (LightWalletMode) where
    dbGetHeader = (coerce :: (HeaderHash -> LightWalletMode' (Maybe header)) ->
                             (HeaderHash -> LightWalletMode (Maybe header)))
                  (dbGetHeader @header @blk @undo)
    dbGetBlock = (coerce :: (HeaderHash -> LightWalletMode' (Maybe blk)) ->
                            (HeaderHash -> LightWalletMode (Maybe blk)))
                 (dbGetBlock @header @blk @undo)
    dbGetUndo = (coerce :: (HeaderHash -> LightWalletMode' (Maybe undo)) ->
                           (HeaderHash -> LightWalletMode (Maybe undo)))
                 (dbGetUndo @header @blk @undo)

instance
    ( Mockable d (LightWalletMode')
    , MFunctor' d (LightWalletMode) (LightWalletMode')
    )
    => Mockable d (LightWalletMode) where
    liftMockable dmt = LightWalletMode $ liftMockable $ hoist' (\(LightWalletMode m) -> m) dmt

instance
    Ether.MonadReader tag r (LightWalletMode') =>
    Ether.MonadReader tag r (LightWalletMode)
  where
    ask =
        (coerce :: LightWalletMode' r -> LightWalletMode r)
        (Ether.ask @tag)
    local =
        (coerce :: forall a .
            Lift.Local r (LightWalletMode') a ->
            Lift.Local r (LightWalletMode) a)
        (Ether.local @tag)
    reader =
        (coerce :: forall a .
            ((r -> a) -> LightWalletMode' a) ->
            ((r -> a) -> LightWalletMode a))
        (Ether.reader @tag)
