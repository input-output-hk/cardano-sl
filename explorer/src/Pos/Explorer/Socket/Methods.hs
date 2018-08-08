{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Logic of Explorer socket-io Server.

module Pos.Explorer.Socket.Methods
       ( Subscription (..)
       , ClientEvent (..)
       , ServerEvent (..)
       , SubscriptionParam (..)

       -- * Creating `SubscriptionParam`s
       , addrSubParam
       , blockPageSubParam
       , txsSubParam

       -- Sessions
       , startSession
       , finishSession

       -- Un-/Subscriptions
       , subscribeAddr
       , subscribeBlocksLastPage
       , subscribeEpochsLastPage
       , subscribeTxs
       , unsubscribeAddr
       , unsubscribeBlocksLastPage
       , unsubscribeEpochsLastPage
       , unsubscribeFully
       , unsubscribeTxs

       -- * Notifications
       , notifyAddrSubscribers
       , notifyBlocksLastPageSubscribers
       , notifyTxsSubscribers
       , notifyEpochsLastPageSubscribers

      -- * DB data
       , getBlundsFromTo
       , getBlockTxs
       , getTxInfo

       -- * Helper
       , addressSetByTxs
       , addrsTouchedByTx
       , fromCAddressOrThrow

       -- needed by tests
       , SubscriptionMode
       ) where

import           Universum hiding (id)

import           Control.Lens (at, ix, lens, non, (.=), _Just)
import           Control.Monad.State (MonadState)
import           Data.Aeson (ToJSON)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import           Formatting (sformat, shown, stext, (%))
import           Network.EngineIO (SocketId)
import           Network.SocketIO (Socket, socketId)
import           Pos.Chain.Block (Block, Blund, HeaderHash, mainBlockTxPayload)
import           Pos.Core (Address)
import           Pos.Core.Chrono (getOldestFirst)
import           Pos.Core.Txp (Tx (..), TxOut (..), TxOutAux (..), txOutAddress,
                     txpTxs)
import           Pos.Crypto (hash, withHash)
import           Pos.DB.Block (getBlund)
import qualified Pos.DB.Block as DB
import           Pos.DB.Class (MonadDBRead)
import           Pos.DB.Txp (getTxOut)
import           Pos.Explorer.Core (TxExtra (..))
import qualified Pos.Explorer.DB as DB
import           Pos.Util (maybeThrow)
import           Pos.Util.Trace.Named (TraceNamed, appendName, logDebug,
                     logWarning, natTrace)

import           Pos.Explorer.Aeson.ClientTypes ()
import           Pos.Explorer.ExplorerMode (ExplorerMode)
import           Pos.Explorer.Socket.Holder (ClientContext, ConnectionsState,
                     ExplorerSocket (..), ExplorerSockets, ccAddress,
                     ccConnection, csAddressSubscribers,
                     csBlocksPageSubscribers, csClients,
                     csEpochsLastPageSubscribers, csTxsSubscribers,
                     mkClientContext, _ProdSocket)
import           Pos.Explorer.Socket.Util (EventName (..), emitTo)
import           Pos.Explorer.Web.ClientTypes (CAddress, CTxBrief,
                     CTxEntry (..), EpochIndex (..), TxInternal (..),
                     fromCAddress, toTxBrief)
import           Pos.Explorer.Web.Error (ExplorerError (..))
import           Pos.Explorer.Web.Server (getBlocksLastPage, getEpochPage,
                     getEpochPagesOrThrow, topsortTxsOrFail)


-- * Event names

data Subscription
    = SubAddr
    | SubBlockLastPage  -- ^ subscribe on blocks last page (latest blocks)
    | SubEpochsLastPage -- ^ subscribe on epochs last page (latest epoch)
    | SubTx
    deriving (Show, Generic)

data ClientEvent
    = Subscribe Subscription
    | Unsubscribe Subscription
    | CallMe
    deriving (Show, Generic)

instance EventName ClientEvent where
    toName = show

data ServerEvent
    = AddrUpdated
    | BlocksLastPageUpdated
    | EpochsLastPageUpdated
    | TxsUpdated
    | CallYou
    deriving (Show, Generic)

instance EventName ServerEvent where
    toName = show

-- * Util

fromCAddressOrThrow :: MonadThrow m => CAddress -> m Address
fromCAddressOrThrow =
    either (\_ -> throwM $ Internal "Malformed address") return .
    fromCAddress

-- * Client requests provessing

type SubscriptionMode m =
    ( MonadThrow m
    , MonadState ConnectionsState m
    , MonadIO m
    )

-- | This describes points related to some subscribtion action.
-- Each subscription type has its own /client data/ which user provides
-- as parameter when subscribes, it is then stored in 'ClientContext' type.
data SubscriptionParam cli = SubscriptionParam
    { -- | Identificator of current session
      spSessId       :: SocketId
      -- | Description of this subscription
    , spDesc         :: cli -> Text
      -- | Sets current session subscribed / unsubscribed
    , spSubscription :: cli -> Lens' ConnectionsState (Maybe ())
      -- | Sets related client data present / absent
    , spCliData      :: Lens' ClientContext (Maybe cli)
    }

startSession
    :: SubscriptionMode m
    => TraceNamed IO -> Socket -> m ()
startSession logTrace socket = do
    let cc = mkClientContext $ ProdSocket socket
        id = socketId socket
    csClients . at id .= Just cc
    liftIO $ logDebug logTrace $ sformat ("New session has started (#"%shown%")") id

finishSession
    :: SubscriptionMode m
    => TraceNamed IO -> SocketId -> m ()
finishSession logTrace sessId =
    whenJustM (use $ csClients . at sessId) $ \_ -> do
        unsubscribeFully logTrace sessId
        csClients . at sessId .= Nothing
        liftIO $ logDebug logTrace $ sformat ("Session #"%shown%" has finished") sessId

subscribe
    :: SubscriptionMode m
    => TraceNamed IO -> cli -> SubscriptionParam cli -> m ()
subscribe logTrace cliData sp@SubscriptionParam{..} = do
    unsubscribe logTrace sp
    session <- use $ csClients . at spSessId
    let logTrace' = natTrace liftIO logTrace
    case session of
        Just _  -> do
            spSubscription cliData .= Just ()
            csClients . ix spSessId . spCliData .= Just cliData
            logDebug logTrace' $ sformat ("Client #"%shown%" subscribed to "%stext%" \
                       \updates") spSessId (spDesc cliData)
        _       ->
            logWarning logTrace' $ sformat ("Unregistered client tries to subscribe on "%
                         stext%" updates") (spDesc cliData)

unsubscribe
    :: SubscriptionMode m
    => TraceNamed IO -> SubscriptionParam cli -> m ()
unsubscribe logTrace SubscriptionParam{..} = do
    mCliData <- preuse $ csClients . ix spSessId . spCliData . _Just
    let logTrace' = natTrace liftIO logTrace
    case mCliData of
        Just cliData -> do
            csClients . ix spSessId . spCliData .= Nothing
            spSubscription cliData .= Nothing
            logDebug logTrace' $ sformat ("Client #"%shown%" unsubscribed from "%stext%
                       " updates") spSessId (spDesc cliData)
        Nothing ->
            logDebug logTrace' $ sformat ("Client #"%shown%" unsubscribes from action \
                       \at which it wasn't subscribed") spSessId

-- | This is hack which makes client data look like always be present in
-- 'ClientContext'.
-- It's exploited in 'unsubscribe' then, because it works only if client data
-- is present.
noCliDataKept :: Lens' a (Maybe ())
noCliDataKept = lens (\_ -> Just ()) const

addrSubParam :: SocketId -> SubscriptionParam Address
addrSubParam sessId =
    SubscriptionParam
        { spSessId        = sessId
        , spDesc          = ("address " <> ) . show
        , spSubscription  = \addr ->
            csAddressSubscribers . at addr . non S.empty . at sessId
        , spCliData       = ccAddress
        }

blockPageSubParam :: SocketId -> SubscriptionParam ()
blockPageSubParam sessId =
    SubscriptionParam
        { spSessId       = sessId
        , spDesc         = const "blockchain last page"
        , spSubscription = \_ -> csBlocksPageSubscribers . at sessId
        , spCliData      = noCliDataKept
        }

txsSubParam :: SocketId -> SubscriptionParam ()
txsSubParam sessId =
    SubscriptionParam
        { spSessId       = sessId
        , spDesc         = const "txs"
        , spSubscription = \_ -> csTxsSubscribers . at sessId
        , spCliData      = noCliDataKept
        }


epochsLastPageSubParam :: SocketId -> SubscriptionParam ()
epochsLastPageSubParam sessId =
    SubscriptionParam
        { spSessId       = sessId
        , spDesc         = const "epochs last page"
        , spSubscription = \_ -> csEpochsLastPageSubscribers . at sessId
        , spCliData      = noCliDataKept
        }

-- | Unsubscribes on any previous address and subscribes on given one.
subscribeAddr
    :: SubscriptionMode m
    => TraceNamed IO -> CAddress -> SocketId -> m ()
subscribeAddr logTrace caddr sessId = do
    addr <- fromCAddressOrThrow caddr
    subscribe logTrace addr (addrSubParam sessId)

unsubscribeAddr
    :: SubscriptionMode m
    => TraceNamed IO -> SocketId -> m ()
unsubscribeAddr logTrace sessId = unsubscribe logTrace (addrSubParam sessId)

subscribeBlocksLastPage
    :: SubscriptionMode m
    => TraceNamed IO -> SocketId -> m ()
subscribeBlocksLastPage logTrace sessId = subscribe logTrace () (blockPageSubParam sessId)

unsubscribeBlocksLastPage
    :: SubscriptionMode m
    => TraceNamed IO -> SocketId -> m ()
unsubscribeBlocksLastPage logTrace sessId = unsubscribe logTrace (blockPageSubParam sessId)

subscribeTxs
    :: SubscriptionMode m
    => TraceNamed IO -> SocketId -> m ()
subscribeTxs logTrace sessId = subscribe logTrace () (txsSubParam sessId)

unsubscribeTxs
    :: SubscriptionMode m
    => TraceNamed IO -> SocketId -> m ()
unsubscribeTxs logTrace sessId = unsubscribe logTrace (txsSubParam sessId)

subscribeEpochsLastPage
    :: SubscriptionMode m
    => TraceNamed IO -> SocketId -> m ()
subscribeEpochsLastPage logTrace sessId =
    subscribe logTrace () (epochsLastPageSubParam sessId)

unsubscribeEpochsLastPage
    :: SubscriptionMode m
    => TraceNamed IO -> SocketId -> m ()
unsubscribeEpochsLastPage logTrace sessId = unsubscribe logTrace (epochsLastPageSubParam sessId)

unsubscribeFully
    :: SubscriptionMode m
    => TraceNamed IO -> SocketId -> m ()
unsubscribeFully logTrace sessId = do
    logDebug (natTrace liftIO logTrace) $ sformat ("Client #"%shown%" unsubscribes from all updates")
               sessId
    let logTrace' = appendName "drop" logTrace
    unsubscribeAddr logTrace' sessId
    unsubscribeBlocksLastPage logTrace' sessId
    unsubscribeTxs logTrace' sessId
    unsubscribeEpochsLastPage logTrace' sessId

-- * Notifications

broadcast
    :: (ExplorerMode ctx m, EventName event, ToJSON args)
    => TraceNamed IO -> event -> args -> Set SocketId -> ExplorerSockets m ()
broadcast logTrace event args recipients = do
    forM_ recipients $ \sockid -> do
        mSock <- preview $ csClients . ix sockid . ccConnection . _ProdSocket
        case mSock of
            Nothing   -> logWarning (natTrace liftIO logTrace) $
                sformat ("No socket with SocketId="%shown%" registered for using in production") sockid
            Just sock -> emitTo sock event args
                `catchAny` handler sockid
  where
    handler sockid = logWarning (natTrace liftIO logTrace) .
        sformat ("Failed to send to SocketId="%shown%": "%shown) sockid

notifyAddrSubscribers
    :: forall ctx m . ExplorerMode ctx m
    => TraceNamed IO -> Address -> [CTxBrief] -> ExplorerSockets m ()
notifyAddrSubscribers logTrace addr cTxEntries = do
    mRecipients <- view $ csAddressSubscribers . at addr
    whenJust mRecipients $ broadcast @ctx (natTrace liftIO logTrace) AddrUpdated cTxEntries

notifyBlocksLastPageSubscribers
    :: forall ctx m . ExplorerMode ctx m
    => TraceNamed IO -> ExplorerSockets m ()
notifyBlocksLastPageSubscribers logTrace = do
    recipients <- view csBlocksPageSubscribers
    blocks     <- lift $ getBlocksLastPage @ctx
    broadcast @ctx (natTrace liftIO logTrace) BlocksLastPageUpdated blocks recipients

notifyTxsSubscribers
    :: forall ctx m . ExplorerMode ctx m
    => TraceNamed IO -> [CTxEntry] -> ExplorerSockets m ()
notifyTxsSubscribers logTrace cTxEntries =
    view csTxsSubscribers >>= broadcast @ctx (natTrace liftIO logTrace) TxsUpdated cTxEntries

notifyEpochsLastPageSubscribers
    :: forall ctx m . ExplorerMode ctx m
    => TraceNamed IO -> EpochIndex -> ExplorerSockets m ()
notifyEpochsLastPageSubscribers logTrace currentEpoch = do
    -- subscriber
    recipients <- view $ csEpochsLastPageSubscribers
    -- last epoch page
    lastPage <- lift $ getEpochPagesOrThrow currentEpoch
    -- epochs of last page
    epochs <- lift $ getEpochPage @ctx currentEpoch $ Just lastPage
    broadcast @ctx (natTrace liftIO logTrace) EpochsLastPageUpdated epochs recipients

-- * Helpers

-- | Gets blocks from recent inclusive to old one exclusive.
getBlundsFromTo
    :: forall ctx m . ExplorerMode ctx m
    => HeaderHash -> HeaderHash -> m (Maybe [Blund])
getBlundsFromTo recentBlock oldBlock =
    DB.getHashesRange Nothing oldBlock recentBlock >>= \case
        Left _ -> pure Nothing
        Right (getOldestFirst -> hashes) ->
            Just . catMaybes <$> forM (NE.tail hashes) getBlund

addrsTouchedByTx
    :: MonadDBRead m
    => Tx -> m (S.Set Address)
addrsTouchedByTx tx = do
      -- for each transaction, get its OutTx
      -- and transactions from InTx
      inTxs <- forM (_txInputs tx) $ getTxOut >=> \case
      -- inTxs :: NonEmpty [TxOut]
          -- TODO [CSM-153]: lookup mempool as well
          Nothing       -> return mempty
          Just txOutAux -> return . one $ toaOut txOutAux

      pure $ addressSetByTxs (_txOutputs tx) inTxs

-- | Helper to filter addresses by a given tx from a list of txs
addressSetByTxs :: NonEmpty TxOut -> NonEmpty [TxOut] -> (S.Set Address)
addressSetByTxs tx txs =
    let txs' = (toList tx) <> (concat txs) in
    S.fromList $ txOutAddress <$> txs'

getBlockTxs
    :: forall ctx m . (ExplorerMode ctx m)
    => Block -> m [TxInternal]
getBlockTxs (Left  _  ) = return []
getBlockTxs (Right blk) = do
    txs <- topsortTxsOrFail withHash $ toList $ blk ^. mainBlockTxPayload . txpTxs
    forM txs $ \tx -> do
        extra@TxExtra {..} <- DB.getTxExtra (hash tx) >>=
            maybeThrow (Internal "In-block transaction doesn't \
                                 \have extra info in DB")
        pure $ TxInternal extra tx

getTxInfo
    :: (ExplorerMode ctx m)
    => TxInternal -> m (CTxBrief, S.Set Address)
getTxInfo tx = do
    let ctxBrief = toTxBrief tx
    addrs <- addrsTouchedByTx (tiTx tx)
    return (ctxBrief, addrs)
