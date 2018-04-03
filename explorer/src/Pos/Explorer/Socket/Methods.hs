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
import qualified Pos.Block.Logic as DB
import           Pos.Block.Types (Blund)
import           Pos.Core (Address, HeaderHash)
import           Pos.Core.Block (Block, mainBlockTxPayload)
import           Pos.Core.Txp (Tx (..), TxOut (..), TxOutAux (..), txOutAddress, txpTxs)
import           Pos.Crypto (hash, withHash)
import           Pos.DB.Block (getBlund)
import           Pos.DB.Class (MonadDBRead)
import           Pos.Explorer.Core (TxExtra (..))
import qualified Pos.Explorer.DB as DB
import qualified Pos.GState as DB
import           Pos.Util (maybeThrow)
import           Pos.Util.Chrono (getOldestFirst)
import           System.Wlog (WithLogger, logDebug, logWarning, modifyLoggerName)

import           Pos.Explorer.Aeson.ClientTypes ()
import           Pos.Explorer.ExplorerMode (ExplorerMode)
import           Pos.Explorer.Socket.Holder (ClientContext, ConnectionsState, ExplorerSocket (..),
                                             ExplorerSockets, ccAddress, ccConnection,
                                             csAddressSubscribers, csBlocksPageSubscribers,
                                             csClients, csEpochsLastPageSubscribers,
                                             csTxsSubscribers, mkClientContext, _ProdSocket)
import           Pos.Explorer.Socket.Util (EventName (..), emitTo)
import           Pos.Explorer.Web.ClientTypes (CAddress, CTxBrief, CTxEntry (..), EpochIndex (..),
                                               TxInternal (..), fromCAddress, toTxBrief)
import           Pos.Explorer.Web.Error (ExplorerError (..))
import           Pos.Explorer.Web.Server (getBlocksLastPage, getEpochPage, getEpochPagesOrThrow,
                                          topsortTxsOrFail)


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
    ( WithLogger m
    , MonadThrow m
    , MonadState ConnectionsState m
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
    => Socket -> m ()
startSession socket = do
    let cc = mkClientContext $ ProdSocket socket
        id = socketId socket
    csClients . at id .= Just cc
    logDebug $ sformat ("New session has started (#"%shown%")") id

finishSession
    :: SubscriptionMode m
    => SocketId -> m ()
finishSession sessId =
    whenJustM (use $ csClients . at sessId) $ \_ -> do
        unsubscribeFully sessId
        csClients . at sessId .= Nothing
        logDebug $ sformat ("Session #"%shown%" has finished") sessId

subscribe
    :: SubscriptionMode m
    => cli -> SubscriptionParam cli -> m ()
subscribe cliData sp@SubscriptionParam{..} = do
    unsubscribe sp
    session <- use $ csClients . at spSessId
    case session of
        Just _  -> do
            spSubscription cliData .= Just ()
            csClients . ix spSessId . spCliData .= Just cliData
            logDebug $ sformat ("Client #"%shown%" subscribed to "%stext%" \
                       \updates") spSessId (spDesc cliData)
        _       ->
            logWarning $ sformat ("Unregistered client tries to subscribe on "%
                         stext%" updates") (spDesc cliData)

unsubscribe
    :: SubscriptionMode m
    => SubscriptionParam cli -> m ()
unsubscribe SubscriptionParam{..} = do
    mCliData <- preuse $ csClients . ix spSessId . spCliData . _Just
    case mCliData of
        Just cliData -> do
            csClients . ix spSessId . spCliData .= Nothing
            spSubscription cliData .= Nothing
            logDebug $ sformat ("Client #"%shown%" unsubscribed from "%stext%
                       " updates") spSessId (spDesc cliData)
        Nothing ->
            logDebug $ sformat ("Client #"%shown%" unsubscribes from action \
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
    => CAddress -> SocketId -> m ()
subscribeAddr caddr sessId = do
    addr <- fromCAddressOrThrow caddr
    subscribe addr (addrSubParam sessId)

unsubscribeAddr
    :: SubscriptionMode m
    => SocketId -> m ()
unsubscribeAddr sessId = unsubscribe (addrSubParam sessId)

subscribeBlocksLastPage
    :: SubscriptionMode m
    => SocketId -> m ()
subscribeBlocksLastPage sessId = subscribe () (blockPageSubParam sessId)

unsubscribeBlocksLastPage
    :: SubscriptionMode m
    => SocketId -> m ()
unsubscribeBlocksLastPage sessId = unsubscribe (blockPageSubParam sessId)

subscribeTxs
    :: SubscriptionMode m
    => SocketId -> m ()
subscribeTxs sessId = subscribe () (txsSubParam sessId)

unsubscribeTxs
    :: SubscriptionMode m
    => SocketId -> m ()
unsubscribeTxs sessId = unsubscribe (txsSubParam sessId)

subscribeEpochsLastPage
    :: SubscriptionMode m
    => SocketId -> m ()
subscribeEpochsLastPage sessId =
    subscribe () (epochsLastPageSubParam sessId)

unsubscribeEpochsLastPage
    :: SubscriptionMode m
    => SocketId -> m ()
unsubscribeEpochsLastPage sessId = unsubscribe (epochsLastPageSubParam sessId)

unsubscribeFully
    :: SubscriptionMode m
    => SocketId -> m ()
unsubscribeFully sessId = do
    logDebug $ sformat ("Client #"%shown%" unsubscribes from all updates")
               sessId
    modifyLoggerName (const "drop") $ do
        unsubscribeAddr sessId
        unsubscribeBlocksLastPage sessId
        unsubscribeTxs sessId
        unsubscribeEpochsLastPage sessId

-- * Notifications

broadcast
    :: (ExplorerMode ctx m, EventName event, ToJSON args)
    => event -> args -> Set SocketId -> ExplorerSockets m ()
broadcast event args recipients = do
    forM_ recipients $ \sockid -> do
        mSock <- preview $ csClients . ix sockid . ccConnection . _ProdSocket
        case mSock of
            Nothing   -> logWarning $
                sformat ("No socket with SocketId="%shown%" registered for using in production") sockid
            Just sock -> emitTo sock event args
                `catchAny` handler sockid
  where
    handler sockid = logWarning .
        sformat ("Failed to send to SocketId="%shown%": "%shown) sockid

notifyAddrSubscribers
    :: forall ctx m . ExplorerMode ctx m
    => Address -> [CTxBrief] -> ExplorerSockets m ()
notifyAddrSubscribers addr cTxEntries = do
    mRecipients <- view $ csAddressSubscribers . at addr
    whenJust mRecipients $ broadcast @ctx AddrUpdated cTxEntries

notifyBlocksLastPageSubscribers
    :: forall ctx m . ExplorerMode ctx m
    => ExplorerSockets m ()
notifyBlocksLastPageSubscribers = do
    recipients <- view csBlocksPageSubscribers
    blocks     <- lift $ getBlocksLastPage @ctx
    broadcast @ctx BlocksLastPageUpdated blocks recipients

notifyTxsSubscribers
    :: forall ctx m . ExplorerMode ctx m
    => [CTxEntry] -> ExplorerSockets m ()
notifyTxsSubscribers cTxEntries =
    view csTxsSubscribers >>= broadcast @ctx TxsUpdated cTxEntries

notifyEpochsLastPageSubscribers
    :: forall ctx m . ExplorerMode ctx m
    => EpochIndex -> ExplorerSockets m ()
notifyEpochsLastPageSubscribers currentEpoch = do
    -- subscriber
    recipients <- view $ csEpochsLastPageSubscribers
    -- last epoch page
    lastPage <- lift $ getEpochPagesOrThrow currentEpoch
    -- epochs of last page
    epochs <- lift $ getEpochPage @ctx currentEpoch $ Just lastPage
    broadcast @ctx EpochsLastPageUpdated epochs recipients

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
    :: (MonadDBRead m, WithLogger m)
    => Tx -> m (S.Set Address)
addrsTouchedByTx tx = do
      -- for each transaction, get its OutTx
      -- and transactions from InTx
      inTxs <- forM (_txInputs tx) $ DB.getTxOut >=> \case
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
