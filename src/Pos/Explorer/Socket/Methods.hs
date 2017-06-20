{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Logic of Explorer socket-io Server.

module Pos.Explorer.Socket.Methods
       ( Subscription (..)
       , ClientEvent (..)
       , ServerEvent (..)

       , startSession
       , finishSession
       , subscribeAddr
       , subscribeBlocks
       , subscribeBlocksLastPage
       , subscribeBlocksOff
       , subscribeTxs
       , unsubscribeAddr
       , unsubscribeBlocks
       , unsubscribeBlocksLastPage
       , unsubscribeBlocksOff
       , unsubscribeTxs

       , notifyAddrSubscribers
       , notifyBlocksSubscribers
       , notifyBlocksLastPageSubscribers
       , notifyBlocksOffSubscribers
       , notifyTxsSubscribers
       , getBlocksFromTo
       , addrsTouchedByTx
       , getBlockTxs
       , getTxInfo
       ) where

import           Control.Lens                   (at, ix, lens, non, (.=), _Just)
import           Control.Monad.State            (MonadState)
import           Data.Aeson                     (ToJSON)
import qualified Data.Map                       as M
import qualified Data.Set                       as S
import           Formatting                     (sformat, shown, stext, (%))
import           Network.EngineIO               (SocketId)
import           Network.SocketIO               (Socket, socketId)
import           Pos.Block.Core                 (Block, mainBlockTxPayload)
import qualified Pos.Block.Logic                as DB
import           Pos.Crypto                     (withHash)
import           Pos.Crypto                     (hash)
import qualified Pos.DB.Block                   as DB
import           Pos.DB.Class                   (MonadDBRead)
import qualified Pos.DB.GState                  as DB
import           Pos.Explorer                   (TxExtra (..))
import qualified Pos.Explorer                   as DB
import           Pos.Ssc.GodTossing             (SscGodTossing)
import           Pos.Txp                        (Tx (..), TxOut (..), TxOutAux (..),
                                                 txOutAddress, txpTxs)
import           Pos.Types                      (Address, HeaderHash)
import           Pos.Util                       (maybeThrow)
import           Pos.Util.Chrono                (getOldestFirst)
import           System.Wlog                    (WithLogger, logDebug, logWarning,
                                                 modifyLoggerName)
import           Universum

import           Pos.Explorer.Aeson.ClientTypes ()
import           Pos.Explorer.Socket.Holder     (ClientContext, ConnectionsState,
                                                 ccAddress, ccBlockOff, ccConnection,
                                                 csAddressSubscribers,
                                                 csBlocksOffSubscribers,
                                                 csBlocksPageSubscribers,
                                                 csBlocksSubscribers, csClients,
                                                 csTxsSubscribers, mkClientContext)
import           Pos.Explorer.Socket.Util       (EventName (..), emitTo)
import           Pos.Explorer.Web.ClientTypes   (CAddress, CTxBrief, CTxEntry (..),
                                                 TxInternal (..), fromCAddress,
                                                 toBlockEntry, toTxBrief)
import           Pos.Explorer.Web.Error         (ExplorerError (..))
import           Pos.Explorer.Web.Server        (ExplorerMode, getBlocksLastPage,
                                                 getLastBlocks, topsortTxsOrFail)

-- * Event names

data Subscription
    = SubAddr
    | SubBlock
    | SubBlockLastPage  -- ^ subscribe on blocks last page (newest blocks)
    | SubBlockOff       -- ^ subscribe on blocks with given offset
    | SubTx
    deriving (Show, Generic)

data ClientEvent
    = Subscribe Subscription
    | Unsubscribe Subscription
    | CallMe
    | CallMeString
    | CallMeTxId
    deriving (Show, Generic)

instance EventName ClientEvent where
    toName = show

data ServerEvent
    = AddrUpdated
    | BlocksUpdated
    | BlocksLastPageUpdated
    | BlocksOffUpdated
    | TxsUpdated
    -- TODO: test events, remove one day
    | CallYou
    | CallYouString
    | CallYouTxId
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
startSession conn = do
    let cc = mkClientContext conn
        id = socketId conn
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

blockSubParam :: SocketId -> SubscriptionParam ()
blockSubParam sessId =
    SubscriptionParam
        { spSessId       = sessId
        , spDesc         = const "blockchain"
        , spSubscription = \_ -> csBlocksSubscribers . at sessId
        , spCliData      = noCliDataKept
        }

blockPageSubParam :: SocketId -> SubscriptionParam ()
blockPageSubParam sessId =
    SubscriptionParam
        { spSessId       = sessId
        , spDesc         = const "blockchain last page"
        , spSubscription = \_ -> csBlocksPageSubscribers . at sessId
        , spCliData      = noCliDataKept
        }

blockOffSubParam :: SocketId -> SubscriptionParam Word
blockOffSubParam sessId =
    SubscriptionParam
        { spSessId       = sessId
        , spDesc         = ("blockchain with offset " <> ) . show
        , spSubscription = \offset ->
            csBlocksOffSubscribers . at offset . non mempty . at sessId
        , spCliData      = ccBlockOff
        }

txsSubParam :: SocketId -> SubscriptionParam ()
txsSubParam sessId =
    SubscriptionParam
        { spSessId       = sessId
        , spDesc         = const "txs"
        , spSubscription = \_ -> csTxsSubscribers . at sessId
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

subscribeBlocks
    :: SubscriptionMode m
    => SocketId -> m ()
subscribeBlocks sessId = subscribe () (blockSubParam sessId)

unsubscribeBlocks
    :: SubscriptionMode m
    => SocketId -> m ()
unsubscribeBlocks sessId = unsubscribe (blockSubParam sessId)

subscribeBlocksLastPage
    :: SubscriptionMode m
    => SocketId -> m ()
subscribeBlocksLastPage sessId = subscribe () (blockPageSubParam sessId)

unsubscribeBlocksLastPage
    :: SubscriptionMode m
    => SocketId -> m ()
unsubscribeBlocksLastPage sessId = unsubscribe (blockPageSubParam sessId)

subscribeBlocksOff
    :: SubscriptionMode m
    => Word -> SocketId -> m ()
subscribeBlocksOff offset sessId = subscribe offset (blockOffSubParam sessId)

unsubscribeBlocksOff
    :: SubscriptionMode m
    => SocketId -> m ()
unsubscribeBlocksOff sessId = unsubscribe (blockOffSubParam sessId)

subscribeTxs
    :: SubscriptionMode m
    => SocketId -> m ()
subscribeTxs sessId = subscribe () (txsSubParam sessId)

unsubscribeTxs
    :: SubscriptionMode m
    => SocketId -> m ()
unsubscribeTxs sessId = unsubscribe (txsSubParam sessId)

unsubscribeFully
    :: SubscriptionMode m
    => SocketId -> m ()
unsubscribeFully sessId = do
    logDebug $ sformat ("Client #"%shown%" unsubscribes from all updates")
               sessId
    modifyLoggerName (const "drop") $ do
        unsubscribeAddr sessId
        unsubscribeBlocks sessId
        unsubscribeBlocksOff sessId
        unsubscribeTxs sessId

-- * Notifications

type NotificationMode m =
    ( ExplorerMode m
    , MonadReader ConnectionsState m
    )

broadcast
    :: (NotificationMode m, EventName event, ToJSON args)
    => event -> args -> Set SocketId -> m ()
broadcast event args recipients = do
    forM_ recipients $ \sockid -> do
        mSock <- preview $ csClients . ix sockid . ccConnection
        case mSock of
            Nothing   -> logWarning $
                sformat ("No socket with SocketId="%shown%" registered") sockid
            Just sock -> emitTo sock event args
                `catchAll` handler sockid
  where
    handler sockid = logWarning .
        sformat ("Failed to send to SocketId="%shown%": "%shown) sockid

notifyAddrSubscribers
    :: NotificationMode m
    => Address -> [CTxBrief] -> m ()
notifyAddrSubscribers addr cTxEntries = do
    mRecipients <- view $ csAddressSubscribers . at addr
    whenJust mRecipients $ broadcast AddrUpdated cTxEntries

notifyBlocksLastPageSubscribers
    :: (NotificationMode m)
    => m ()
notifyBlocksLastPageSubscribers = do
    recipients <- view csBlocksPageSubscribers
    blocks     <- getBlocksLastPage
    broadcast BlocksLastPageUpdated blocks recipients

notifyBlocksSubscribers
    :: (NotificationMode m)
    => [Block SscGodTossing] -> m ()
notifyBlocksSubscribers blocks = do
    recipients <- view csBlocksSubscribers
    cblocks    <- catMaybes <$> forM blocks toClientType
    broadcast BlocksUpdated cblocks recipients
  where
    toClientType (Left _)          = return Nothing
    toClientType (Right mainBlock) = Just <$> toBlockEntry mainBlock

notifyBlocksOffSubscribers
    :: NotificationMode m
    => Int -> m ()
notifyBlocksOffSubscribers newBlocksNum = do
    recipientsByOffset <- M.toList <$> view csBlocksOffSubscribers
    for_ recipientsByOffset $ \(offset, recipients) -> do
        cblocks <- getLastBlocks (fromIntegral newBlocksNum) offset
        broadcast BlocksOffUpdated cblocks recipients

notifyTxsSubscribers
    :: NotificationMode m
    => [CTxEntry] -> m ()
notifyTxsSubscribers cTxEntries =
    view csTxsSubscribers >>= broadcast TxsUpdated cTxEntries

-- * Helpers

-- | Gets blocks from recent inclusive to old one exclusive.
getBlocksFromTo
    :: (ExplorerMode m)
    => HeaderHash -> HeaderHash -> m (Maybe [Block SscGodTossing])
getBlocksFromTo recentBlock oldBlock = do
    mheaders <- DB.getHeadersFromToIncl @SscGodTossing oldBlock recentBlock
    forM (getOldestFirst <$> mheaders) $ \(_ :| headers) ->
        fmap catMaybes $ forM headers (DB.blkGetBlock @SscGodTossing)

addrsTouchedByTx
    :: (MonadDBRead m, WithLogger m)
    => Tx -> m (S.Set Address)
addrsTouchedByTx tx = do
    -- for each transaction, get its OutTx
    -- and transactions from InTx
    inTxs <- forM (_txInputs tx) $ DB.getTxOut >=> \case
        -- TODO [CSM-153]: lookup mempool as well
        Nothing    -> mempty <$ return () -- logError "Can't find input of transaction!"
        Just txOut -> return . one $ toaOut txOut

    let relatedTxs = toList (_txOutputs tx) <> concat (toList inTxs)
    return . S.fromList $ txOutAddress <$> relatedTxs

getBlockTxs
    :: (ExplorerMode m)
    => Block ssc -> m [TxInternal]
getBlockTxs (Left  _  ) = return []
getBlockTxs (Right blk) = do
    txs <- topsortTxsOrFail withHash $ toList $ blk ^. mainBlockTxPayload . txpTxs
    forM txs $ \tx -> do
        extra@TxExtra {..} <- DB.getTxExtra (hash tx) >>=
            maybeThrow (Internal "In-block transaction doesn't \
                                 \have extra info in DB")
        pure $ TxInternal extra tx

getTxInfo
    :: (ExplorerMode m)
    => TxInternal -> m (CTxBrief, S.Set Address)
getTxInfo tx = do
    let ctxBrief = toTxBrief tx
    addrs <- addrsTouchedByTx (tiTx tx)
    return (ctxBrief, addrs)
