{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Logic of Explorer socket-io Server.

module Pos.Explorer.Socket.Methods
       ( Subscription (..)
       , ClientEvent (..)
       , ServerEvent (..)

       , startSession
       , finishSession
       , subscribeAddr
       , subscribeBlocks
       , subscribeTxs
       , unsubscribeAddr
       , unsubscribeBlocks
       , unsubscribeTxs

       , notifyAddrSubscribers
       , notifyBlocksSubscribers
       , notifyTxsSubscribers
       , getBlocksFromTo
       , addrsTouchedByTx
       , getBlockTxs
       , getTxInfo
       , groupTxsInfo
       ) where

import           Control.Lens                   (at, non, (.=), _Just)
import           Control.Monad.State            (MonadState)
import           Data.Aeson                     (ToJSON)
import           Data.List                      (intersperse)
import qualified Data.Map                       as M
import qualified Data.Set                       as S
import           Data.Text.Buildable            (build)
import           Formatting                     (sformat, shown, stext, (%))
import qualified Formatting                     as F
import           Network.EngineIO               (SocketId)
import           Network.SocketIO               (Socket, socketId)
import qualified Pos.Block.Logic                as DB
import           Pos.Crypto                     (withHash)
import           Pos.Crypto                     (hash)
import           Pos.DB                         (MonadDB)
import qualified Pos.DB.Block                   as DB
import qualified Pos.DB.GState                  as DB
import           Pos.Slotting.Class             (MonadSlots)
import           Pos.Ssc.Class                  (SscHelpersClass)
import           Pos.Txp                        (Tx (..), TxOut (..), TxOutAux (..),
                                                 txOutAddress)
import           Pos.Types                      (Address, Block, HeaderHash, TxExtra (..),
                                                 blockTxs)
import           Pos.Util                       (maybeThrow)
import           System.Wlog                    (WithLogger, logDebug, logError,
                                                 logWarning)
import           Universum

import           Pos.Explorer.Aeson.ClientTypes ()
import           Pos.Explorer.Socket.Holder     (ConnectionsState, ccAddress, ccBlock,
                                                 ccConnection, csAddressSubscribers,
                                                 csBlocksSubscribers, csClients,
                                                 csTxsSubscribers, mkClientContext)
import           Pos.Explorer.Socket.Util       (EventName (..), emitTo)
import           Pos.Explorer.Web.ClientTypes   (CAddress, CTxEntry (..), TxInternal (..),
                                                 fromCAddress, tiToTxEntry, toBlockEntry)
import           Pos.Explorer.Web.Error         (ExplorerError (..))
import           Pos.Explorer.Web.Server        (topsortTxsOrFail)

-- * Event names

data Subscription
    = SubAddr
    | SubBlock
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
    | TxsUpdated
    -- TODO: test events
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

startSession
    :: (MonadState ConnectionsState m, WithLogger m)
    => Socket -> m ()
startSession conn = do
    let cc = mkClientContext conn
        id = socketId conn
    csClients . at id .= Just cc
    logDebug $ sformat ("New session has started (#"%shown%")") id

finishSession
    :: (MonadState ConnectionsState m, WithLogger m)
    => SocketId -> m ()
finishSession sessId =
    whenJustM (use $ csClients . at sessId) $ \_ -> do
        csClients . at sessId .= Nothing
        unsubscribeFully sessId
        logDebug $ sformat ("Session #"%shown%" has finished") sessId

subscribe
    :: (MonadState ConnectionsState m, WithLogger m)
    => SocketId -> Text -> Lens' ConnectionsState (Maybe ()) -> m ()
subscribe sessId desc stateZoom = do
    session <- use $ csClients . at sessId
    case session of
        Just _  -> do
            stateZoom .= Just ()
            logDebug $ sformat ("Client #"%shown%" subscribed to "%stext%" \
                       \updates") sessId desc
        _       ->
            logWarning $ sformat ("Unregistered client tries to subscribe on "%
                         stext%" updates") desc

unsubscribe
    :: (MonadState ConnectionsState m, WithLogger m)
    => SocketId -> Text -> Lens' ConnectionsState (Maybe ()) -> m ()
unsubscribe sessId desc stateZoom = do
    stateZoom .= Nothing
    logDebug $ sformat ("Client #"%shown%" unsubscribed from "%stext%" \
               \updates") sessId desc

-- | Unsubscribes on any previous address and subscribes on given one.
subscribeAddr
    :: (MonadState ConnectionsState m, WithLogger m, MonadThrow m)
    => CAddress -> SocketId -> m ()
subscribeAddr caddr sessId = do
    addr <- fromCAddressOrThrow caddr
    unsubscribeAddr sessId
    csClients . at sessId . _Just . ccAddress .= Just addr
    subscribe
        sessId
        (sformat ("address "%shown) addr)
        (csAddressSubscribers . at addr . non S.empty . at sessId)

unsubscribeAddr
    :: (MonadState ConnectionsState m, WithLogger m)
    => SocketId -> m ()
unsubscribeAddr sessId = do
    maddr <- preuse $ csClients . at sessId . _Just . ccAddress . _Just
    csClients . at sessId . _Just . ccAddress .= Nothing
    whenJust maddr $ \addr ->
        unsubscribe
            sessId
            (sformat ("address "%shown) addr)
            (csAddressSubscribers . at addr . non S.empty . at sessId)

subscribeBlocks
    :: (MonadState ConnectionsState m, WithLogger m)
    => SocketId -> m ()
subscribeBlocks sessId = do
    -- TODO: set ChainDifficulty:
    -- csClients . at sessId . _Just . ccBlock .= Just pId
    subscribe sessId "blockchain" (csBlocksSubscribers . at sessId)

unsubscribeBlocks
    :: (MonadState ConnectionsState m, WithLogger m)
    => SocketId -> m ()
unsubscribeBlocks sessId = do
    csClients . at sessId . _Just . ccBlock .= Nothing
    unsubscribe sessId "blockchain" (csBlocksSubscribers . at sessId)

subscribeTxs
    :: (MonadState ConnectionsState m, WithLogger m)
    => SocketId -> m ()
subscribeTxs sessId = subscribe sessId "txs" (csTxsSubscribers . at sessId)

unsubscribeTxs
    :: (MonadState ConnectionsState m, WithLogger m)
    => SocketId -> m ()
unsubscribeTxs sessId = unsubscribe sessId "txs" (csTxsSubscribers . at sessId)

unsubscribeFully
    :: (MonadState ConnectionsState m, WithLogger m)
    => SocketId -> m ()
unsubscribeFully sessId = do
    unsubscribeAddr sessId
    unsubscribeBlocks sessId
    unsubscribeTxs sessId

-- * Notifications

broadcast
    :: (MonadIO m, MonadReader ConnectionsState m, WithLogger m, MonadCatch m,
        EventName event, ToJSON args)
    => event -> args -> Set SocketId -> m ()
broadcast event args recipients = do
    forM_ recipients $ \sockid -> do
        mSock <- preview $ csClients . at sockid . _Just . ccConnection
        case mSock of
            Nothing   -> logError $
                sformat ("No socket with SocketId="%shown%" registered") sockid
            Just sock -> emitTo sock event args
                `catchAll` handler sockid
  where
    handler sockid = logWarning .
        sformat ("Failed to send to SocketId="%shown%": "%shown) sockid

notifyAddrSubscribers
    :: (MonadIO m, MonadReader ConnectionsState m, WithLogger m, MonadCatch m)
    => Address -> [CTxEntry] -> m ()
notifyAddrSubscribers addr cTxEntries = do
    mRecipients <- view $ csAddressSubscribers . at addr
    whenJust mRecipients $ broadcast AddrUpdated cTxEntries

notifyBlocksSubscribers
    :: (MonadIO m, MonadReader ConnectionsState m, WithLogger m, MonadCatch m,
        MonadSlots m, SscHelpersClass ssc)
    => [Block ssc] -> m ()
notifyBlocksSubscribers blocks = do
    recipients <- view csBlocksSubscribers
    cblocks    <- catMaybes <$> forM blocks toClientType
    broadcast BlocksUpdated cblocks recipients
  where
    toClientType (Left _)          = return Nothing
    toClientType (Right mainBlock) = Just <$> toBlockEntry mainBlock

notifyTxsSubscribers
    :: (MonadIO m, MonadReader ConnectionsState m, WithLogger m, MonadCatch m,
        MonadDB m)
    => [CTxEntry] -> m ()
notifyTxsSubscribers cTxEntries = do
    recipients <- view csTxsSubscribers
    broadcast TxsUpdated cTxEntries recipients
    logDebug $ sformat ("Broadcasted transactions: "%F.build)
               (mconcat . intersperse "," $ build . cteId <$> cTxEntries)

-- * Helpers

getBlocksFromTo
    :: forall ssc m.
       (MonadDB m, SscHelpersClass ssc)
    => HeaderHash -> HeaderHash -> m (Maybe [Block ssc])
getBlocksFromTo recentBlock oldBlock = do
    mheaders <- DB.getHeadersFromToIncl @ssc oldBlock recentBlock
    forM mheaders $ fmap catMaybes . mapM DB.getBlock . toList

addrsTouchedByTx
    :: (MonadDB m, WithLogger m)
    => Tx -> m (S.Set Address)
addrsTouchedByTx tx = do
    -- for each transaction, get its OutTx
    -- and transactions from InTx
    inTxs <- forM (_txInputs tx) $ DB.getTxOut >=> \case
        -- TODO [CSM-153]: lookup mempool as well
        Nothing    -> mempty <$ logError "Can't find input of transaction!"
        Just txOut -> return . one $ toaOut txOut

    let relatedTxs = toList (_txOutputs tx) <> concat (toList inTxs)
    return . S.fromList $ txOutAddress <$> relatedTxs

getBlockTxs
    :: (MonadDB m, WithLogger m)
    => Block ssc -> m [TxInternal]
getBlockTxs (Left  _  ) = return []
getBlockTxs (Right blk) = do
    txs <- topsortTxsOrFail withHash $ toList $ blk ^. blockTxs
    forM txs $ \tx -> do
        TxExtra {..} <- DB.getTxExtra (hash tx) >>=
            maybeThrow (Internal "In-block transaction doesn't \
                                 \have extra info in DB")
        pure $ TxInternal teReceivedTime tx

getTxInfo
    :: (MonadDB m, WithLogger m)
    => TxInternal -> m (CTxEntry, S.Set Address)
getTxInfo tx = do
    let cTxEntry = tiToTxEntry tx
    addrs <- addrsTouchedByTx (tiTx tx)
    return (cTxEntry, addrs)

groupTxsInfo :: [(CTxEntry, S.Set Address)] -> M.Map Address [CTxEntry]
groupTxsInfo info =
    let entries = fmap swap $ concat $ fmap sequence
                $ toList <<$>> info :: [(Address, CTxEntry)]
    in  fmap ($ []) $ M.fromListWith (.) $ fmap (second $ (++) . pure) entries
