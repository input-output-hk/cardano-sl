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
       , setClientAddress
       , setClientBlock
       , subscribeAddr
       , subscribeBlocks
       , subscribeTxs
       , unsubscribeAddr
       , unsubscribeBlocks
       , unsubscribeTxs
       , unsubscribeFully

       , notifyAddrSubscribers
       , notifyAllAddrSubscribers
       , notifyBlocksSubscribers
       , notifyTxsSubscribers
       , getBlocksFromTo
       , blockAddresses
       , getBlockTxs
       ) where

import           Control.Lens                   (at, non, (.=), _Just)
import           Control.Monad.State            (MonadState)
import           Data.Aeson                     (ToJSON)
import qualified Data.Set                       as S
import           Formatting                     (sformat, shown, stext, (%))
import qualified GHC.Exts                       as Exts
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
import           Pos.Types                      (Address, Block, ChainDifficulty,
                                                 HeaderHash, TxExtra (..), blockTxas,
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
import           Pos.Explorer.Web.ClientTypes   (CAddress, CTxEntry, fromCAddress,
                                                 toBlockEntry, toTxEntry)
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
    | SetClientAddress
    | SetClientBlock
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
finishSession i = whenJustM (use $ csClients . at i) finishSessionDo
  where
    finishSessionDo _ = do
        csClients . at i .= Nothing
        unsubscribeBlocks i
        unsubscribeAddr i
        logDebug $ sformat ("Session #"%shown%" has finished") i

setClientAddress
    :: (MonadState ConnectionsState m, WithLogger m, MonadThrow m)
    => Maybe CAddress -> SocketId -> m ()
setClientAddress caddr sessId = do
    addr <- mapM fromCAddressOrThrow caddr
    unsubscribeAddr sessId
    csClients . at sessId . _Just . ccAddress .= addr
    whenJust caddr $ flip subscribeAddr sessId

setClientBlock
    :: (MonadState ConnectionsState m, WithLogger m)
    => Maybe ChainDifficulty -> SocketId -> m ()
setClientBlock pId sessId = do
    csClients . at sessId . _Just . ccBlock .= pId
    subscribeBlocks sessId

subscribe
    :: (MonadState ConnectionsState m, WithLogger m)
    => SocketId -> Text -> Lens' ConnectionsState (Maybe ()) -> m ()
subscribe i desc stateZoom = do
    session <- use $ csClients . at i
    case session of
        Just _  -> do
            stateZoom .= Just ()
            logDebug $ sformat ("Client #"%shown%" subscribed to "%stext%" \
                       \updates") i desc
        _       ->
            logWarning $ sformat ("Unregistered client tries to subscribe on "%
                         stext%" updates") desc

unsubscribe
    :: (MonadState ConnectionsState m, WithLogger m)
    => SocketId -> Text -> Lens' ConnectionsState (Maybe ()) -> m ()
unsubscribe i desc stateZoom = do
    stateZoom .= Nothing
    logDebug $ sformat ("Client #"%shown%" unsubscribed from "%stext%" \
               \updates") i desc

subscribeAddr
    :: (MonadState ConnectionsState m, WithLogger m, MonadThrow m)
    => CAddress -> SocketId -> m ()
subscribeAddr caddr i = do
    addr <- fromCAddressOrThrow caddr
    subscribe
        i
        (sformat ("address "%shown) addr)
        (csAddressSubscribers . at addr . non S.empty . at i)

unsubscribeAddr
    :: (MonadState ConnectionsState m, WithLogger m)
    => SocketId -> m ()
unsubscribeAddr i = do
    maddr <- preuse $ csClients . at i . _Just . ccAddress . _Just
    whenJust maddr $ \addr ->
        unsubscribe
            i
            (sformat ("address "%shown) addr)
            (csAddressSubscribers . at addr . non S.empty . at i)

subscribeBlocks
    :: (MonadState ConnectionsState m, WithLogger m)
    => SocketId -> m ()
subscribeBlocks i = subscribe i "blockchain" (csBlocksSubscribers . at i)

unsubscribeBlocks
    :: (MonadState ConnectionsState m, WithLogger m)
    => SocketId -> m ()
unsubscribeBlocks i = unsubscribe i "blockchain" (csBlocksSubscribers . at i)

subscribeTxs
    :: (MonadState ConnectionsState m, WithLogger m)
    => SocketId -> m ()
subscribeTxs i = subscribe i "txs" (csTxsSubscribers . at i)

unsubscribeTxs
    :: (MonadState ConnectionsState m, WithLogger m)
    => SocketId -> m ()
unsubscribeTxs i = unsubscribe i "txs" (csTxsSubscribers . at i)

unsubscribeFully
    :: (MonadState ConnectionsState m, WithLogger m)
    => SocketId -> m ()
unsubscribeFully i = unsubscribeBlocks i >> unsubscribeAddr i

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
    => Address -> m ()
notifyAddrSubscribers addr = do
    mRecipients <- view $ csAddressSubscribers . at addr
    whenJust mRecipients $ broadcast AddrUpdated ()

notifyAllAddrSubscribers
    :: (MonadIO m, MonadReader ConnectionsState m, WithLogger m, MonadCatch m)
    => m ()
notifyAllAddrSubscribers = do
    addrSubscribers <- view csAddressSubscribers
    mapM_ notifyAddrSubscribers $ map fst $ Exts.toList addrSubscribers

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
    => [Block ssc] -> m ()
notifyTxsSubscribers blocks = do
    recipients <- view csTxsSubscribers
    cTxEntries <- forM blocks getBlockTxs
    broadcast TxsUpdated cTxEntries recipients

getBlocksFromTo
    :: forall ssc m.
       (MonadDB m, SscHelpersClass ssc)
    => HeaderHash -> HeaderHash -> m (Maybe [Block ssc])
getBlocksFromTo recentBlock oldBlock = do
    mheaders <- DB.getHeadersFromToIncl @ssc oldBlock recentBlock
    forM mheaders $ fmap catMaybes . mapM DB.getBlock . toList

blockAddresses
    :: (MonadDB m, WithLogger m)
    => Block ssc -> m [Address]
blockAddresses block = do
    relatedTxs <- case block of
        Left _          -> return S.empty
        Right mainBlock -> fmap mconcat $
            forM (mainBlock ^. blockTxas) $ \(tx, _, _) -> do
                -- for each transaction, get its OutTx
                -- and transactions from InTx
                inTxs <- forM (_txInputs tx) $ DB.getTxOut >=> \case
                    Nothing       -> S.empty <$ logError "DB is malformed!"
                    Just txOut -> return . one $ toaOut txOut

                return $ S.fromList (toList $ _txOutputs tx)
                      <> (mconcat $ toList inTxs)

    return $ txOutAddress <$> S.toList relatedTxs

getBlockTxs
    :: (MonadDB m, WithLogger m)
    => Block ssc -> m [CTxEntry]
getBlockTxs (Left  _  ) = return []
getBlockTxs (Right blk) = do
    txs <- topsortTxsOrFail withHash $ toList $ blk ^. blockTxs
    forM txs $ \tx -> do
        TxExtra {..} <- DB.getTxExtra (hash tx) >>=
            maybeThrow (Internal "In-block transaction doesn't \
                                 \have extra info in DB")
        pure $ toTxEntry teReceivedTime tx
