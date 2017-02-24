{-# LANGUAGE TemplateHaskell #-}

-- | Logic of Explorer socket-io Server.

module Pos.Explorer.Web.Sockets.Methods
       ( ClientEvent (..)
       , ServerEvent (..)

       , startSession
       , finishSession
       , setClientAddress
       , setClientBlock
       , subscribeAddr
       , subscribeBlocks
       , unsubscribeAddr
       , unsubscribeBlocks
       , unsubscribeFully

       , notifyAddrSubscribers
       , notifyAllAddrSubscribers
       , notifyBlocksSubscribers
       , getBlocksFromTo
       , blockAddresses
       ) where

import           Control.Lens                    (at, (%=), (.=), _Just)
import           Control.Monad                   (join)
import           Control.Monad.State             (MonadState)
import qualified Data.Set                        as S
import           Formatting                      (build, sformat, shown, (%))
import           GHC.Exts                        (toList)
import           Network.EngineIO                (SocketId)
import           Network.SocketIO                (Socket, socketId)
import           Pos.DB                          (MonadDB)
import qualified Pos.DB                          as DB
import qualified Pos.DB.GState                   as DB
import           Pos.Ssc.Class                   (SscHelpersClass)
import           Pos.Types                       (Address, Block, ChainDifficulty,
                                                  HeaderHash, Tx (..), blockTxas,
                                                  prevBlockL, txOutAddress)
import           System.Wlog                     (WithLogger, logError, logWarning)
import           Universum                       hiding (toList)

import           Pos.Explorer.Web.Sockets.Holder (ConnectionsState, ccAddress, ccBlock,
                                                  ccConnection, csAddressSubscribers,
                                                  csBlocksSubscribers, csClients,
                                                  mkClientContext)
import           Pos.Explorer.Web.Sockets.Util   (EventName (..), emitJSONTo)

-- * Event names

data ClientEvent
    = StartSession
    | SubscribeAddr
    | SubscribeBlock
    | UnsubscribeAddr
    | UnsubscribeBlock
    | SetClientAddress
    | SetClientBlock

instance EventName ClientEvent where
    toName StartSession     = "S"
    toName SubscribeAddr    = "SA"
    toName SubscribeBlock   = "SB"
    toName UnsubscribeAddr  = "UA"
    toName UnsubscribeBlock = "UB"
    toName SetClientAddress = "CA"
    toName SetClientBlock   = "CB"

data ServerEvent
    = AddrUpdated
    | BlocksUpdated

instance EventName ServerEvent where
    toName AddrUpdated   = "A"
    toName BlocksUpdated = "B"

-- * Client requests provessing

startSession
    :: MonadState ConnectionsState m
    => Socket -> m ()
startSession conn = do
    let cc = mkClientContext conn
        id = socketId conn
    csClients . at id .= Just cc

finishSession :: MonadState ConnectionsState m => SocketId -> m ()
finishSession i = whenJustM (use $ csClients . at i) finishSessionDo
  where
    finishSessionDo _ = do
        csClients . at i .= Nothing
        unsubscribeBlocks i
        unsubscribeAddr i

setClientAddress
    :: (MonadState ConnectionsState m, WithLogger m)
    => SocketId -> Maybe Address -> m ()
setClientAddress sessId addr = do
    unsubscribeAddr sessId
    csClients . at sessId . _Just . ccAddress .= addr
    whenJust addr $ subscribeAddr sessId

setClientBlock
    :: (MonadState ConnectionsState m, WithLogger m)
    => SocketId -> Maybe ChainDifficulty -> m ()
setClientBlock sessId pId = do
    csClients . at sessId . _Just . ccBlock .= pId
    subscribeBlocks sessId

subscribeAddr
    :: (MonadState ConnectionsState m, WithLogger m)
    => SocketId -> Address -> m ()
subscribeAddr i addr = do
    session <- use $ csClients . at i
    case session of
        Just _ -> csAddressSubscribers . at addr %=
            Just . (maybe (S.singleton i) (S.insert i))
        _      -> logWarning $
            sformat ("Unregistered client tries to subscribe on address \
            \updates"%build) addr

unsubscribeAddr
    :: MonadState ConnectionsState m
    => SocketId -> m ()
unsubscribeAddr i = do
    addr <- preuse $ csClients . at i . _Just . ccAddress
    whenJust (join addr) unsubscribeDo
  where
    unsubscribeDo a = csAddressSubscribers . at a %= fmap (S.delete i)

subscribeBlocks
    :: (MonadState ConnectionsState m, WithLogger m)
    => SocketId -> m ()
subscribeBlocks i = do
    session <- use $ csClients . at i
    case session of
        Just _  -> csBlocksSubscribers %= S.insert i
        _       -> logWarning "Unregistered client tries to subscribe on block\
                   \ updates"

unsubscribeBlocks
    :: MonadState ConnectionsState m
    => SocketId -> m ()
unsubscribeBlocks i = csBlocksSubscribers %= S.delete i

unsubscribeFully
    :: MonadState ConnectionsState m
    => SocketId -> m ()
unsubscribeFully i = unsubscribeBlocks i >> unsubscribeAddr i

-- * Notifications

broadcastTo
    :: (MonadIO m, MonadReader ConnectionsState m, WithLogger m, MonadCatch m)
    => Set SocketId -> m ()
broadcastTo recipients = do
    forM_ recipients $ \sockid -> do
        mSock <- preview $ csClients . at sockid . _Just . ccConnection
        case mSock of
            Nothing   -> logError $
                sformat ("No socket with SocketId="%shown%" registered") sockid
            Just sock -> emitJSONTo sock BlocksUpdated empty
                `catchAll` handler sockid
  where
    handler sockid = logWarning .
        sformat ("Failed to send to SocketId="%shown%": "%shown) sockid

notifyAddrSubscribers
    :: (MonadIO m, MonadReader ConnectionsState m, WithLogger m, MonadCatch m)
    => Address -> m ()
notifyAddrSubscribers addr = do
    mRecipients <- view $ csAddressSubscribers . at addr
    whenJust mRecipients broadcastTo

notifyAllAddrSubscribers
    :: (MonadIO m, MonadReader ConnectionsState m, WithLogger m, MonadCatch m)
    => m ()
notifyAllAddrSubscribers = do
    addrSubscribers <- view csAddressSubscribers
    mapM_ notifyAddrSubscribers $ map fst $ toList addrSubscribers

notifyBlocksSubscribers
    :: (MonadIO m, MonadReader ConnectionsState m, WithLogger m, MonadCatch m)
    => m ()
notifyBlocksSubscribers =
    view csBlocksSubscribers >>= broadcastTo

getBlocksFromTo
    :: (MonadDB ssc m, SscHelpersClass ssc)
    => HeaderHash -> HeaderHash -> Int -> m (Maybe [Block ssc])
getBlocksFromTo recentBlock oldBlock limit
    | recentBlock == oldBlock = return $ Just []
    | limit == 0              = return Nothing
    | otherwise               = do
        mBlock <- DB.getBlock recentBlock
        case mBlock of
            Nothing    -> return $ Just []
            Just block ->
                fmap (block :) <$> getBlocksFromTo
                    (block ^. prevBlockL) oldBlock (limit - 1)

blockAddresses
    :: (MonadDB ssc m, WithLogger m)
    => Block ssc -> m [Address]
blockAddresses block = do
    relatedTxs <- case block of
        Left _          -> return S.empty
        Right mainBlock -> fmap mconcat $
            forM (mainBlock ^. blockTxas) $ \(tx, _, _) -> do
                -- for each transaction, get its OutTx
                -- and transactions from InTx
                inTxs <- forM (txInputs tx) $ DB.getTxOut >=> \case
                    Nothing       -> S.empty <$ logError "DB is malformed!"
                    Just (tx', _) -> return $ one tx'

                return $ S.fromList (txOutputs tx) <> mconcat inTxs

    return $ txOutAddress <$> S.toList relatedTxs
