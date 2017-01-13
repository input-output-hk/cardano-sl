{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Wallet web server.

module Pos.Wallet.Web.Server.Methods
       ( walletApplication
       , walletServer
       , walletServeImpl
       ) where

import           Data.Default                  (def)
import           Data.List                     (elemIndex, (!!))
import           Data.Time.Clock.POSIX         (getPOSIXTime)
import           Formatting                    (build, ords, sformat, stext, (%))
import           Network.Wai                   (Application)
import           Node                          (SendActions, hoistSendActions)
import           Pos.Crypto                    (hash)
import           Servant.API                   ((:<|>) ((:<|>)),
                                                FromHttpApiData (parseUrlPiece))
import           Servant.Server                (Handler, Server, ServerT, serve)
import           Servant.Utils.Enter           ((:~>) (..), enter)
import           System.Wlog                   (logInfo)
import           Universum

import           Pos.Aeson.ClientTypes         ()
import           Pos.Crypto                    (toPublic)
import           Pos.DHT.Model                 (dhtAddr, getKnownPeers)
import           Pos.Types                     (Address, Coin, Tx, TxId, TxOut (..),
                                                addressF, coinF, decodeTextAddress,
                                                makePubKeyAddress, mkCoin)
import           Pos.Web.Server                (serveImpl)

import           Control.Monad.Catch           (try)
import           Pos.Communication.BiP         (BiP)
import           Pos.Wallet.KeyStorage         (KeyError (..), MonadKeys (..),
                                                newSecretKey)
import           Pos.Wallet.Tx                 (submitTx)
import           Pos.Wallet.WalletMode         (WalletMode, getBalance, getTxHistory)
import           Pos.Wallet.Web.Api            (WalletApi, walletApi)
import           Pos.Wallet.Web.ClientTypes    (CAddress, CCurrency (ADA), CTx, CTxId,
                                                CTxMeta (..), CWallet (..),
                                                CWalletMeta (..), addressToCAddress,
                                                cAddressToAddress, mkCTx, mkCTxId,
                                                txContainsTitle, txIdToCTxId)
import           Pos.Wallet.Web.Error          (WalletError (..))
import           Pos.Wallet.Web.Server.Sockets (MonadWalletWebSockets (..),
                                                WalletWebSockets, closeWSConnection,
                                                initWSConnection, runWalletWS,
                                                upgradeApplicationWS)
import           Pos.Wallet.Web.State          (MonadWalletWebDB (..), WalletWebDB,
                                                addOnlyNewTxMeta, closeState,
                                                createWallet, getTxMeta, getWalletMeta,
                                                openState, removeWallet, runWalletWebDB,
                                                setWalletMeta, setWalletTransactionMeta)

----------------------------------------------------------------------------
-- Top level functionality
----------------------------------------------------------------------------

type WalletWebHandler m = WalletWebSockets (WalletWebDB m)

walletServeImpl
    :: (MonadIO m, MonadMask m)
    => WalletWebHandler m Application     -- ^ Application getter
    -> FilePath                        -- ^ Path to wallet acid-state
    -> Bool                            -- ^ Rebuild flag for acid-state
    -> Word16                          -- ^ Port to listen
    -> m ()
walletServeImpl app daedalusDbPath dbRebuild port = bracket ((,) <$> openDB <*> initWS) (\(db, conn) -> closeDB db >> closeWS conn) $ \(db, conn) ->
    serveImpl (runWalletWebDB db $ runWalletWS conn app) port
  where openDB = openState dbRebuild daedalusDbPath
        closeDB = closeState
        initWS = initWSConnection
        closeWS = closeWSConnection

walletApplication
    :: WalletMode ssc m
    => WalletWebHandler m (Server WalletApi)
    -> WalletWebHandler m Application
walletApplication serv = do
    wsConn <- getWalletWebSockets
    serv >>= return . upgradeApplicationWS wsConn . serve walletApi

walletServer
    :: WalletMode ssc m
    => SendActions BiP m
    -> WalletWebHandler m (WalletWebHandler m :~> Handler)
    -> WalletWebHandler m (Server WalletApi)
walletServer sendActions nat = do
    join $ mapM_ insertAddressMeta <$> myCAddresses
    (`enter` servantHandlers sendActions') <$> nat
  where
    insertAddressMeta cAddr =
        getWalletMeta cAddr >>= createWallet cAddr . fromMaybe def

----------------------------------------------------------------------------
-- Handlers
----------------------------------------------------------------------------

type WalletWebMode ssc m
    = ( WalletMode ssc m
      , MonadWalletWebDB m
      , MonadWalletWebSockets m
      )

servantHandlers :: WalletWebMode ssc m => SendActions BiP m -> ServerT WalletApi m
servantHandlers sendActions =
     (catchWalletError . getWallet)
    :<|>
     catchWalletError getWallets
    :<|>
     (\a b -> catchWalletError . send sendActions a b)
    :<|>
     (\a b c d e -> catchWalletError . sendExtended a b c d e)
    :<|>
     catchWalletError . getHistory
    :<|>
     (\a b -> catchWalletError . searchHistory a b)
    :<|>
     (\a b -> catchWalletError . updateTransaction a b)
    :<|>
     catchWalletError . newWallet
    :<|>
     (\a -> catchWalletError . updateWallet a)
    :<|>
     catchWalletError . deleteWallet
    :<|>
     (\a -> catchWalletError . isValidAddress a)
  where
    -- TODO: can we with Traversable map catchWalletError over :<|>
    -- TODO: add logging on error
    catchWalletError = try

-- getAddresses :: WalletWebMode ssc m => m [CAddress]
-- getAddresses = map addressToCAddress <$> myAddresses

-- getBalances :: WalletWebMode ssc m => m [(CAddress, Coin)]
-- getBalances = join $ mapM gb <$> myAddresses
--   where gb addr = (,) (addressToCAddress addr) <$> getBalance addr

getWallet :: WalletWebMode ssc m => CAddress -> m CWallet
getWallet cAddr = do
    balance <- getBalance =<< decodeCAddressOrFail cAddr
    meta <- getWalletMeta cAddr >>= maybe noWallet pure
    pure $ CWallet cAddr balance meta
  where
    noWallet = throwM . Internal $
        sformat ("No wallet with address "%build%" is found") cAddr

-- TODO: probably poor naming
decodeCAddressOrFail :: WalletWebMode ssc m => CAddress -> m Address
decodeCAddressOrFail = either wrongAddress pure . cAddressToAddress
  where wrongAddress err = throwM . Internal $
            sformat ("Error while decoding CAddress: "%stext) err

getWallets :: WalletWebMode ssc m => m [CWallet]
getWallets = join $ mapM getWallet <$> myCAddresses

send :: WalletWebMode ssc m => SendActions BiP m -> CAddress -> CAddress -> Coin -> m CTx
send sendActions srcCAddr dstCAddr c =
    sendExtended sendActions srcCAddr dstCAddr c ADA mempty mempty

sendExtended :: WalletWebMode ssc m => SendActions BiP m -> CAddress -> CAddress -> Coin -> CCurrency -> Text -> Text -> m CTx
sendExtended sendActions srcCAddr dstCAddr c curr title desc = do
    srcAddr <- decodeCAddressOrFail srcCAddr
    dstAddr <- decodeCAddressOrFail dstCAddr
    idx <- getAddrIdx srcAddr
    sks <- getSecretKeys
    let sk = sks !! idx
    na <- fmap dhtAddr <$> getKnownPeers
    etx <- submitTx sendActions sk na [(TxOut dstAddr c, [])]
    case etx of
        Left err -> throwM . Internal $ sformat ("Cannot send transaction: "%stext) err
        Right (tx, _, _) -> do
            logInfo $
                sformat ("Successfully sent "%coinF%" from "%ords%" address to "%addressF)
                c idx dstAddr
            -- TODO: this should be removed in production
            let txHash = hash tx
            () <$ addHistoryTx dstCAddr curr title desc (txHash, tx, False)
            addHistoryTx srcCAddr curr title desc (txHash, tx, True)

getHistory :: WalletWebMode ssc m => CAddress -> m [CTx]
getHistory cAddr = do
    history <- getTxHistory =<< decodeCAddressOrFail cAddr
    mapM (addHistoryTx cAddr ADA mempty mempty) history

searchHistory :: WalletWebMode ssc m => CAddress -> Text -> Word -> m [CTx]
searchHistory cAddr search limit = take (fromIntegral limit) . filter (txContainsTitle search) <$> getHistory cAddr

addHistoryTx :: WalletWebMode ssc m => CAddress -> CCurrency -> Text -> Text -> (TxId, Tx, Bool) -> m CTx
addHistoryTx cAddr curr title desc wtx@(txId, _, _) = do
    -- TODO: this should be removed in production
    addr <- decodeCAddressOrFail cAddr
    meta <- CTxMeta curr title desc <$> liftIO getPOSIXTime
    let cId = txIdToCTxId txId
    addOnlyNewTxMeta cAddr cId meta
    meta' <- maybe meta identity <$> getTxMeta cAddr cId
    return $ mkCTx addr wtx meta'

newWallet :: WalletWebMode ssc m => CWalletMeta -> m CWallet
newWallet wMeta = do
    cAddr <- newAddress
    createWallet cAddr wMeta
    getWallet cAddr
  where
    newAddress = addressToCAddress . makePubKeyAddress . toPublic <$> newSecretKey

updateWallet :: WalletWebMode ssc m => CAddress -> CWalletMeta -> m CWallet
updateWallet cAddr wMeta = do
    setWalletMeta cAddr wMeta
    getWallet cAddr

updateTransaction :: WalletWebMode ssc m => CAddress -> CTxId -> CTxMeta -> m ()
updateTransaction = setWalletTransactionMeta

deleteWallet :: WalletWebMode ssc m => CAddress -> m ()
deleteWallet cAddr = do
    deleteAddress =<< decodeCAddressOrFail cAddr
    removeWallet cAddr
  where
    deleteAddress addr = do
        idx <- getAddrIdx addr
        deleteSecretKey (fromIntegral idx) `catch` deleteErrHandler
    deleteErrHandler (PrimaryKey err) = throwM . Internal $
        sformat ("Error while deleting wallet: "%stext) err

-- NOTE: later we will have `isValidAddress :: CCurrency -> CAddress -> m Bool` which should work for arbitrary crypto
isValidAddress :: WalletWebMode ssc m => CCurrency -> Text -> m Bool
isValidAddress ADA sAddr = pure . either (const False) (const True) $ decodeTextAddress sAddr
isValidAddress _ _       = pure False

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

myAddresses :: MonadKeys m => m [Address]
myAddresses = map (makePubKeyAddress . toPublic) <$> getSecretKeys

myCAddresses :: MonadKeys m => m [CAddress]
myCAddresses = map addressToCAddress <$> myAddresses

getAddrIdx :: WalletWebMode ssc m => Address -> m Int
getAddrIdx addr = elemIndex addr <$> myAddresses >>= maybe notFound pure
  where notFound = throwM . Internal $
            sformat ("Address "%addressF%" is not found in wallet") $ addr

----------------------------------------------------------------------------
-- Orphan instances
----------------------------------------------------------------------------

instance FromHttpApiData Coin where
    parseUrlPiece = fmap mkCoin . parseUrlPiece

instance FromHttpApiData Address where
    parseUrlPiece = decodeTextAddress

instance FromHttpApiData CAddress where
    parseUrlPiece = fmap addressToCAddress . decodeTextAddress

-- FIXME: unsafe (temporary, will be removed probably in future)
-- we are not checking is receaved Text really vald CTxId
instance FromHttpApiData CTxId where
    parseUrlPiece = pure . mkCTxId

instance FromHttpApiData CCurrency where
    parseUrlPiece = first fromString . readEither . toString
