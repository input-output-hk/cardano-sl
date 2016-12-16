{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}

-- | Wallet web server.

module Pos.Wallet.Web.Server
       ( walletApplication
       , walletServeWeb
       ) where

import qualified Control.Monad.Catch             as Catch
import           Control.Monad.Except            (MonadError (throwError))
import           Control.TimeWarp.Rpc            (Dialog, Transfer)
import           Data.List                       ((!!))
import           Formatting                      (int, ords, sformat, (%))
import           Network.Wai                     (Application)
import           Servant.API                     ((:<|>) ((:<|>)),
                                                  FromHttpApiData (parseUrlPiece))
import           Servant.Server                  (Handler, ServantErr (errBody), Server,
                                                  ServerT, err404, serve)
import           Servant.Utils.Enter             ((:~>) (..), enter)
import           System.Wlog                     (logInfo)
import           Universum

import           Pos.Crypto                      (SecretKey, toPublic)
import           Pos.DHT.Model                   (DHTPacking, dhtAddr, getKnownPeers)
import           Pos.DHT.Real                    (KademliaDHTContext, getKademliaDHTCtx,
                                                  runKademliaDHTRaw)
import           Pos.Genesis                     (genesisSecretKeys)
import           Pos.Launcher                    (runTimed)
#ifdef WITH_ROCKS
import qualified Pos.Modern.DB                   as Modern
import qualified Pos.Modern.Txp.Holder           as Modern
import qualified Pos.Modern.Txp.Storage.UtxoView as Modern
#endif
import           Pos.Context                     (ContextHolder, NodeContext,
                                                  getNodeContext, runContextHolder)
import           Pos.Ssc.Class                   (SscConstraint)
import           Pos.Ssc.LocalData               (SscLDImpl, runSscLDImpl)
import qualified Pos.State                       as St
import           Pos.Txp.LocalData               (TxLocalData, getTxLocalData,
                                                  setTxLocalData)
import           Pos.Types                       (Address, Coin (Coin), Tx, TxOut (..),
                                                  addressF, coinF, decodeTextAddress,
                                                  makePubKeyAddress)
import           Pos.Wallet.KeyStorage           (KeyData, MonadKeys (..), newSecretKey,
                                                  runKeyStorageRaw)
import           Pos.Wallet.Tx                   (getBalance, getTxHistory, submitTx)
import           Pos.Wallet.WalletMode           (WalletRealMode)
import           Pos.Wallet.Web.Api              (WalletApi, walletApi)
import           Pos.Wallet.Web.State            (MonadWalletWebDB (..), WalletState,
                                                  WalletWebDB, closeState, openState,
                                                  runWalletWebDB)
import           Pos.Web.Server                  (serveImpl)
import           Pos.Wallet.Web.ClientTypes      (CWallet, addressToCAddress, CAddress)
import           Pos.WorkMode                    (SocketState, TxLDImpl, runTxLDImpl)

----------------------------------------------------------------------------
-- Top level functionality
----------------------------------------------------------------------------

walletServeWeb :: SscConstraint ssc => FilePath -> Word16 -> WalletRealMode ssc ()
walletServeWeb daedalusDbPath = serveImpl $ walletApplication daedalusDbPath

walletApplication :: SscConstraint ssc => FilePath -> WalletRealMode ssc Application
walletApplication daedalusDbPath = bracket openDB closeDB $ \ws ->
    runWalletWebDB ws servantServer >>= return . serve walletApi
  where openDB = openState False daedalusDbPath
        closeDB = closeState

----------------------------------------------------------------------------
-- Servant infrastructure
----------------------------------------------------------------------------

type WebHandler ssc = WalletWebDB (WalletRealMode ssc)
type SubKademlia ssc = (
#ifdef WITH_ROCKS
                   Modern.TxpLDHolder ssc (
#endif
                       TxLDImpl (
                           SscLDImpl ssc (
                               ContextHolder ssc (
#ifdef WITH_ROCKS
                                   Modern.DBHolder ssc (
#endif
                                       St.DBHolder ssc (Dialog DHTPacking
                                            (Transfer SocketState))))))
#ifdef WITH_ROCKS
                       ))
#endif

convertHandler
    :: forall ssc a . SscConstraint ssc
    => KademliaDHTContext (SubKademlia ssc)
    -> TxLocalData
    -> NodeContext ssc
    -> St.NodeState ssc
#ifdef WITH_ROCKS
    -> Modern.NodeDBs ssc
#endif
    -> KeyData
    -> WalletState
    -> WebHandler ssc a
    -> Handler a
#ifdef WITH_ROCKS
convertHandler kctx tld nc ns modernDB kd ws handler =
#else
convertHandler kctx tld nc ns kd ws handler =
#endif
    liftIO (runTimed "wallet-api" .
            St.runDBHolder ns .
#ifdef WITH_ROCKS
            Modern.runDBHolder modernDB .
#endif
            runContextHolder nc .
            runSscLDImpl .
            runTxLDImpl .
#ifdef WITH_ROCKS
            flip Modern.runTxpLDHolderUV (Modern.createFromDB . Modern._utxoDB $ modernDB) .
#endif
            runKademliaDHTRaw kctx .
            flip runKeyStorageRaw kd .
            runWalletWebDB ws $
            setTxLocalData tld >> handler)
    `Catch.catches`
    excHandlers
  where
    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError

nat :: SscConstraint ssc => WebHandler ssc (WebHandler ssc :~> Handler)
nat = do
    ws <- getWalletWebState
    kd <- lift ask
    kctx <- lift $ lift getKademliaDHTCtx
    tld <- getTxLocalData
    nc <- getNodeContext
    ns <- St.getNodeState
#ifdef WITH_ROCKS
    modernDB <- Modern.getNodeDBs
    return $ Nat (convertHandler kctx tld nc ns modernDB kd ws)
#else
    return $ Nat (convertHandler kctx tld nc ns kd ws)
#endif

servantServer :: forall ssc . SscConstraint ssc => WebHandler ssc (Server WalletApi)
servantServer = flip enter servantHandlers <$> (nat @ssc)

----------------------------------------------------------------------------
-- Handlers
----------------------------------------------------------------------------

servantHandlers :: SscConstraint ssc => ServerT WalletApi (WebHandler ssc)
servantHandlers = getAddresses :<|> getBalances :<|> send :<|>
                  getHistory :<|> newAddress :<|> deleteAddress

getAddresses :: WebHandler ssc [CAddress]
getAddresses = map fst <$> getBalances

getBalances :: SscConstraint ssc => WebHandler ssc [(CAddress, Coin)]
getBalances = join $ mapM gb <$> addresses
  where gb addr = (,) (addressToCAddress addr) <$> getBalance addr
        addresses = map (makePubKeyAddress . toPublic) <$> mySecretKeys

send :: SscConstraint ssc
     => Word -> Address -> Coin -> WebHandler ssc ()
send srcIdx dstAddr c = do
    sks <- mySecretKeys
    let skCount = length sks
    if fromIntegral srcIdx > skCount
    then throwM err404 {
        errBody = encodeUtf8 $
                  sformat ("There are only "%int%" addresses in wallet") $ skCount
        }
    else do
        let sk = sks !! fromIntegral srcIdx
        na <- fmap dhtAddr <$> getKnownPeers
        () <$ submitTx sk na [TxOut dstAddr c]
        logInfo $
              sformat ("Successfully sent "%coinF%" from "%ords%" address to "%addressF)
              c srcIdx dstAddr

getHistory :: SscConstraint ssc => Address -> WebHandler ssc ([Tx], [Tx])
getHistory = getTxHistory

newAddress :: WebHandler ssc CAddress
newAddress = addressToCAddress . makePubKeyAddress . toPublic <$> newSecretKey

deleteAddress :: Word -> WebHandler ssc ()
deleteAddress = deleteSecretKey

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- TODO: @flyingleafe provide a `debug` flag (in some `WalletContext`?)
-- to disable/enable inclusion of genesis keys
mySecretKeys :: MonadKeys m => m [SecretKey]
mySecretKeys = (genesisSecretKeys ++) <$> getSecretKeys

----------------------------------------------------------------------------
-- Orphan instances
----------------------------------------------------------------------------

deriving instance FromHttpApiData Coin

instance FromHttpApiData Address where
    parseUrlPiece = decodeTextAddress
