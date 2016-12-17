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

import qualified Control.Monad.Catch        as Catch
import           Control.Monad.Except       (MonadError (throwError))
import           Control.TimeWarp.Rpc       (Dialog, Transfer)
import           Data.List                  ((!!))
import           Formatting                 (int, ords, sformat, (%))
import           Network.Wai                (Application)
import           Servant.API                ((:<|>) ((:<|>)),
                                             FromHttpApiData (parseUrlPiece))
import           Servant.Server             (Handler, ServantErr (errBody), Server,
                                             ServerT, err404, serve)
import           Servant.Utils.Enter        ((:~>) (..), enter)
import           System.Wlog                (logInfo)
import           Universum

import           Pos.Crypto                 (SecretKey, toPublic)
import           Pos.DHT.Model              (DHTPacking, dhtAddr, getKnownPeers)
import           Pos.DHT.Real               (KademliaDHTContext, getKademliaDHTCtx,
                                             runKademliaDHTRaw)
import           Pos.Genesis                (genesisSecretKeys)
import           Pos.Launcher               (runTimed)
import           Pos.Types                  (Address, Coin (Coin), Tx, TxOut (..),
                                             addressF, coinF, decodeTextAddress,
                                             makePubKeyAddress)
import           Pos.Web.Server             (serveImpl)
import           Pos.WorkMode               (SocketState)

import           Pos.Wallet.Context         (ContextHolder, WalletContext,
                                             getWalletContext, runContextHolder)
import           Pos.Wallet.KeyStorage      (KeyData, KeyStorage, MonadKeys (..),
                                             newSecretKey, runKeyStorageRaw)
import           Pos.Wallet.State           (WalletDB, getWalletState, runWalletDB)
import qualified Pos.Wallet.State           as WS
import           Pos.Wallet.Tx              (getBalance, getTxHistory, submitTx)
import           Pos.Wallet.WalletMode      (WalletRealMode)
import           Pos.Wallet.Web.Api         (WalletApi, walletApi)
import           Pos.Wallet.Web.ClientTypes (CAddress, addressToCAddress)
import           Pos.Wallet.Web.State       (MonadWalletWebDB (..), WalletState,
                                             WalletWebDB, closeState, getWalletWebState,
                                             openState, runWalletWebDB)

----------------------------------------------------------------------------
-- Top level functionality
----------------------------------------------------------------------------

walletServeWeb :: FilePath -> Word16 -> WalletRealMode ()
walletServeWeb daedalusDbPath = serveImpl $ walletApplication daedalusDbPath

walletApplication :: FilePath -> WalletRealMode Application
walletApplication daedalusDbPath = bracket openDB closeDB $ \ws ->
    runWalletWebDB ws servantServer >>= return . serve walletApi
  where openDB = openState False daedalusDbPath
        closeDB = closeState

----------------------------------------------------------------------------
-- Servant infrastructure
----------------------------------------------------------------------------

type WebHandler = WalletWebDB WalletRealMode
type SubKademlia = KeyStorage
                   (WalletDB
                    (ContextHolder
                     (Dialog DHTPacking (Transfer SocketState))))

type MainWalletState = WS.WalletState

convertHandler
    :: KademliaDHTContext SubKademlia
    -> WalletContext
    -> MainWalletState
    -> KeyData
    -> WalletState
    -> WebHandler a
    -> Handler a
convertHandler kctx wc mws kd ws handler =
    liftIO (runTimed "wallet-api" .
            runContextHolder wc .
            runWalletDB mws .
            flip runKeyStorageRaw kd .
            runKademliaDHTRaw kctx .
            runWalletWebDB ws $
            handler)
    `Catch.catches`
    excHandlers
  where
    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError

nat :: WebHandler (WebHandler :~> Handler)
nat = do
    ws <- getWalletWebState
    kd <- (lift . lift) ask
    kctx <- lift getKademliaDHTCtx
    wc <- getWalletContext
    mws <- getWalletState
    return $ Nat (convertHandler kctx wc mws kd ws)

servantServer :: WebHandler (Server WalletApi)
servantServer = flip enter servantHandlers <$> nat

----------------------------------------------------------------------------
-- Handlers
----------------------------------------------------------------------------

servantHandlers :: ServerT WalletApi WebHandler
servantHandlers = getAddresses :<|> getBalances :<|> send :<|>
                  getHistory :<|> newAddress :<|> deleteAddress

getAddresses :: WebHandler [CAddress]
getAddresses = map fst <$> getBalances

getBalances :: WebHandler [(CAddress, Coin)]
getBalances = join $ mapM gb <$> addresses
  where gb addr = (,) (addressToCAddress addr) <$> getBalance addr
        addresses = map (makePubKeyAddress . toPublic) <$> mySecretKeys

send :: Word -> Address -> Coin -> WebHandler ()
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

getHistory :: Address -> WebHandler ([Tx], [Tx])
getHistory = getTxHistory

newAddress :: WebHandler CAddress
newAddress = addressToCAddress . makePubKeyAddress . toPublic <$> newSecretKey

deleteAddress :: Word -> WebHandler ()
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
