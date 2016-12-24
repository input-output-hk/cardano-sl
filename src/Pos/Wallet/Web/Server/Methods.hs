{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}

-- | Wallet web server.

module Pos.Wallet.Web.Server.Methods
       ( walletApplication
       , walletServer
       ) where

import           Data.List                  (elemIndex, (!!))
import           Formatting                 (ords, sformat, stext, (%))
import           Network.Wai                (Application)
import           Servant.API                ((:<|>) ((:<|>)),
                                             FromHttpApiData (parseUrlPiece), addHeader)
import           Servant.Server             (Handler, ServantErr (errBody), Server,
                                             ServerT, err404, serve)
import           Servant.Utils.Enter        ((:~>) (..), enter)
import           System.Wlog                (logInfo)
import           Universum

import           Pos.Crypto                 (toPublic, whData)
import           Pos.DHT.Model              (dhtAddr, getKnownPeers)
import           Pos.Types                  (Address, Coin (Coin), Tx, TxOut (..),
                                             addressF, coinF, decodeTextAddress,
                                             makePubKeyAddress)

import           Pos.Aeson.ClientTypes      ()
import           Pos.Wallet.KeyStorage      (MonadKeys (..), newSecretKey)
import           Pos.Wallet.Tx              (submitTx)
import           Pos.Wallet.WalletMode      (WalletMode, getBalance, getTxHistory)
import           Pos.Wallet.Web.Api         (Cors, WalletApi, walletApi)
import           Pos.Wallet.Web.ClientTypes (CAddress, CWallet (..), CWalletMeta (..),
                                             addressToCAddress, cAddressToAddress)
import           Pos.Wallet.Web.State       (MonadWalletWebDB (..), WalletWebDB,
                                             addWalletMeta, closeState, getWalletMeta,
                                             openState, removeWallet, runWalletWebDB)



----------------------------------------------------------------------------
-- Top level functionality
----------------------------------------------------------------------------

walletApplication
    :: WalletMode ssc m
    => WalletWebDB m (Server WalletApi)
    -> FilePath
    -> m Application
walletApplication server daedalusDbPath = bracket openDB closeDB $ \ws ->
    runWalletWebDB ws server >>= return . serve walletApi
  where openDB = openState True daedalusDbPath
        closeDB = closeState

walletServer
    :: WalletMode ssc m
    => WalletWebDB m (WalletWebDB m :~> Handler)
    -> WalletWebDB m (Server WalletApi)
walletServer nat = flip enter servantHandlers <$> nat

----------------------------------------------------------------------------
-- Handlers
----------------------------------------------------------------------------

type WalletWebMode ssc m
    = ( WalletMode ssc m
      , MonadWalletWebDB m
      )

servantHandlers :: WalletWebMode ssc m => ServerT WalletApi m
servantHandlers =
     addCors . getWallet
    :<|>
     addCors getWallets
    :<|>
     (\a b -> addCors . send a b)
    :<|>
     addCors . getHistory
    :<|>
     addCors . newWallet
    :<|>
     (\a -> addCors . updateWallet a)
    :<|>
     addCors . deleteWallet

getAddresses :: WalletWebMode ssc m => m [CAddress]
getAddresses = map addressToCAddress <$> myAddresses

getBalances :: WalletWebMode ssc m => m [(CAddress, Coin)]
getBalances = join $ mapM gb <$> myAddresses
  where gb addr = (,) (addressToCAddress addr) <$> getBalance addr

getWallet :: WalletWebMode ssc m => CAddress -> m CWallet
getWallet cAddr = do
    balance <- getBalance =<< decodeCAddressOrFail cAddr
    meta <- getWalletMeta cAddr >>= maybe noWallet pure
    pure $ CWallet cAddr balance meta
  where
    -- TODO: improve error handling
    noWallet = throwM err404

-- TODO: probably poor naming
decodeCAddressOrFail :: WalletWebMode ssc m => CAddress -> m Address
decodeCAddressOrFail = either wrongAddress pure . cAddressToAddress
  where
    wrongAddress err = throwM err404 {
        errBody = encodeUtf8 $
            sformat ("Error while decoding CAddress: "%stext) err
        }

getWallets :: WalletWebMode ssc m => m [CWallet]
getWallets = join $ mapM getWallet <$> myCAddresses

send :: WalletWebMode ssc m => CAddress -> CAddress -> Coin -> m ()
send srcCAddr dstCAddr c = do
    srcAddr <- decodeCAddressOrFail srcCAddr
    dstAddr <- decodeCAddressOrFail dstCAddr
    idx <- getAddrIdx srcAddr
    sks <- getSecretKeys
    let sk = sks !! idx
    na <- fmap dhtAddr <$> getKnownPeers
    () <$ submitTx sk na [TxOut dstAddr c]
    logInfo $
        sformat ("Successfully sent "%coinF%" from "%ords%" address to "%addressF)
        c idx dstAddr

getHistory :: WalletWebMode ssc m => CAddress -> m [Tx]
getHistory cAddr = map whData <$> (getTxHistory =<< decodeCAddressOrFail cAddr)

newWallet :: WalletWebMode ssc m => CWalletMeta -> m CWallet
newWallet wMeta = do
    cAddr <- newAddress
    addWalletMeta cAddr wMeta
    getWallet cAddr
  where
    newAddress = addressToCAddress . makePubKeyAddress . toPublic <$> newSecretKey

updateWallet :: WalletWebMode ssc m => CAddress -> CWalletMeta -> m CWallet
updateWallet cAddr wMeta = do
    addWalletMeta cAddr wMeta
    getWallet cAddr

deleteWallet :: WalletWebMode ssc m => CAddress -> m ()
deleteWallet cAddr = do
    removeWallet cAddr
    deleteAddress =<< decodeCAddressOrFail cAddr
  where
    deleteAddress addr = do
        idx <- getAddrIdx addr
        deleteSecretKey $ fromIntegral idx

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

myAddresses :: MonadKeys m => m [Address]
myAddresses = map (makePubKeyAddress . toPublic) <$> getSecretKeys

myCAddresses :: MonadKeys m => m [CAddress]
myCAddresses = map addressToCAddress <$> myAddresses

getAddrIdx :: WalletWebMode ssc m => Address -> m Int
getAddrIdx addr = elemIndex addr <$> myAddresses >>= maybe notFound return
  where notFound = throwM err404 {
            errBody = encodeUtf8 $
                sformat ("Address "%addressF%" is not found in wallet") $ addr
            }

addCors :: Monad m => m a -> m (Cors a)
addCors = fmap (addHeader "*")

----------------------------------------------------------------------------
-- Orphan instances
----------------------------------------------------------------------------

deriving instance FromHttpApiData Coin

instance FromHttpApiData Address where
    parseUrlPiece = decodeTextAddress

instance FromHttpApiData CAddress where
    parseUrlPiece = fmap addressToCAddress . decodeTextAddress
