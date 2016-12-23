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
import           Formatting                 (int, ords, sformat, (%))
import           Network.Wai                (Application)
import           Servant.API                ((:<|>) ((:<|>)),
                                             FromHttpApiData (parseUrlPiece), addHeader)
import           Servant.Server             (Handler, ServantErr (errBody), Server,
                                             ServerT, err404, serve)
import           Servant.Utils.Enter        ((:~>) (..), enter)
import           System.Wlog                (logInfo)
import           Universum

import           Pos.Crypto                 (SecretKey, toPublic, whData)
import           Pos.DHT.Model              (dhtAddr, getKnownPeers)
import           Pos.Genesis                (genesisSecretKeys)
import           Pos.Types                  (Address, Coin (Coin), Tx, TxOut (..),
                                             addressF, coinF, decodeTextAddress,
                                             makePubKeyAddress)

import           Pos.Aeson.ClientTypes      ()
import           Pos.Wallet.KeyStorage      (MonadKeys (..), newSecretKey)
import           Pos.Wallet.Tx              (submitTx)
import           Pos.Wallet.WalletMode      (WalletMode, getBalance, getTxHistory)
import           Pos.Wallet.Web.Api         (Cors, WalletApi, walletApi)
import           Pos.Wallet.Web.ClientTypes (CAddress, CWallet (..), addressToCAddress,
                                             cAddressToAddress)
import           Pos.Wallet.Web.State       (MonadWalletWebDB (..), WalletWebDB,
                                             closeState, getWalletMeta, openState,
                                             runWalletWebDB)


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
     addCors getAddresses
    :<|>
     addCors . getWallet
    :<|>
     addCors getBalances
    :<|>
     (\a b -> addCors . send a b)
    :<|>
     addCors . getHistory
    :<|>
     addCors newAddress
    :<|>
     addCors . deleteAddress
  where
    addCors :: Monad m => m a -> m (Cors a)
    addCors = fmap (addHeader "*")

getAddresses :: WalletWebMode ssc m => m [CAddress]
getAddresses = map fst <$> getBalances

getBalances :: WalletWebMode ssc m => m [(CAddress, Coin)]
getBalances = join $ mapM gb <$> myAddresses
  where gb addr = (,) (addressToCAddress addr) <$> getBalance addr

getWallet :: WalletWebMode ssc m => CAddress -> m CWallet
getWallet cAddr = do
    balance <- getBalance <$> cAddressToAddress cAddr >>= either wrongAddress pure
    meta <- getWalletMeta cAddr >>= maybe noWallet pure
    pure $ CWallet cAddr balance meta
  where
    -- TODO: improve error handling
    wrongAddress = throwM err404
    noWallet = throwM err404

send :: WalletWebMode ssc m => Address -> Address -> Coin -> m ()
send srcAddr dstAddr c = do
    idx <- getAddrIdx srcAddr
    sks <- getSecretKeys
    let sk = sks !! idx
    na <- fmap dhtAddr <$> getKnownPeers
    () <$ submitTx sk na [TxOut dstAddr c]
    logInfo $
        sformat ("Successfully sent "%coinF%" from "%ords%" address to "%addressF)
        c idx dstAddr

getHistory :: WalletWebMode ssc m => Address -> m [Tx]
getHistory addr = map whData <$> getTxHistory addr

newAddress :: WalletWebMode ssc m => m CAddress
newAddress = addressToCAddress . makePubKeyAddress . toPublic <$> newSecretKey

-- newWallet :: WalletWebMode ssc m => CWalletMeta -> m (Cors CWallet)
-- newWallet wMeta = addCors $ do
--     cAddr <- addressToCAddress <$> newAddress
--     addWalletMeta cAddr wMeta
--
--   where
--     newAddress = addressToCAddress . makePubKeyAddress . toPublic <$> newSecretKey
--
deleteAddress :: WalletWebMode ssc m => Address -> m ()
deleteAddress addr = do
    idx <- getAddrIdx addr
    deleteSecretKey $ fromIntegral idx

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

myAddresses :: MonadKeys m => m [Address]
myAddresses = map (makePubKeyAddress . toPublic) <$> getSecretKeys

getAddrIdx :: WalletWebMode ssc m => Address -> m Int
getAddrIdx addr = elemIndex addr <$> myAddresses >>= maybe notFound return
  where notFound = throwM err404 {
            errBody = encodeUtf8 $
                sformat ("Address "%addressF%" is not found in wallet") $ addr
            }

----------------------------------------------------------------------------
-- Orphan instances
----------------------------------------------------------------------------

deriving instance FromHttpApiData Coin

instance FromHttpApiData Address where
    parseUrlPiece = decodeTextAddress

instance FromHttpApiData CAddress where
    parseUrlPiece = fmap addressToCAddress . decodeTextAddress
