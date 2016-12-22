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

import           Data.List                  ((!!))
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
import           Pos.Wallet.Web.ClientTypes (CAddress, addressToCAddress)
import           Pos.Wallet.Web.State       (MonadWalletWebDB (..), WalletWebDB,
                                             closeState, openState, runWalletWebDB)

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
  where openDB = openState False daedalusDbPath
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
servantHandlers = getAddresses :<|> getBalances :<|> send :<|>
                  getHistory :<|> newAddress :<|> deleteAddress

getAddresses :: WalletWebMode ssc m => m (Cors [CAddress])
getAddresses = fmap (map fst) <$> getBalances

getBalances :: WalletWebMode ssc m => m (Cors [(CAddress, Coin)])
getBalances = fmap (addHeader "*") $ join $ mapM gb <$> addresses
  where gb addr = (,) (addressToCAddress addr) <$> getBalance addr
        addresses = map (makePubKeyAddress . toPublic) <$> mySecretKeys

send :: WalletWebMode ssc m => Word -> Address -> Coin -> m (Cors ())
send srcIdx dstAddr c = fmap (addHeader "*") $ do
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

getHistory :: WalletWebMode ssc m => Address -> m (Cors [Tx])
getHistory addr = addHeader "*" . map whData <$> getTxHistory addr

newAddress :: WalletWebMode ssc m => m (Cors CAddress)
newAddress = addHeader "*" . addressToCAddress . makePubKeyAddress . toPublic <$> newSecretKey

deleteAddress :: WalletWebMode ssc m => Word -> m (Cors ())
deleteAddress key = addHeader "*" <$> deleteSecretKey key

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
