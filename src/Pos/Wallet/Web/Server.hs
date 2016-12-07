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

import qualified Control.Monad.Catch                  as Catch
import           Control.Monad.Except                 (MonadError (throwError))
import           Control.TimeWarp.Rpc                 (BinaryP, Dialog, Transfer)
import           Control.TimeWarp.Timed               (TimedIO, runTimedIO)
import           Data.List                            ((!!))
import           Formatting                           (int, ords, sformat, (%))
import           Network.Wai                          (Application)
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant.API                          ((:<|>) ((:<|>)),
                                                       FromHttpApiData (parseUrlPiece))
import           Servant.Server                       (Handler, ServantErr (errBody),
                                                       Server, ServerT, err404, serve)
import           Servant.Utils.Enter                  ((:~>) (..), enter)
import           Universum

import           Pos.Crypto                           (parseFullPublicKey)
import           Pos.DHT                              (dhtAddr, getKnownPeers)
import           Pos.Genesis                          (genesisAddresses,
                                                       genesisSecretKeys)
import           Pos.Launcher                         (runTimed)
import           Pos.Ssc.Class                        (SscConstraint)
import qualified Pos.State                            as St
import           Pos.Types                            (Address, Coin (Coin), TxOut (..),
                                                       addressF, coinF, makePubKeyAddress)
import           Pos.Wallet.Tx                        (getBalance, submitTx)
import           Pos.Wallet.Web.Api                   (WalletApi, walletApi)
import           Pos.Web.Server                       (MyWorkMode)
import           Pos.WorkMode                         (ContextHolder, DBHolder,
                                                       NodeContext, WorkMode,
                                                       getNodeContext, ncPublicKey,
                                                       ncSscContext, runContextHolder,
                                                       runDBHolder)

----------------------------------------------------------------------------
-- Top level functionality
----------------------------------------------------------------------------

walletServeWeb :: MyWorkMode ssc m => Word16 -> m ()
walletServeWeb = serveImpl walletApplication

walletApplication :: MyWorkMode ssc m => m Application
walletApplication = servantServer >>= return . serve walletApi

serveImpl :: MonadIO m => m Application -> Word16 -> m ()
serveImpl application port =
    liftIO . run (fromIntegral port) . logStdoutDev =<< application

----------------------------------------------------------------------------
-- Servant infrastructure
----------------------------------------------------------------------------

type WebHandler ssc = ContextHolder ssc (DBHolder ssc (Dialog BinaryP Transfer))

convertHandler
    :: forall ssc a.
       NodeContext ssc -> St.NodeState ssc -> WebHandler ssc a -> Handler a
convertHandler nc ns handler =
    liftIO (runTimed "wallet-api" .
            runDBHolder ns .
            runContextHolder nc $
            handler) `Catch.catches`
    excHandlers
  where
    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError

nat :: MyWorkMode ssc m => m (WebHandler ssc :~> Handler)
nat = do
    nc <- getNodeContext
    ns <- St.getNodeState
    return $ Nat (convertHandler nc ns)

servantServer :: forall ssc m . MyWorkMode ssc m => m (Server WalletApi)
servantServer = flip enter servantHandlers <$> (nat @ssc @m)

----------------------------------------------------------------------------
-- Handlers
----------------------------------------------------------------------------

servantHandlers :: SscConstraint ssc => ServerT WalletApi (WebHandler ssc)
servantHandlers = getAddresses :<|> getBalances :<|> send

getAddresses :: SscConstraint ssc => WebHandler ssc [Address]
getAddresses = pure genesisAddresses

getBalances :: SscConstraint ssc => WebHandler ssc [(Address, Coin)]
getBalances = mapM gb genesisAddresses
  where gb addr = (,) addr <$> getBalance addr

send :: SscConstraint ssc
     => Word -> Address -> Coin -> WebHandler ssc ()
send srcIdx dstAddr c
    | fromIntegral srcIdx > length genesisAddresses =
        throwM err404 {
          errBody = encodeUtf8 $
                    sformat ("There are only "%int%" addresses in wallet") $
                    length genesisAddresses
          }
    | otherwise = do
          let sk = genesisSecretKeys !! fromIntegral srcIdx
          na <- fmap dhtAddr <$> getKnownPeers
          submitTx sk na [TxOut dstAddr c]
          putText $
              sformat ("Successfully sent "%coinF%" from "%ords%" address to "%addressF)
              c srcIdx dstAddr

----------------------------------------------------------------------------
-- Orphan instances
----------------------------------------------------------------------------

deriving instance FromHttpApiData Coin

instance FromHttpApiData Address where
    parseUrlPiece = fmap makePubKeyAddress . maybe onError pure . parseFullPublicKey
      where
        onError = throwError "failed to parse address"
