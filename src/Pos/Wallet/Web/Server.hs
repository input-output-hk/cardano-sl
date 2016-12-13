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

import qualified Control.Monad.Catch      as Catch
import           Control.Monad.Except     (MonadError (throwError))
import           Control.TimeWarp.Rpc     (Transfer)
import           Data.List                ((!!))
import           Formatting               (int, ords, sformat, (%))
import           Network.Wai              (Application)
import           Servant.API              ((:<|>) ((:<|>)), FromHttpApiData (parseUrlPiece))
import           Servant.Server           (Handler, ServantErr (errBody), Server, ServerT,
                                           err404, serve)
import           Servant.Utils.Enter      ((:~>) (..), enter)
import           Universum

import           Pos.DHT                  (dhtAddr, getKnownPeers)
import           Pos.DHT.Real             (KademliaDHTContext, getKademliaDHTCtx,
                                           runKademliaDHTRaw)
import           Pos.Genesis              (genesisAddresses, genesisSecretKeys)
import           Pos.Launcher             (runTimed)
#ifdef WITH_ROCKS
import qualified Pos.Modern.DB            as Modern
#endif
import           Pos.Context              (ContextHolder, NodeContext, getNodeContext,
                                           runContextHolder)
import           Pos.Ssc.Class            (SscConstraint)
import           Pos.Ssc.LocalData        (SscLDImpl, runSscLDImpl)
import qualified Pos.State                as St
import           Pos.Statistics           (getNoStatsT)
import           Pos.Txp.LocalData        (TxLocalData, getTxLocalData, setTxLocalData)
import           Pos.Types                (Address, Coin (Coin), TxOut (..), addressF, coinF,
                                           decodeTextAddress)
import           Pos.Wallet.Tx            (getBalance, submitTx)
import           Pos.Wallet.Web.Api       (WalletApi, walletApi)
import           Pos.Wallet.Web.AcidState (WalletState)
import           Pos.Web.Server           (serveImpl)
import           Pos.WorkMode             (ProductionMode, TxLDImpl,
                                           UserDialog, runTxLDImpl)

----------------------------------------------------------------------------
-- Top level functionality
----------------------------------------------------------------------------

walletServeWeb :: SscConstraint ssc => WalletState -> Word16 -> ProductionMode ssc ()
walletServeWeb ws = serveImpl (walletApplication ws)

walletApplication :: SscConstraint ssc => WalletState -> ProductionMode ssc Application
walletApplication ws = servantServer ws >>= return . serve walletApi

----------------------------------------------------------------------------
-- Servant infrastructure
----------------------------------------------------------------------------

type WalletStorage = ReaderT WalletState
type WebHandler ssc = WalletStorage (ProductionMode ssc)
type SubKademlia ssc = (TxLDImpl (
                           SscLDImpl ssc (
                               ContextHolder ssc (
#ifdef WITH_ROCKS
                                   Modern.DBHolder ssc (
#endif
                                       St.DBHolder ssc (UserDialog Transfer)))))
#ifdef WITH_ROCKS
                       )
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
    -> WalletState
    -> WebHandler ssc a
    -> Handler a
#ifdef WITH_ROCKS
convertHandler kctx tld nc ns modernDB ws handler =
#else
convertHandler kctx tld nc ns ws handler =
#endif
    liftIO (runTimed "wallet-api" .
            St.runDBHolder ns .
#ifdef WITH_ROCKS
            Modern.runDBHolder modernDB .
#endif
            runContextHolder nc .
            runSscLDImpl .
            runTxLDImpl .
            runKademliaDHTRaw kctx .
            getNoStatsT .
            flip runReaderT ws $
            setTxLocalData tld >> handler)
    `Catch.catches`
    excHandlers
  where
    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError

nat :: SscConstraint ssc => WalletState -> ProductionMode ssc (WebHandler ssc :~> Handler)
nat ws = do
    kctx <- lift getKademliaDHTCtx
    tld <- getTxLocalData
    nc <- getNodeContext
    ns <- St.getNodeState
#ifdef WITH_ROCKS
    modernDB <- Modern.getNodeDBs
    return $ Nat (convertHandler kctx tld nc ns modernDB ws)
#else
    return $ Nat (convertHandler kctx tld nc ns ws)
#endif

servantServer :: forall ssc . SscConstraint ssc => WalletState -> ProductionMode ssc (Server WalletApi)
servantServer ws = flip enter servantHandlers <$> (nat @ssc ws)

----------------------------------------------------------------------------
-- Handlers
----------------------------------------------------------------------------

servantHandlers :: SscConstraint ssc => ServerT WalletApi (WebHandler ssc)
servantHandlers = getAddresses :<|> getBalances :<|> send

getAddresses :: WebHandler ssc [Address]
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
          () <$ submitTx sk na [TxOut dstAddr c]
          putText $
              sformat ("Successfully sent "%coinF%" from "%ords%" address to "%addressF)
              c srcIdx dstAddr

----------------------------------------------------------------------------
-- Orphan instances
----------------------------------------------------------------------------

deriving instance FromHttpApiData Coin

instance FromHttpApiData Address where
    parseUrlPiece = decodeTextAddress
