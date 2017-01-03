{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Module for full-node implementation of Daedalus API

module Pos.Wallet.Web.Server.Full
       ( walletServeWebFull
       ) where

import           Control.Concurrent.STM        (TVar)
import qualified Control.Monad.Catch           as Catch
import           Control.Monad.Except          (MonadError (throwError))
import           Control.TimeWarp.Rpc          (ConnectionPool, Dialog, Transfer,
                                                getConnPool)
import           Servant.Server                (Handler)
import           Servant.Utils.Enter           ((:~>) (..))
import           System.Wlog                   (logInfo)
import           Universum

import           Pos.Communication             (MutSocketState, newMutSocketState)
import           Pos.Context                   (ContextHolder, NodeContext,
                                                getNodeContext, runContextHolder)
import qualified Pos.DB                        as Modern
import           Pos.DHT.Model                 (DHTPacking)
import           Pos.DHT.Real                  (KademliaDHTContext, getKademliaDHTCtx,
                                                runKademliaDHTRaw)
import           Pos.Genesis                   (genesisSecretKeys)
import           Pos.Launcher                  (runOurDialogRaw)
import           Pos.Ssc.Class                 (SscConstraint, sscLoadGlobalState)
import           Pos.Ssc.Extra                 (SscHolder (..), SscState, runSscHolderRaw)
import           Pos.Txp.Class                 (getTxpLDWrap)
import qualified Pos.Txp.Holder                as Modern
import qualified Pos.Txp.Types.UtxoView        as UV
import           Pos.WorkMode                  (RawRealMode)

import           Pos.Wallet.KeyStorage         (addSecretKey)
import           Pos.Wallet.Web.Server.Methods (walletApplication, walletServeImpl,
                                                walletServer)
import           Pos.Wallet.Web.State          (MonadWalletWebDB (..), WalletState,
                                                WalletWebDB, runWalletWebDB)

walletServeWebFull
    :: SscConstraint ssc
    => Bool               -- whether to include genesis keys
    -> FilePath           -- to Daedalus acid-state
    -> Bool               -- Rebuild flag
    -> Word16
    -> RawRealMode ssc ()
walletServeWebFull debug = walletServeImpl $ do
    logInfo "DAEDALUS is STARTED!"
    when debug $ mapM_ addSecretKey genesisSecretKeys
    walletApplication $ walletServer nat

type WebHandler ssc = WalletWebDB (RawRealMode ssc)
type SubKademlia ssc = (Modern.TxpLDHolder ssc
                        (SscHolder ssc
                           (ContextHolder ssc
                            (Modern.DBHolder ssc
                              (Dialog DHTPacking (Transfer (MutSocketState ssc)))))))

type CPool ssc = TVar (ConnectionPool (MutSocketState ssc))

convertHandler
    :: forall ssc a . SscConstraint ssc
    => KademliaDHTContext (SubKademlia ssc)
    -> CPool ssc
    -> NodeContext ssc
    -> Modern.NodeDBs ssc
    -> Modern.TxpLDWrap ssc
    -> SscState ssc
    -> WalletState
    -> WebHandler ssc a
    -> Handler a
convertHandler kctx cp nc modernDBs tlw ssc ws handler = do
    liftIO (runOurDialogRaw cp newMutSocketState "wallet-api" .
            Modern.runDBHolder modernDBs .
            runContextHolder nc .
            runSscHolderRaw ssc .
            Modern.runTxpLDHolderReader tlw .
            runKademliaDHTRaw kctx .
            runWalletWebDB ws $
            handler)
    `Catch.catches`
    excHandlers
  where
    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError

nat :: SscConstraint ssc => WebHandler ssc (WebHandler ssc :~> Handler)
nat = do
    ws <- getWalletWebState
    kctx <- lift getKademliaDHTCtx
    tlw <- getTxpLDWrap
    ssc <- lift . lift . lift $ SscHolder ask
    nc <- getNodeContext
    modernDB <- Modern.getNodeDBs
    cp <- lift . lift . lift . lift . lift . lift . lift $ getConnPool
    return $ Nat (convertHandler kctx cp nc modernDB tlw ssc ws)
