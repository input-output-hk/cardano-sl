{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Module for full-node implementation of Daedalus API

module Pos.Wallet.Web.Server.Full
       ( walletServeWebFull
       ) where

import qualified Control.Monad.Catch           as Catch
import           Control.Monad.Except          (MonadError (throwError))
import           Control.TimeWarp.Rpc          (Dialog, Transfer)
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
import           Pos.Launcher                  (runOurDialog)
import           Pos.Ssc.Class                 (SscConstraint, sscLoadGlobalState)
import           Pos.Ssc.Extra                 (SscHolder, runSscHolder)
import qualified Pos.Txp.Holder                as Modern
import qualified Pos.Txp.Types.UtxoView        as UV
import           Pos.WorkMode                  (RawRealMode)

import           Pos.Web.Server                (serveImpl)

import           Pos.Wallet.KeyStorage         (addSecretKey)
import           Pos.Wallet.Web.Server.Methods (walletApplication, walletServer)
import           Pos.Wallet.Web.State          (MonadWalletWebDB (..), WalletState,
                                                WalletWebDB, runWalletWebDB)

walletServeWebFull
    :: SscConstraint ssc
    => FilePath           -- to Daedalus acid-state
    -> Bool               -- whether to include genesis keys
    -> Word16
    -> RawRealMode ssc ()
walletServeWebFull daedalusDbPath debug = serveImpl $ do
    logInfo "DAEDALUS is STARTED!"
    when debug $ mapM_ addSecretKey genesisSecretKeys
    walletApplication (walletServer nat) daedalusDbPath

type WebHandler ssc = WalletWebDB (RawRealMode ssc)
type SubKademlia ssc = (Modern.TxpLDHolder ssc
                        (SscHolder ssc
                           (ContextHolder ssc
                            (Modern.DBHolder ssc
                              (Dialog DHTPacking (Transfer (MutSocketState ssc)))))))

convertHandler
    :: forall ssc a . SscConstraint ssc
    => KademliaDHTContext (SubKademlia ssc)
    -> NodeContext ssc
    -> Modern.NodeDBs ssc
    -> WalletState
    -> WebHandler ssc a
    -> Handler a
convertHandler kctx nc modernDBs ws handler = do
    tip <- Modern.runDBHolder modernDBs Modern.getTip
    initGS <- Modern.runDBHolder modernDBs (sscLoadGlobalState @ssc tip)
    liftIO (runOurDialog newMutSocketState "wallet-api" .
            Modern.runDBHolder modernDBs .
            runContextHolder nc .
            flip runSscHolder initGS .
            Modern.runTxpLDHolder (UV.createFromDB . Modern._utxoDB $ modernDBs) tip .
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
    nc <- getNodeContext
    modernDB <- Modern.getNodeDBs
    return $ Nat (convertHandler kctx nc modernDB ws)
