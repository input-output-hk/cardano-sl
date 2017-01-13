{-# LANGUAGE AllowAmbiguousTypes #-}
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
import           Mockable                      (runProduction)
import           Node                          (SendActions)
import           Servant.Server                (Handler)
import           Servant.Utils.Enter           ((:~>) (..))
import           System.Wlog                   (logInfo, usingLoggerName)
import           Universum

import           Pos.Context                   (NodeContext, getNodeContext,
                                                runContextHolder)
import qualified Pos.DB                        as Modern
import           Pos.Delegation.Class          (DelegationWrap, askDelegationState,
                                                runDelegationTFromTVar)
import           Pos.Genesis                   (genesisSecretKeys)
import           Pos.Ssc.Class                 (SscConstraint)
import           Pos.Ssc.Extra                 (SscHolder (..), SscState, runSscHolderRaw)
import           Pos.Txp.Class                 (getTxpLDWrap)
import qualified Pos.Txp.Holder                as Modern
import           Pos.WorkMode                  (RawRealMode)

import           Pos.Communication.BiP         (BiP)
import           Pos.Communication.PeerState   (PeerStateSnapshot, WithPeerState (..),
                                                getAllStates, peerStateFromSnapshot,
                                                runPeerStateHolder)
import           Pos.DHT.Real.Real             (getKademliaDHTInstance, runKademliaDHT)
import           Pos.DHT.Real.Types            (KademliaDHTInstance (..))
import           Pos.Update.MemState.Holder    (runUSHolder)
import           Pos.Wallet.KeyStorage         (MonadKeys (..), addSecretKey)
import           Pos.Wallet.Web.Server.Methods (walletApplication, walletServeImpl,
                                                walletServer)
import           Pos.Wallet.Web.Server.Sockets (ConnectionsVar,
                                                MonadWalletWebSockets (..),
                                                WalletWebSockets, runWalletWS)
import           Pos.Wallet.Web.State          (MonadWalletWebDB (..), WalletState,
                                                WalletWebDB, runWalletWebDB)

walletServeWebFull
    :: SscConstraint ssc
    => SendActions BiP (RawRealMode ssc)
    -> Bool               -- whether to include genesis keys
    -> FilePath           -- to Daedalus acid-state
    -> Bool               -- Rebuild flag
    -> Word16
    -> RawRealMode ssc ()
walletServeWebFull sendActions debug = walletServeImpl $ do
    logInfo "DAEDALUS has STARTED!"
    when debug $ mapM_ addSecretKey genesisSecretKeys
    walletApplication $ walletServer sendActions nat

type WebHandler ssc = WalletWebSockets (WalletWebDB (RawRealMode ssc))

nat :: WebHandler ssc (WebHandler ssc :~> Handler)
nat = do
    ws       <- getWalletWebState
    kinst    <- lift . lift $ getKademliaDHTInstance
    tlw      <- getTxpLDWrap
    ssc      <- lift . lift . lift . lift . lift . lift $ SscHolder ask
    delWrap  <- askDelegationState
    psCtx    <- lift getAllStates
    -- psCtx    <- lift $ PeerStateCtx ask
    nc       <- getNodeContext
    modernDB <- Modern.getNodeDBs
    pure $ Nat (convertHandler kinst nc modernDB tlw ssc ws delWrap psCtx)

convertHandler
    :: forall ssc a .
       KademliaDHTInstance
    -> NodeContext ssc              -- (.. insert monad `m` here ..)
    -> Modern.NodeDBs ssc
    -> Modern.TxpLDWrap ssc
    -> SscState ssc
    -> WalletState
<<<<<<< HEAD
    -> (TVar DelegationWrap)
    -> PeerStateSnapshot ssc
    -> WebHandler ssc a
    -> Handler a
convertHandler kinst nc modernDBs tlw ssc ws delWrap psCtx handler = do
    liftIO ( runProduction
           . usingLoggerName "wallet-api"
           . Modern.runDBHolder modernDBs
           . runContextHolder nc
           . runSscHolderRaw ssc
           . Modern.runTxpLDHolderReader tlw
           . runDelegationTFromTVar delWrap
           . runUSHolder
           . runKademliaDHT kinst
           . (\m -> flip runPeerStateHolder m =<< peerStateFromSnapshot psCtx)
           . runWalletWebDB ws
           $ handler
           ) `Catch.catches` excHandlers
  where
    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError
=======
    -> TVar DelegationWrap
    -> TVar US.MemState
    -> ConnectionsVar
    -> WebHandler ssc a
    -> Handler a
convertHandler kctx cp nc modernDBs tlw ssc ws delWrap usTVar wsConn handler = do
    liftIO (runOurDialogRaw cp newMutSocketState "wallet-api" .
            Modern.runDBHolder modernDBs .
            runContextHolder nc .
            runSscHolderRaw ssc .
            Modern.runTxpLDHolderReader tlw .
            runDelegationTFromTVar delWrap .
            US.runUSHolderFromTVar usTVar .
            runKademliaDHTRaw kctx .
            runWalletWebDB ws .
            runWalletWS wsConn $
            handler)
    `Catch.catches`
    excHandlers
  where
    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError

nat :: WebHandler ssc (WebHandler ssc :~> Handler)
nat = do
    wsConn <- getWalletWebSockets
    ws <- getWalletWebState
    kctx <- lift . lift $ getKademliaDHTCtx
    tlw <- getTxpLDWrap
    ssc <- lift . lift . lift . lift . lift . lift $ SscHolder ask
    delWrap <- askDelegationState
    usTVar <- US.askUSMemState
    nc <- getNodeContext
    modernDB <- Modern.getNodeDBs
    cp <- lift . lift . lift . lift . lift . lift . lift . lift . lift . lift $ getConnPool
    pure $ Nat (convertHandler kctx cp nc modernDB tlw ssc ws delWrap usTVar wsConn)
>>>>>>> master
