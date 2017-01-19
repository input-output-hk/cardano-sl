{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
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
import           Pos.Delegation.Class          (DelegationWrap, askDelegationState)
import           Pos.Delegation.Holder         (runDelegationTFromTVar)
#ifdef DEV_MODE
import           Pos.Genesis                   (genesisSecretKeys)
#endif
import           Pos.Ssc.Class                 (SscConstraint)
import           Pos.Ssc.Extra                 (SscHolder (..), SscState, runSscHolderRaw)
import           Pos.Txp.Class                 (getTxpLDWrap)
import qualified Pos.Txp.Holder                as Modern
import           Pos.WorkMode                  (RawRealMode)

import           Pos.Communication.BiP         (BiP)
import           Pos.Communication.PeerState   (PeerStateSnapshot, WithPeerState (..),
                                                getAllStates, peerStateFromSnapshot,
                                                runPeerStateHolder)
import           Pos.DHT.Real.Real             (runKademliaDHT)
import           Pos.DHT.Real.Types            (KademliaDHTInstance (..), getKademliaDHTInstance)
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
#ifdef DEV_MODE
    when debug $ mapM_ addSecretKey genesisSecretKeys
#endif
    walletApplication $ walletServer sendActions nat

type WebHandler ssc = WalletWebSockets (WalletWebDB (RawRealMode ssc))

nat :: WebHandler ssc (WebHandler ssc :~> Handler)
nat = do
    ws       <- getWalletWebState
    kinst    <- lift . lift . lift $ getKademliaDHTInstance
    tlw      <- getTxpLDWrap
    ssc      <- lift . lift . lift . lift . lift . lift . lift $ SscHolder ask
    delWrap  <- askDelegationState
    psCtx    <- lift . lift $ getAllStates
    nc       <- getNodeContext
    modernDB <- Modern.getNodeDBs
    conn     <- getWalletWebSockets
    pure $ Nat (convertHandler kinst nc modernDB tlw ssc ws delWrap psCtx conn)

convertHandler
    :: forall ssc a .
       KademliaDHTInstance
    -> NodeContext ssc              -- (.. insert monad `m` here ..)
    -> Modern.NodeDBs ssc
    -> Modern.TxpLDWrap ssc
    -> SscState ssc
    -> WalletState
    -> (TVar DelegationWrap)
    -> PeerStateSnapshot ssc
    -> ConnectionsVar
    -> WebHandler ssc a
    -> Handler a
convertHandler kinst nc modernDBs tlw ssc ws delWrap psCtx conn handler = do
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
           . runWalletWS conn
           $ handler
           ) `Catch.catches` excHandlers
  where
    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError
