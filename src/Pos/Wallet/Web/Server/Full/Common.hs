{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Module containing common parts for all wallet modes.

module Pos.Wallet.Web.Server.Full.Common
       ( nat
       , convertHandler
       ) where

import           Universum

import qualified Control.Monad.Catch           as Catch
import           Control.Monad.Except          (MonadError (throwError))
import           Data.Tagged                   (Tagged (..))
import qualified Ether
import           Mockable                      (runProduction)
import           Servant.Server                (Handler)
import           Servant.Utils.Enter           ((:~>) (..))
import           System.Wlog                   (usingLoggerName)

import           Pos.Block.BListener           (runBListenerStub)
import           Pos.Communication.PeerState   (PeerStateSnapshot, PeerStateTag,
                                                WithPeerState (..), getAllStates,
                                                peerStateFromSnapshot,
                                                runPeerStateRedirect)
import           Pos.Context                   (NodeContext, NodeContextTag)
import           Pos.DB                        (NodeDBs, getNodeDBs, runDBPureRedirect)
import           Pos.DB.Block                  (runBlockDBRedirect)
import           Pos.DB.DB                     (runGStateCoreRedirect)
import           Pos.Delegation.Class          (DelegationVar, askDelegationState)
import           Pos.Discovery                 (runDiscoveryRedirect)
import           Pos.Slotting                  (NtpSlottingVar, SlottingVar,
                                                askFullNtpSlotting, askSlotting,
                                                runSlotsDataRedirect)
import           Pos.Slotting.Ntp              (runSlotsRedirect)
import           Pos.Ssc.Extra                 (SscMemTag, SscState)
import           Pos.Ssc.Extra.Class           (askSscMem)
import           Pos.Txp                       (GenericTxpLocalData, TxpHolderTag,
                                                askTxpMem)
import           Pos.Wallet.Redirect           (runWalletRedirects)
import           Pos.Util.TimeWarp             (runWithoutJsonLogT)
import           Pos.Wallet.SscType            (WalletSscType)
import           Pos.Wallet.Web.Server.Methods (WalletWebHandler)
import           Pos.Wallet.Web.Server.Sockets (ConnectionsVar, getWalletWebSockets,
                                                runWalletWS)
import           Pos.Wallet.Web.State          (WalletState, getWalletWebState,
                                                runWalletWebDB)
import           Pos.WorkMode                  (RealMode (..), TxpExtra_TMP)

type WebHandler = WalletWebHandler (RealMode WalletSscType)

nat :: WebHandler (WebHandler :~> Handler)
nat = do
    ws         <- getWalletWebState
    tlw        <- askTxpMem
    ssc        <- askSscMem
    delWrap    <- askDelegationState
    psCtx      <- getAllStates
    nc         <- Ether.ask @NodeContextTag
    modernDB   <- getNodeDBs
    conn       <- getWalletWebSockets
    slotVar    <- askSlotting
    ntpSlotVar <- askFullNtpSlotting
    pure $ NT (convertHandler nc modernDB tlw ssc ws delWrap
                              psCtx conn slotVar ntpSlotVar)

convertHandler
    :: NodeContext WalletSscType              -- (.. insert monad `m` here ..)
    -> NodeDBs
    -> GenericTxpLocalData TxpExtra_TMP
    -> SscState WalletSscType
    -> WalletState
    -> DelegationVar
    -> PeerStateSnapshot
    -> ConnectionsVar
    -> SlottingVar
    -> (Bool, NtpSlottingVar)
    -> WebHandler a
    -> Handler a
convertHandler nc modernDBs tlw ssc ws delWrap psCtx
               conn slotVar ntpSlotVar handler =
    liftIO (realRunner . walletRunner $ handler) `Catch.catches` excHandlers
  where
    walletRunner = runWalletWebDB ws
      . runWalletWS conn
      . runWalletRedirects

    realRunner :: forall t . RealMode WalletSscType t -> IO t
    realRunner (RealMode act) = runProduction 
           . runWithoutJsonLogT
           . usingLoggerName "wallet-api"
           . flip Ether.runReadersT nc
           . (\m -> do
               peerStateCtx <- peerStateFromSnapshot psCtx
               Ether.runReadersT m
                   ( Tagged @NodeDBs modernDBs
                   , Tagged @SlottingVar slotVar
                   , Tagged @(Bool, NtpSlottingVar) ntpSlotVar
                   , Tagged @SscMemTag ssc
                   , Tagged @TxpHolderTag tlw
                   , Tagged @DelegationVar delWrap
                   , Tagged @PeerStateTag peerStateCtx
                   ))
           . runDBPureRedirect
           . runBlockDBRedirect
           . runSlotsDataRedirect
           . runSlotsRedirect
           . runDiscoveryRedirect
           . runPeerStateRedirect
           . runGStateCoreRedirect
           . runBListenerStub
           $ act

    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError
{-# NOINLINE convertHandler #-}
