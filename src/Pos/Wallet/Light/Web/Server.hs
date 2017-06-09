{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Module for lite-wallet implementation of Daedalus API. We don't
-- maintain it because we don't know whether we will ever use at least
-- 30% of its current state.

module Pos.Wallet.Light.Web.Server
       ( walletServeWebLite
       , walletServerOuts
       ) where

import           Universum

import           Pos.Communication.Protocol    (SendActions)
import           Pos.Ssc.Class                 (SscHelpersClass)
import           Pos.Wallet.Light.Mode         (LightWalletMode (..))
import           Pos.Wallet.SscType            (WalletSscType)
import           Pos.Wallet.Web.Server.Methods (walletServerOuts)

-- type WebHandler = WalletWebSockets (WalletWebDB LightWalletMode)

-- type MainWalletState = WS.WalletState

walletServeWebLite
    :: SscHelpersClass WalletSscType
    => SendActions LightWalletMode
    -> FilePath
    -> Bool
    -> Word16
    -> LightWalletMode ()
walletServeWebLite __sendActions __dbPath __dbRebuild __port =
    error "lite wallet's web server is not implemented, sorry about that"
    -- bracketWalletWebDB dbPath dbRebuild $ \db ->
    --     bracketWalletWS $ \conn -> do
    --         let runner =
    --               runWalletWebDB db .
    --               runWalletWS conn .
    --               runBalancesRedirect .
    --               runTxHistoryRedirect .
    --               runUpdatesRedirect .
    --               runBlockchainInfoRedirect
    --         let hoistedSA :: SendActions (WalletWebHandler LightWalletMode)
    --             hoistedSA = hoistSendActions powerLift runner sendActions
    --         let action :: WalletWebHandler LightWalletMode Application
    --             action = walletApplication $ walletServer hoistedSA nat
    --         runner $ walletServeImpl action port

-- nat :: WebHandler (WebHandler :~> Handler)
-- nat = do
--     wsConn <- getWalletWebSockets
--     ws     <- getWalletWebState
--     kd     <- Ether.ask'
--     mws    <- getWalletState
--     peers  <- getPeers
--     pure $ NT (convertHandler mws kd ws wsConn peers)

-- convertHandler
--     :: MainWalletState
--     -> KeyData
--     -> WalletState
--     -> ConnectionsVar
--     -> Set NodeId
--     -> WebHandler a
--     -> Handler a
-- convertHandler mws kd ws wsConn peers handler = do
--     stateM <- liftIO SM.newIO
--     liftIO ( runProduction
--            . usingLoggerName "wallet-lite-api"
--            . flip Ether.runReadersT
--                 ( Tagged @PeerStateTag stateM
--                 , Tagged @KeyData kd
--                 , Tagged @MainWalletState mws
--                 , Tagged @ReportingContext emptyReportingContext )
--            . runDBPureRedirect
--            . runBlockDBRedirect
--            . runTxHistoryWalletRedirect
--            . runBalancesWalletRedirect
--            . runGStateCoreWalletRedirect
--            . runPeerStateRedirect
--            . runUpdatesNotImplemented
--            . runBlockchainInfoNotImplemented
--            . runBListenerStub
--            . runDiscoveryConstT peers
--            . (\(LightWalletMode m) -> m)
--            . runWalletWebDB ws
--            . runWalletWS wsConn
--            $ handler
--            ) `Catch.catches` excHandlers
--   where
--     excHandlers = [Catch.Handler catchServant]
--     catchServant = throwError
-- {-# NOINLINE convertHandler #-}

-- instance Monad m => MonadWalletTracking (WalletWebSockets m) where
--     syncWSetsAtStart = const pass
--     syncOnImport = const pass
--     txMempoolToModifier = const (pure mempty)

-- -- Stub implementations for lite wallet.
-- instance Ether.MonadReader NodeDBs NodeDBs Production where
--     ask = error "Stub implementation for Lite Wallet"
--     local = error "Stub implementation for Lite Wallet"

-- instance Ether.MonadReader
--              BlkSemaphore
--              BlkSemaphore
--              Production where
--     ask = error "Stub implementation for Lite Wallet"
--     local = error "Stub implementation for Lite Wallet"

-- instance Ether.MonadReader
--              NodeContextTag
--              (NodeContext WalletSscType)
--              Production where
--     ask = error "Stub implementation for Lite Wallet"
--     local = error "Stub implementation for Lite Wallet"
