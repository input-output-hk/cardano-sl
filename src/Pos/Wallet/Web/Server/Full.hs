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
import           Servant.Server                (Handler)
import           Servant.Utils.Enter           ((:~>) (..))
import           System.Wlog                   (logInfo)
import           Universum

import           Pos.Communication             (MutSocketState, newMutSocketState)
import           Pos.Context                   (ContextHolder, NodeContext,
                                                getNodeContext, runContextHolder)
import qualified Pos.DB                        as Modern
import           Pos.Delegation.Class          (DelegationT, DelegationWrap,
                                                askDelegationState,
                                                runDelegationTFromTVar)
import           Pos.Genesis                   (genesisSecretKeys)
import           Pos.Ssc.Class                 (SscConstraint)
import           Pos.Ssc.Extra                 (SscHolder (..), SscState, runSscHolderRaw)
import           Pos.Txp.Class                 (getTxpLDWrap)
import qualified Pos.Txp.Holder                as Modern
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
walletServeWebFull = notImplemented
--walletServeWebFull debug = walletServeImpl $ do
--    logInfo "DAEDALUS has STARTED!"
--    when debug $ mapM_ addSecretKey genesisSecretKeys
--    walletApplication $ walletServer nat

-- type WebHandler ssc = WalletWebDB (RawRealMode ssc)
--
-- -- RawRealMode without last layer
-- type SubKademlia ssc =
--     DelegationT (
--     Modern.TxpLDHolder ssc (
--     SscHolder ssc (
--     ContextHolder ssc (
--     Modern.DBHolder ssc (
--     Dialog DHTPacking (
--     Transfer (
--     MutSocketState ssc)))))))
--
-- type CPool ssc = TVar (ConnectionPool (MutSocketState ssc))
--
-- convertHandler
--     :: forall ssc a .
--        KademliaDHTContext (SubKademlia ssc)
--     -> CPool ssc
--     -> NodeContext ssc
--     -> Modern.NodeDBs ssc
--     -> Modern.TxpLDWrap ssc
--     -> SscState ssc
--     -> WalletState
--     -> (TVar DelegationWrap)
--     -> WebHandler ssc a
--     -> Handler a
-- convertHandler kctx cp nc modernDBs tlw ssc ws delWrap handler = do
--     liftIO (runOurDialogRaw cp newMutSocketState "wallet-api" .
--             Modern.runDBHolder modernDBs .
--             runContextHolder nc .
--             runSscHolderRaw ssc .
--             Modern.runTxpLDHolderReader tlw .
--             runDelegationTFromTVar delWrap .
--             runKademliaDHTRaw kctx .
--             runWalletWebDB ws $
--             handler)
--     `Catch.catches`
--     excHandlers
--   where
--     excHandlers = [Catch.Handler catchServant]
--     catchServant = throwError
--
-- nat :: WebHandler ssc (WebHandler ssc :~> Handler)
-- nat = do
--     ws <- getWalletWebState
--     kctx <- lift getKademliaDHTCtx
--     tlw <- getTxpLDWrap
--     ssc <- lift . lift . lift . lift $ SscHolder ask
--     delWrap <- askDelegationState
--     nc <- getNodeContext
--     modernDB <- Modern.getNodeDBs
--     cp <- lift . lift . lift . lift . lift . lift . lift . lift $ getConnPool
--     pure $ Nat (convertHandler kctx cp nc modernDB tlw ssc ws delWrap)
