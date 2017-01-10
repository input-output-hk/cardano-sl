{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

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

import           Pos.Communication             (MutPeerState, newMutPeerState)
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
import           Pos.NewDHT.Real.Real          (runKademliaDHT, getKademliaDHTInstance)
import           Pos.NewDHT.Real.Types         (KademliaDHTInstance(..))
import           Pos.Wallet.KeyStorage         (MonadKeys(..))
import           Pos.Wallet.Context.Class      (WithWalletContext(..))
import           Pos.Wallet.WalletMode         (MonadTxHistory(..))
import           Pos.Wallet.WalletMode         (MonadBalances(..))
import           Pos.Communication.PeerState   (PeerStateCtx(..), getAllStates, runPeerStateHolder)
import           Pos.Update.MemState.Holder    (runUSHolder)
import           Pos.Communication.PeerState   (WithPeerState(..))

walletServeWebFull
    :: ( SscConstraint ssc
       , MonadKeys (RawRealMode ssc)
       , WithWalletContext (RawRealMode ssc)
       , MonadTxHistory (RawRealMode ssc)
       , MonadBalances (RawRealMode ssc)
       )
    => Bool               -- whether to include genesis keys
    -> FilePath           -- to Daedalus acid-state
    -> Bool               -- Rebuild flag
    -> Word16
    -> RawRealMode ssc ()
walletServeWebFull debug = walletServeImpl $ do
    logInfo "DAEDALUS has STARTED!"
    when debug $ mapM_ addSecretKey genesisSecretKeys
    walletApplication $ walletServer nat

type WebHandler ssc = WalletWebDB (RawRealMode ssc)

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
    -> (TVar DelegationWrap)
    -> PeerStateCtx ssc (RawRealMode ssc)
    -> WebHandler ssc a
    -> Handler a
convertHandler kinst nc modernDBs tlw ssc ws delWrap psCtx handler = do
    liftIO ( Modern.runDBHolder modernDBs
           . runContextHolder nc
           . runSscHolderRaw ssc
           . Modern.runTxpLDHolderReader tlw
           . runDelegationTFromTVar delWrap
           . runUSHolder
           . runKademliaDHT kinst
           . runPeerStateHolder psCtx
           . runWalletWebDB ws
           $ handler
           ) `Catch.catches` excHandlers
  where
    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError
