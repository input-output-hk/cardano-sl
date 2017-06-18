{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Stack of monads used by light wallet.

module Pos.Wallet.Light.Mode
       ( LightWalletMode
       , unLightWalletMode
       , LightWalletContext(..)
       ) where

import           Universum

import qualified Control.Monad.Reader             as Mtl
import qualified Ether
import           Mockable                         (Production)
import           System.Wlog                      (HasLoggerName (..), LoggerName)

import           Pos.Block.BListener              (MonadBListener (..), onApplyBlocksStub,
                                                   onRollbackBlocksStub)
import           Pos.Client.Txp.Balances          (MonadBalances (..))
import           Pos.Client.Txp.History           (MonadTxHistory (..))
import           Pos.Communication.PeerState      (PeerStateCtx, PeerStateTag,
                                                   WithPeerState (..), clearPeerStateDefault,
                                                   getAllStatesDefault, getPeerStateDefault)
import           Pos.Communication.Types.Protocol (NodeId)
import           Pos.DB                           (MonadGState (..))
import           Pos.Discovery                    (DiscoveryTag, MonadDiscovery (..),
                                                   findPeersConst, getPeersConst)
import           Pos.ExecMode                     ((:::), ExecMode (..), ExecModeM,
                                                   modeContext)
import           Pos.Reporting.MemState           (ReportingContext)
import           Pos.Util.JsonLog                 (JsonLogConfig, jsonLogDefault)
import           Pos.Util.TimeWarp                (CanJsonLog (..))
import           Pos.Wallet.KeyStorage            (KeyData)
import           Pos.Wallet.Light.Redirect        (getBalanceWallet, getOwnUtxosWallet,
                                                   getTxHistoryWallet, saveTxWallet)
import           Pos.Wallet.Light.State.Acidic    (WalletState)
import           Pos.Wallet.Light.State.Core      (gsAdoptedBVDataWallet)
import           Pos.Wallet.WalletMode            (MonadBlockchainInfo (..),
                                                   MonadUpdates (..))

modeContext [d|
    data LightWalletContext = LightWalletContext
        !(PeerStateTag     ::: PeerStateCtx Production)
        !(KeyData          ::: KeyData)
        !(WalletState      ::: WalletState)
        !(ReportingContext ::: ReportingContext)
        !(DiscoveryTag     ::: Set NodeId)
        !(JsonLogConfig    ::: JsonLogConfig)
        !(LoggerName       ::: LoggerName)
    |]

data LW

type LightWalletMode = ExecMode LW

type instance ExecModeM LW =
    Mtl.ReaderT LightWalletContext Production

unLightWalletMode :: ExecMode LW a -> ExecModeM LW a
unLightWalletMode = unExecMode

instance HasLoggerName LightWalletMode where
    getLoggerName = Ether.ask'
    modifyLoggerName = Ether.local'

instance CanJsonLog LightWalletMode where
    jsonLog = jsonLogDefault

instance MonadDiscovery LightWalletMode where
    getPeers = getPeersConst
    findPeers = findPeersConst

instance MonadBListener LightWalletMode where
    onApplyBlocks = onApplyBlocksStub
    onRollbackBlocks = onRollbackBlocksStub

-- FIXME: Dummy instance for lite-wallet.
instance MonadBlockchainInfo LightWalletMode where
    networkChainDifficulty = error "notImplemented"
    localChainDifficulty = error "notImplemented"
    blockchainSlotDuration = error "notImplemented"
    connectedPeers = error "notImplemented"

-- FIXME: Dummy instance for lite-wallet.
instance MonadUpdates LightWalletMode where
    waitForUpdate = error "notImplemented"
    applyLastUpdate = pure ()

instance WithPeerState LightWalletMode where
    getPeerState = getPeerStateDefault
    clearPeerState = clearPeerStateDefault
    getAllStates = getAllStatesDefault

instance MonadGState LightWalletMode where
    gsAdoptedBVData = gsAdoptedBVDataWallet

instance MonadBalances LightWalletMode where
    getOwnUtxos = getOwnUtxosWallet
    getBalance = getBalanceWallet

instance MonadTxHistory LightWalletMode where
    getTxHistory = getTxHistoryWallet
    saveTx = saveTxWallet
