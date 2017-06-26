{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Stack of monads used by light wallet.

module Pos.Wallet.Light.Mode
       ( LightWalletMode
       , LightWalletContext(..)
       ) where

import           Universum

import qualified Control.Monad.Reader             as Mtl
import           EtherCompat
import           Mockable                         (Production, SharedAtomicT)
import           System.Wlog                      (HasLoggerName (..), LoggerName)

import           Pos.Block.BListener              (MonadBListener (..), onApplyBlocksStub,
                                                   onRollbackBlocksStub)
import           Pos.Client.Txp.Balances          (MonadBalances (..))
import           Pos.Client.Txp.History           (MonadTxHistory (..))
import           Pos.Communication.PeerState      (HasPeerState (..), PeerStateCtx,
                                                   WithPeerState (..),
                                                   clearPeerStateDefault,
                                                   getAllStatesDefault,
                                                   getPeerStateDefault)
import           Pos.Communication.Types.Protocol (NodeId)
import           Pos.DB                           (MonadGState (..))
import           Pos.Discovery                    (MonadDiscovery (..))
import           Pos.ExecMode.Context             ((:::), modeContext)
import           Pos.Reporting.MemState           (ReportingContext)
import           Pos.Ssc.GodTossing               (SscGodTossing)
import           Pos.Util.JsonLog                 (JsonLogConfig, jsonLogDefault)
import           Pos.Util.TimeWarp                (CanJsonLog (..))
import           Pos.Wallet.KeyStorage            (KeyData)
import           Pos.Wallet.Light.Redirect        (getBalanceWallet, getOwnUtxosWallet,
                                                   getTxHistoryWallet, saveTxWallet)
import           Pos.Wallet.Light.State.Acidic    (WalletState)
import           Pos.Wallet.Light.State.Core      (gsAdoptedBVDataWallet)
import           Pos.Wallet.WalletMode            (MonadBlockchainInfo (..),
                                                   MonadUpdates (..))

type LightWalletSscType = SscGodTossing
-- type LightWalletSscType = SscNistBeacon

data DiscoveryTag
data PeerStateTag

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

type LightWalletMode = Mtl.ReaderT LightWalletContext Production

instance sa ~ SharedAtomicT Production => HasPeerState sa LightWalletContext where
    peerState = lensOf @PeerStateTag

instance {-# OVERLAPPING #-} HasLoggerName LightWalletMode where
    getLoggerName = view (lensOf @LoggerName)
    modifyLoggerName f = local (lensOf @LoggerName %~ f)

instance {-# OVERLAPPING #-} CanJsonLog LightWalletMode where
    jsonLog = jsonLogDefault

instance MonadDiscovery LightWalletMode where
    getPeers = view (lensOf @DiscoveryTag)
    findPeers = view (lensOf @DiscoveryTag)

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

instance MonadTxHistory LightWalletSscType LightWalletMode where
    getTxHistory = getTxHistoryWallet
    saveTx = saveTxWallet
