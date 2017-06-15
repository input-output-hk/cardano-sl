{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Stack of monads used by light wallet.

module Pos.Wallet.Light.Mode
       ( LightWalletMode
       , unLightWalletMode
       , LightWalletContext(..)
       ) where

import           Universum

import           Control.Lens                     (makeLensesFor)
import qualified Control.Monad.Reader             as Mtl
import qualified Ether
import           Ether.Internal                   (HasLens (..))
import           Mockable                         (Production)
import           System.Wlog                      (HasLoggerName (..), LoggerName)

import           Pos.Block.BListener              (MonadBListener (..), onApplyBlocksStub,
                                                   onRollbackBlocksStub)
import           Pos.Client.Txp.Balances          (MonadBalances (..))
import           Pos.Client.Txp.History           (MonadTxHistory (..))
import           Pos.Communication.PeerState      (PeerStateCtx, PeerStateTag,
                                                   WithPeerState (..), clearPeerStateReal,
                                                   getAllStatesReal, getPeerStateReal)
import           Pos.Communication.Types.Protocol (NodeId)
import           Pos.DB                           (MonadGState (..))
import           Pos.Discovery                    (DiscoveryTag, MonadDiscovery (..),
                                                   findPeersConst, getPeersConst)
import           Pos.ExecMode                     (ExecMode (..), ExecModeM)
import           Pos.Reporting.MemState           (ReportingContext)
import           Pos.Util.JsonLog                 (JsonLogConfig, jsonLogReal)
import           Pos.Util.TimeWarp                (CanJsonLog (..))
import           Pos.Wallet.KeyStorage            (KeyData)
import           Pos.Wallet.Light.Redirect        (getBalanceWallet, getOwnUtxosWallet,
                                                   getTxHistoryWallet, saveTxWallet)
import           Pos.Wallet.Light.State.Acidic    (WalletState)
import           Pos.Wallet.Light.State.Core      (gsAdoptedBVDataWallet)
import           Pos.Wallet.WalletMode            (MonadBlockchainInfo (..),
                                                   MonadUpdates (..))

data LightWalletContext = LightWalletContext
    { lwcPeerState        :: !(PeerStateCtx Production)
    , lwcKeyData          :: !KeyData
    , lwcWalletState      :: !WalletState
    , lwcReportingContext :: !ReportingContext
    , lwcDiscoveryConst   :: !(Set NodeId)
    , lwcJsonLogConfig    :: !JsonLogConfig
    , lwcLoggerName       :: !LoggerName
    }

makeLensesFor
  [ ("lwcPeerState", "lwcPeerStateL")
  , ("lwcKeyData", "lwcKeyDataL")
  , ("lwcWalletState", "lwcWalletStateL")
  , ("lwcReportingContext", "lwcReportingContextL")
  , ("lwcDiscoveryConst", "lwcDiscoveryConstL")
  , ("lwcJsonLogConfig", "lwcJsonLogConfigL")
  , ("lwcLoggerName", "lwcLoggerNameL") ]
  ''LightWalletContext

instance r ~ PeerStateCtx Production => HasLens PeerStateTag LightWalletContext r where
    lensOf = lwcPeerStateL

instance HasLens KeyData LightWalletContext KeyData where
    lensOf = lwcKeyDataL

instance HasLens WalletState LightWalletContext WalletState where
    lensOf = lwcWalletStateL

instance HasLens ReportingContext LightWalletContext ReportingContext where
    lensOf = lwcReportingContextL

instance HasLens DiscoveryTag LightWalletContext (Set NodeId) where
    lensOf = lwcDiscoveryConstL

instance HasLens JsonLogConfig LightWalletContext JsonLogConfig where
    lensOf = lwcJsonLogConfigL

instance HasLens LoggerName LightWalletContext LoggerName where
    lensOf = lwcLoggerNameL

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
    jsonLog = jsonLogReal

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
    getPeerState = getPeerStateReal
    clearPeerState = clearPeerStateReal
    getAllStates = getAllStatesReal

instance MonadGState LightWalletMode where
    gsAdoptedBVData = gsAdoptedBVDataWallet

instance MonadBalances LightWalletMode where
    getOwnUtxos = getOwnUtxosWallet
    getBalance = getBalanceWallet

instance MonadTxHistory LightWalletMode where
    getTxHistory = getTxHistoryWallet
    saveTx = saveTxWallet
