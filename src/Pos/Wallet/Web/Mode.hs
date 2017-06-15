{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Pos.Wallet.Web.Mode
    ( WalletWebMode
    , unWalletWebMode
    , WalletWebModeContext(..)
    ) where

import           Universum

import           Control.Lens
import qualified Control.Monad.Reader          as Mtl
import qualified Ether
import           Ether.Internal                (HasLens (..))
import           Mockable                      (Production)
import           System.Wlog                   (HasLoggerName (..))

import           Pos.Block.BListener           (MonadBListener (..), onApplyBlocksStub,
                                                onRollbackBlocksStub)
import           Pos.Block.Core                (Block, BlockHeader)
import           Pos.Block.Types               (Undo)
import           Pos.Communication.PeerState   (WithPeerState (..), clearPeerStateReal,
                                                getAllStatesReal, getPeerStateReal)
import           Pos.Core                      (IsHeader)
import           Pos.DB                        (MonadGState (..))
import           Pos.DB.Block                  (MonadBlockDBWrite (..), dbGetBlockReal,
                                                dbGetBlockReal', dbGetHeaderReal,
                                                dbGetHeaderReal', dbGetUndoReal,
                                                dbGetUndoReal', dbPutBlundReal)
import           Pos.DB.Class                  (MonadBlockDBGeneric (..), MonadDB (..),
                                                MonadDBRead (..))
import           Pos.DB.DB                     (gsAdoptedBVDataDB)
import           Pos.DB.Redirect               (dbDeleteReal, dbGetReal, dbPutReal,
                                                dbWriteBatchReal)

import           Pos.Client.Txp.Balances       (MonadBalances (..), getBalanceWebWallet,
                                                getOwnUtxosWebWallet)
import           Pos.Client.Txp.History        (MonadTxHistory (..),
                                                getTxHistoryWebWallet, saveTxWebWallet)
import           Pos.Discovery                 (MonadDiscovery (..), findPeersReal,
                                                getPeersReal)
import           Pos.ExecMode                  (ExecMode (..), ExecModeM)
import           Pos.Slotting.Class            (MonadSlots (..))
import           Pos.Slotting.Impl.Sum         (currentTimeSlottingReal,
                                                getCurrentSlotBlockingReal,
                                                getCurrentSlotInaccurateReal,
                                                getCurrentSlotReal)
import           Pos.Slotting.MemState         (MonadSlotsData (..), getSlottingDataReal,
                                                getSystemStartReal, putSlottingDataReal,
                                                waitPenultEpochEqualsReal)
import           Pos.Ssc.Class.Types           (SscBlock)
import           Pos.Util                      (Some (..))
import           Pos.Util.JsonLog              (jsonLogReal)
import           Pos.Util.TimeWarp             (CanJsonLog (..))
import           Pos.Wallet.Redirect           (MonadBlockchainInfo (..),
                                                MonadUpdates (..),
                                                applyLastUpdateWebWallet,
                                                blockchainSlotDurationWebWallet,
                                                connectedPeersWebWallet,
                                                localChainDifficultyWebWallet,
                                                networkChainDifficultyWebWallet,
                                                waitForUpdateWebWallet)
import           Pos.Wallet.SscType            (WalletSscType)
import           Pos.Wallet.Web.Server.Sockets (ConnectionsVar)
import           Pos.Wallet.Web.State.State    (WalletState)
import           Pos.Wallet.Web.Tracking       (MonadWalletTracking (..),
                                                syncOnImportWebWallet,
                                                syncWSetsAtStartWebWallet,
                                                txMempoolToModifierWebWallet)
import           Pos.WorkMode                  (RealModeContext)

data WalletWebModeContext = WalletWebModeContext
    { wmcWalletState     :: !WalletState
    , wmcWebSockets      :: !ConnectionsVar
    , wmcRealModeContext :: !(RealModeContext WalletSscType)
    }

makeLensesFor
    [ ("wmcRealModeContext", "wmcRealModeContextL")
    , ("wmcWalletState",     "wmcWalletStateL")
    , ("wmcWebSockets",      "wmcWebSocketsL") ]
    ''WalletWebModeContext

instance {-# OVERLAPPABLE #-} HasLens tag (RealModeContext WalletSscType) r => HasLens tag WalletWebModeContext r where
    lensOf = wmcRealModeContextL . lensOf @tag

instance HasLens WalletState WalletWebModeContext WalletState where
    lensOf = wmcWalletStateL

instance HasLens ConnectionsVar WalletWebModeContext ConnectionsVar where
    lensOf = wmcWebSocketsL

data WWEB

type WalletWebMode = ExecMode WWEB

type instance ExecModeM WWEB =
    Mtl.ReaderT WalletWebModeContext Production

unWalletWebMode :: ExecMode WWEB a -> ExecModeM WWEB a
unWalletWebMode = unExecMode

instance WithPeerState WalletWebMode where
    getPeerState = getPeerStateReal
    clearPeerState = clearPeerStateReal
    getAllStates = getAllStatesReal

instance MonadSlotsData WalletWebMode where
    getSystemStart = getSystemStartReal
    getSlottingData = getSlottingDataReal
    waitPenultEpochEquals = waitPenultEpochEqualsReal
    putSlottingData = putSlottingDataReal

instance MonadSlots WalletWebMode where
    getCurrentSlot = getCurrentSlotReal
    getCurrentSlotBlocking = getCurrentSlotBlockingReal
    getCurrentSlotInaccurate = getCurrentSlotInaccurateReal
    currentTimeSlotting = currentTimeSlottingReal

instance MonadDiscovery WalletWebMode where
    getPeers = getPeersReal
    findPeers = findPeersReal

instance HasLoggerName WalletWebMode where
    getLoggerName = Ether.ask'
    modifyLoggerName = Ether.local'

instance CanJsonLog WalletWebMode where
    jsonLog = jsonLogReal

instance MonadDBRead WalletWebMode where
    dbGet = dbGetReal

instance MonadDB WalletWebMode where
    dbPut = dbPutReal
    dbWriteBatch = dbWriteBatchReal
    dbDelete = dbDeleteReal

instance MonadBlockDBWrite WalletSscType WalletWebMode where
    dbPutBlund = dbPutBlundReal

instance MonadBlockDBGeneric (BlockHeader WalletSscType) (Block WalletSscType) Undo WalletWebMode
  where
    dbGetBlock  = dbGetBlockReal @WalletSscType
    dbGetUndo   = dbGetUndoReal @WalletSscType
    dbGetHeader = dbGetHeaderReal @WalletSscType

instance MonadBlockDBGeneric (Some IsHeader) (SscBlock WalletSscType) () WalletWebMode
  where
    dbGetBlock  = dbGetBlockReal' @WalletSscType
    dbGetUndo   = dbGetUndoReal' @WalletSscType
    dbGetHeader = dbGetHeaderReal' @WalletSscType

instance MonadGState WalletWebMode where
    gsAdoptedBVData = gsAdoptedBVDataDB

instance MonadBListener WalletWebMode where
    onApplyBlocks = onApplyBlocksStub
    onRollbackBlocks = onRollbackBlocksStub

instance MonadUpdates WalletWebMode where
    waitForUpdate = waitForUpdateWebWallet
    applyLastUpdate = applyLastUpdateWebWallet

instance MonadBlockchainInfo WalletWebMode where
    networkChainDifficulty = networkChainDifficultyWebWallet
    localChainDifficulty = localChainDifficultyWebWallet
    connectedPeers = connectedPeersWebWallet
    blockchainSlotDuration = blockchainSlotDurationWebWallet

instance MonadBalances WalletWebMode where
    getOwnUtxos = getOwnUtxosWebWallet
    getBalance = getBalanceWebWallet

instance MonadTxHistory WalletWebMode where
    getTxHistory = getTxHistoryWebWallet
    saveTx = saveTxWebWallet

instance MonadWalletTracking WalletWebMode where
    syncWSetsAtStart = syncWSetsAtStartWebWallet
    syncOnImport = syncOnImportWebWallet
    txMempoolToModifier = txMempoolToModifierWebWallet
