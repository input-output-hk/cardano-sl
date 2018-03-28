{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}

module Cardano.Wallet.Kernel.Mode
    ( WalletMode
    , WalletContext -- opaque
    , runWalletMode
    , getWallet
    ) where

import           Universum

import           Control.Lens (makeLensesWith)
import qualified Control.Monad.Reader as Mtl
import           System.Wlog

import           Mockable
import           Pos.Block.BListener
import           Pos.Block.Slog
import           Pos.Block.Types
import           Pos.Communication
import           Pos.Context
import           Pos.Core
import           Pos.DB
import           Pos.DB.Block
import           Pos.DB.DB
import           Pos.KnownPeers
import           Pos.Launcher
import           Pos.Network.Types
import           Pos.Reporting
import           Pos.Shutdown
import           Pos.Slotting
import           Pos.Txp.Configuration
import           Pos.Txp.Logic
import           Pos.Txp.MemState
import           Pos.Util
import           Pos.Util.Chrono
import           Pos.Util.JsonLog
import           Pos.Util.TimeWarp (CanJsonLog (..))
import           Pos.WorkMode

import           Cardano.Wallet.Kernel (PassiveWalletLayer)

{-------------------------------------------------------------------------------
  The wallet context and monad
-------------------------------------------------------------------------------}

-- First approximation as I have no clue where this will @m@ come from.
data WalletContext m = WalletContext {
      wcWallet          :: !(PassiveWalletLayer m)
    , wcRealModeContext :: !(RealModeContext EmptyMempoolExt)
    }

-- | The monad that the wallet runs in
--
-- NOTE: I'd prefer to @newtype@ this but it's just too painful (too many
-- constraints cannot be derived, there are a /lot/ of them).
type WalletMode m = ReaderT (WalletContext m) Production

makeLensesWith postfixLFields ''WalletContext

getWallet :: forall m. (WalletMode m) (PassiveWalletLayer m)
getWallet = view wcWallet_L

{-------------------------------------------------------------------------------
  The raison d'être of the wallet monad: call into the wallet in the BListener
-------------------------------------------------------------------------------}

-- | Callback for new blocks
--
-- TODO: This should wrap the functionality in "Cardano.Wallet.Core" to
-- wrap things in Cardano specific types.
walletApplyBlocks :: PassiveWalletLayer m
                  -> OldestFirst NE Blund
                  -> (WalletMode m) SomeBatchOp
walletApplyBlocks _w _bs = do
    -- TODO: Call into the wallet. This should be an asynchronous operation
    -- because 'onApplyBlocks' gets called with the block lock held.
    logError "walletApplyBlocks not implemented"

    -- We don't make any changes to the DB so we always return 'mempty'.
    return mempty

-- | Callback for rollbacks
--
-- TODO: This should wrap the functionality in "Cardano.Wallet.Core" to
-- wrap things in Cardano specific types.
walletRollbackBlocks :: PassiveWalletLayer m
                     -> NewestFirst NE Blund
                     -> (WalletMode m) SomeBatchOp
walletRollbackBlocks _w _bs = do
    -- TODO: Call into the wallet. This should be an asynchronous operation
    -- because 'onRollbackBlocks' gets called with the block lock held.
    logError "walletRollbackBlocks not implemented"

    -- We don't make any changes to the DB so we always return 'mempty'.
    return mempty

instance MonadBListener (WalletMode m) where
  onApplyBlocks    bs = getWallet >>= (`walletApplyBlocks`    bs)
  onRollbackBlocks bs = getWallet >>= (`walletRollbackBlocks` bs)

{-------------------------------------------------------------------------------
  Run the wallet
-------------------------------------------------------------------------------}

runWalletMode :: forall a m. (HasConfigurations, HasCompileInfo)
              => NodeResources ()
              -> PassiveWalletLayer m
              -> (ActionSpec (WalletMode m) a, OutSpecs)
              -> Production a
runWalletMode nr wallet (action, outSpecs) =
    elimRealMode nr serverRealMode
  where
    NodeContext{..} = nrContext nr

    ekgNodeMetrics =
        EkgNodeMetrics
          (nrEkgStore nr)
          (runProduction . elimRealMode nr . walletModeToRealMode wallet)

    serverWalletMode :: (WalletMode m) a
    serverWalletMode = runServer
        (runProduction . elimRealMode nr . walletModeToRealMode wallet)
        ncNodeParams
        ekgNodeMetrics
        outSpecs
        action

    serverRealMode :: RealMode EmptyMempoolExt a
    serverRealMode = walletModeToRealMode wallet serverWalletMode

walletModeToRealMode :: forall m a. PassiveWalletLayer m -> (WalletMode m) a -> RealMode () a
walletModeToRealMode wallet ma = do
    rmc <- ask
    let env = WalletContext {
                  wcWallet          = wallet
                , wcRealModeContext = rmc
                }
    lift $ runReaderT ma env

{-------------------------------------------------------------------------------
  'WalletContext' instances

  These all just piggy-back on the 'RealModeContext'.
-------------------------------------------------------------------------------}

instance HasNodeType (WalletContext m) where
  getNodeType = getNodeType . wcRealModeContext

instance HasSlottingVar (WalletContext m) where
  slottingTimestamp = wcRealModeContext_L . slottingTimestamp
  slottingVar       = wcRealModeContext_L . slottingVar

instance HasPrimaryKey (WalletContext m) where
  primaryKey        = wcRealModeContext_L . primaryKey

instance HasReportingContext (WalletContext m) where
  reportingContext  = wcRealModeContext_L . reportingContext

instance HasSlogGState (WalletContext m) where
  slogGState        = wcRealModeContext_L . slogGState

instance HasSlogContext (WalletContext m) where
  slogContext       = wcRealModeContext_L . slogContext

instance HasShutdownContext (WalletContext m) where
  shutdownContext   = wcRealModeContext_L . shutdownContext

instance HasJsonLogConfig (WalletContext m) where
  jsonLogConfig     = wcRealModeContext_L . jsonLogConfig

instance HasSscContext (WalletContext m) where
  sscContext        = wcRealModeContext_L . sscContext

instance {-# OVERLAPPABLE #-}
    HasLens tag (RealModeContext EmptyMempoolExt) r =>
    HasLens tag (WalletContext m) r
  where
    lensOf = wcRealModeContext_L . lensOf @tag

{-------------------------------------------------------------------------------
  WalletMode instances that just piggy-back on existing infrastructure

  These are all modelled after the instances for the (legacy) 'WalletWebMode'
-------------------------------------------------------------------------------}

type instance MempoolExt (WalletMode a) = EmptyMempoolExt

instance HasConfiguration => MonadDBRead (WalletMode a) where
  dbGet         = dbGetDefault
  dbIterSource  = dbIterSourceDefault
  dbGetSerBlock = dbGetSerBlockRealDefault
  dbGetSerUndo  = dbGetSerUndoRealDefault

instance HasConfiguration => MonadDB (WalletMode a) where
  dbPut         = dbPutDefault
  dbWriteBatch  = dbWriteBatchDefault
  dbDelete      = dbDeleteDefault
  dbPutSerBlunds = dbPutSerBlundsRealDefault

instance ( HasConfiguration
         , MonadSlotsData ctx (WalletMode a)
         ) => MonadSlots ctx (WalletMode a) where
  getCurrentSlot           = getCurrentSlotSimple
  getCurrentSlotBlocking   = getCurrentSlotBlockingSimple
  getCurrentSlotInaccurate = getCurrentSlotInaccurateSimple
  currentTimeSlotting      = currentTimeSlottingSimple

instance HasConfiguration => MonadGState (WalletMode a) where
  gsAdoptedBVData = gsAdoptedBVDataDefault

instance HasConfiguration => HasAdoptedBlockVersionData (WalletMode a) where
  adoptedBVData = gsAdoptedBVData

instance MonadFormatPeers (WalletMode a) where
  formatKnownPeers f = Mtl.withReaderT wcRealModeContext $ formatKnownPeers f

instance {-# OVERLAPPING #-} CanJsonLog (WalletMode a) where
  jsonLog = jsonLogDefault

instance (HasConfiguration, HasTxpConfiguration, HasCompileInfo)
      => MonadTxpLocal (WalletMode a) where
  txpNormalize = txNormalize
  txpProcessTx = txProcessTransaction
