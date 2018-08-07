{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}

module Cardano.Wallet.Kernel.Mode
    ( WalletMode
    , WalletContext -- opaque
    , runWalletMode
    , getWallet
    ) where

import           Control.Lens (makeLensesWith)
import           Universum

import           Pos.Chain.Block
import           Pos.Chain.Txp
import           Pos.Context
import           Pos.Core
import           Pos.Core.Chrono
import           Pos.Core.JsonLog (CanJsonLog (..))
import           Pos.Core.Reporting (HasMisbehaviorMetrics (..))
import           Pos.DB
import           Pos.DB.Block hiding (applyBlocks, rollbackBlocks)
import           Pos.DB.DB
import           Pos.DB.Txp.Logic
import           Pos.DB.Txp.MemState
import           Pos.Infra.Diffusion.Types (Diffusion, hoistDiffusion)
import           Pos.Infra.Network.Types
import           Pos.Infra.Reporting
import           Pos.Infra.Shutdown
import           Pos.Infra.Slotting
import           Pos.Infra.Util.JsonLog.Events
import           Pos.Launcher
import           Pos.Util
import           Pos.WorkMode

import           Cardano.Wallet.WalletLayer (PassiveWalletLayer (..),
                     applyBlocks, rollbackBlocks)

{-------------------------------------------------------------------------------
  The wallet context and monad
-------------------------------------------------------------------------------}

data WalletContext = WalletContext {
      wcWallet          :: !(PassiveWalletLayer IO)
    , wcRealModeContext :: !(RealModeContext EmptyMempoolExt)
    }

-- | The monad that the wallet runs in
--
-- NOTE: I'd prefer to @newtype@ this but it's just too painful (too many
-- constraints cannot be derived, there are a /lot/ of them).
type WalletMode = ReaderT WalletContext IO

makeLensesWith postfixLFields ''WalletContext

getWallet :: WalletMode (PassiveWalletLayer IO)
getWallet = view wcWallet_L

{-------------------------------------------------------------------------------
  The raison d'être of the wallet monad: call into the wallet in the BListener
-------------------------------------------------------------------------------}

-- | Callback for new blocks
--
-- TODO: This should wrap the functionality in "Cardano.Wallet.Core" to
-- wrap things in Cardano specific types.
walletApplyBlocks :: PassiveWalletLayer IO
                  -> OldestFirst NE Blund
                  -> WalletMode SomeBatchOp
walletApplyBlocks _w _bs = do
    lift $ applyBlocks _w _bs

    -- We don't make any changes to the DB so we always return 'mempty'.
    return mempty

-- | Callback for rollbacks
--
-- TODO: This should wrap the functionality in "Cardano.Wallet.Core" to
-- wrap things in Cardano specific types.
walletRollbackBlocks :: PassiveWalletLayer IO
                     -> NewestFirst NE Blund
                     -> WalletMode SomeBatchOp
walletRollbackBlocks _w _bs = do
    lift $ rollbackBlocks _w _bs

    -- We don't make any changes to the DB so we always return 'mempty'.
    return mempty

instance MonadBListener WalletMode where
  onApplyBlocks    bs = getWallet >>= (`walletApplyBlocks`    bs)
  onRollbackBlocks bs = getWallet >>= (`walletRollbackBlocks` bs)

{-------------------------------------------------------------------------------
  Run the wallet
-------------------------------------------------------------------------------}

runWalletMode :: forall a. (HasConfigurations, HasCompileInfo)
              => ProtocolMagic
              -> TxpConfiguration
              -> NodeResources ()
              -> PassiveWalletLayer IO
              -> (Diffusion WalletMode -> WalletMode a)
              -> IO a
runWalletMode pm txpConfig nr wallet action =
    runRealMode pm txpConfig nr $ \diffusion ->
        walletModeToRealMode wallet (action (hoistDiffusion realModeToWalletMode (walletModeToRealMode wallet) diffusion))

walletModeToRealMode :: forall a. PassiveWalletLayer IO -> WalletMode a -> RealMode () a
walletModeToRealMode wallet ma = do
    rmc <- ask
    let env = WalletContext {
                  wcWallet          = wallet
                , wcRealModeContext = rmc
                }
    lift $ runReaderT ma env

realModeToWalletMode :: RealMode () a -> WalletMode a
realModeToWalletMode rm = ask >>= \ctx ->
  lift (runReaderT rm (wcRealModeContext ctx))

{-------------------------------------------------------------------------------
  'WalletContext' instances

  These all just piggy-back on the 'RealModeContext'.
-------------------------------------------------------------------------------}

instance HasNodeType WalletContext where
  getNodeType = getNodeType . wcRealModeContext

instance HasSlottingVar WalletContext where
  slottingTimestamp = wcRealModeContext_L . slottingTimestamp
  slottingVar       = wcRealModeContext_L . slottingVar

instance HasPrimaryKey WalletContext where
  primaryKey        = wcRealModeContext_L . primaryKey

instance MonadReporting WalletMode where
  report ct = ask >>= \ctx ->
    liftIO (runReporter (rmcReporter (wcRealModeContext ctx)) ct)

instance HasMisbehaviorMetrics WalletContext where
  misbehaviorMetrics = wcRealModeContext_L . misbehaviorMetrics

instance HasSlogGState WalletContext where
  slogGState        = wcRealModeContext_L . slogGState

instance HasSlogContext WalletContext where
  slogContext       = wcRealModeContext_L . slogContext

instance HasShutdownContext WalletContext where
  shutdownContext   = wcRealModeContext_L . shutdownContext

instance HasJsonLogConfig WalletContext where
  jsonLogConfig     = wcRealModeContext_L . jsonLogConfig

instance HasSscContext WalletContext where
  sscContext        = wcRealModeContext_L . sscContext

instance {-# OVERLAPPABLE #-}
    HasLens tag (RealModeContext EmptyMempoolExt) r =>
    HasLens tag WalletContext r
  where
    lensOf = wcRealModeContext_L . lensOf @tag

{-------------------------------------------------------------------------------
  WalletMode instances that just piggy-back on existing infrastructure

  These are all modelled after the instances for the (legacy) 'WalletWebMode'
-------------------------------------------------------------------------------}

type instance MempoolExt WalletMode = EmptyMempoolExt

instance HasConfiguration => MonadDBRead WalletMode where
  dbGet         = dbGetDefault
  dbIterSource  = dbIterSourceDefault
  dbGetSerBlock = dbGetSerBlockRealDefault
  dbGetSerUndo  = dbGetSerUndoRealDefault

instance HasConfiguration => MonadDB WalletMode where
  dbPut         = dbPutDefault
  dbWriteBatch  = dbWriteBatchDefault
  dbDelete      = dbDeleteDefault
  dbPutSerBlunds = dbPutSerBlundsRealDefault

instance ( HasConfiguration
         , MonadSlotsData ctx WalletMode
         ) => MonadSlots ctx WalletMode where
  getCurrentSlot           = getCurrentSlotSimple
  getCurrentSlotBlocking   = getCurrentSlotBlockingSimple
  getCurrentSlotInaccurate = getCurrentSlotInaccurateSimple
  currentTimeSlotting      = currentTimeSlottingSimple

instance HasConfiguration => MonadGState WalletMode where
  gsAdoptedBVData = gsAdoptedBVDataDefault

instance {-# OVERLAPPING #-} CanJsonLog WalletMode where
  jsonLog = jsonLogDefault

instance HasConfiguration
      => MonadTxpLocal WalletMode where
  txpNormalize = txNormalize
  txpProcessTx = txProcessTransaction
