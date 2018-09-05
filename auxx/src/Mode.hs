{-# LANGUAGE TypeFamilies #-}

-- | Execution mode used in Auxx.

module Mode
       (
       -- * Mode, context, etc.
         AuxxContext (..)
       , AuxxMode
       , MonadAuxxMode

       -- * Helpers
       , isTempDbUsed
       , realModeToAuxx
       , makePubKeyAddressAuxx
       , deriveHDAddressAuxx

       -- * Specialisations of utils
       , getOwnUtxos
       , getBalance
       ) where

import           Universum

import           Control.Lens (lens, makeLensesWith)
import           Control.Monad.Reader (withReaderT)
import           Control.Monad.Trans.Resource (transResourceT)
import           Data.Conduit (transPipe)

import           Pos.Chain.Block (HasSlogContext (..), HasSlogGState (..))
import           Pos.Chain.Ssc (HasSscContext (..))
import           Pos.Client.KeyStorage (MonadKeys (..), MonadKeysRead (..),
                     getPublicDefault, getSecretDefault, modifyPublicDefault,
                     modifySecretDefault)
import           Pos.Client.Txp.Addresses (MonadAddresses (..))
import           Pos.Client.Txp.Balances (MonadBalances (..),
                     getBalanceFromUtxo, getOwnUtxosGenesis)
import           Pos.Client.Txp.History (MonadTxHistory (..),
                     getBlockHistoryDefault, getLocalHistoryDefault,
                     saveTxDefault)
import           Pos.Context (HasNodeContext (..))
import           Pos.Core (Address, HasPrimaryKey (..), IsBootstrapEraAddr (..),
                     SlotCount, deriveFirstHDAddress, largestPubKeyAddressBoot,
                     largestPubKeyAddressSingleKey, makePubKeyAddress, siEpoch)
import           Pos.Core.JsonLog (CanJsonLog (..))
import           Pos.Core.Reporting (HasMisbehaviorMetrics (..),
                     MonadReporting (..))
import           Pos.Core.Slotting (HasSlottingVar (..), MonadSlotsData)
import           Pos.Crypto (EncryptedSecretKey, PublicKey, emptyPassphrase)
import           Pos.DB (DBSum (..), MonadGState (..), NodeDBs,
                     gsIsBootstrapEra)
import           Pos.DB.Block (MonadBListener (..))
import           Pos.DB.Class (MonadDB (..), MonadDBRead (..))
import           Pos.DB.Txp (MempoolExt, MonadTxpLocal (..), txNormalize,
                     txProcessTransaction, txProcessTransactionNoLock)
import           Pos.DB.Txp.Utxo (getFilteredUtxo)
import           Pos.Generator.Block (BlockGenMode)
import           Pos.GState (HasGStateContext (..), getGStateImplicit)
import           Pos.Infra.Network.Types (HasNodeType (..), NodeType (..))
import           Pos.Infra.Shutdown (HasShutdownContext (..))
import           Pos.Infra.Slotting.Class (MonadSlots (..))
import           Pos.Infra.Util.JsonLog.Events (HasJsonLogConfig (..))
import           Pos.Launcher (HasConfigurations)
import           Pos.Util (HasLens (..), postfixLFields)
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)
import           Pos.Util.LoggerName (HasLoggerName' (..))
import           Pos.Util.UserPublic (HasUserPublic (..))
import           Pos.Util.UserSecret (HasUserSecret (..))
import           Pos.Util.Wlog (HasLoggerName (..))
import           Pos.WorkMode (EmptyMempoolExt, RealMode, RealModeContext (..))

type AuxxMode = ReaderT AuxxContext IO

class (m ~ AuxxMode, HasConfigurations, HasCompileInfo) => MonadAuxxMode m
instance (HasConfigurations, HasCompileInfo) => MonadAuxxMode AuxxMode

data AuxxContext = AuxxContext
    { acRealModeContext :: !(RealModeContext EmptyMempoolExt)
    , acTempDbUsed      :: !Bool
    }

makeLensesWith postfixLFields ''AuxxContext

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

isTempDbUsed :: AuxxMode Bool
isTempDbUsed = view acTempDbUsed_L

-- | Turn 'RealMode' action into 'AuxxMode' action.
realModeToAuxx :: RealMode EmptyMempoolExt a -> AuxxMode a
realModeToAuxx = withReaderT acRealModeContext

----------------------------------------------------------------------------
-- Boilerplate instances
----------------------------------------------------------------------------

-- hacky instance needed to make blockgen work
instance HasLens DBSum AuxxContext DBSum where
    lensOf =
        let getter ctx = RealDB (ctx ^. (lensOf @NodeDBs))
            setter ctx (RealDB db') = ctx & (lensOf @NodeDBs) .~ db'
            setter _ (PureDB _)     = error "Auxx: tried to set pure db insteaf of nodedb"
        in lens getter setter

instance HasGStateContext AuxxContext where
    gStateContext = getGStateImplicit

instance HasSscContext AuxxContext where
    sscContext = acRealModeContext_L . sscContext

instance HasPrimaryKey AuxxContext where
    primaryKey = acRealModeContext_L . primaryKey

-- | Ignore reports.
-- FIXME it's a bad sign that we even need this instance.
-- The pieces of the software which the block generator uses should never
-- even try to report.
instance MonadReporting AuxxMode where
    report _ = pure ()

-- | Ignore reports.
-- FIXME it's a bad sign that we even need this instance.
instance HasMisbehaviorMetrics AuxxContext where
    misbehaviorMetrics = lens (const Nothing) const

instance HasUserPublic AuxxContext where
    userPublic = acRealModeContext_L . userPublic

instance HasUserSecret AuxxContext where
    userSecret = acRealModeContext_L . userSecret

instance HasShutdownContext AuxxContext where
    shutdownContext = acRealModeContext_L . shutdownContext

instance HasNodeContext AuxxContext where
    nodeContext = acRealModeContext_L . nodeContext

instance HasSlottingVar AuxxContext where
    slottingTimestamp = acRealModeContext_L . slottingTimestamp
    slottingVar = acRealModeContext_L . slottingVar

instance HasNodeType AuxxContext where
    getNodeType _ = NodeEdge

instance {-# OVERLAPPABLE #-}
    HasLens tag (RealModeContext EmptyMempoolExt) r =>
    HasLens tag AuxxContext r
  where
    lensOf = acRealModeContext_L . lensOf @tag

instance HasLoggerName' AuxxContext where
    loggerName = acRealModeContext_L . loggerName

instance HasSlogContext AuxxContext where
    slogContext = acRealModeContext_L . slogContext

instance HasSlogGState AuxxContext where
    slogGState = acRealModeContext_L . slogGState

instance HasJsonLogConfig AuxxContext where
    jsonLogConfig = acRealModeContext_L . jsonLogConfig

instance MonadSlotsData ctx AuxxMode => MonadSlots ctx AuxxMode where
    getCurrentSlot = realModeToAuxx . getCurrentSlot
    getCurrentSlotBlocking = realModeToAuxx . getCurrentSlotBlocking
    getCurrentSlotInaccurate = realModeToAuxx . getCurrentSlotInaccurate
    currentTimeSlotting = realModeToAuxx currentTimeSlotting

instance {-# OVERLAPPING #-} HasLoggerName AuxxMode where
    askLoggerName = realModeToAuxx askLoggerName
    modifyLoggerName f action = do
        auxxCtx <- ask
        let auxxToRealMode :: AuxxMode a -> RealMode EmptyMempoolExt a
            auxxToRealMode = withReaderT (\realCtx -> set acRealModeContext_L realCtx auxxCtx)
        realModeToAuxx $ modifyLoggerName f $ auxxToRealMode action

instance {-# OVERLAPPING #-} CanJsonLog AuxxMode where
    jsonLog = realModeToAuxx ... jsonLog

instance MonadDBRead AuxxMode where
    dbGet = realModeToAuxx ... dbGet
    dbIterSource tag p =
        transPipe (transResourceT realModeToAuxx) (dbIterSource tag p)
    dbGetSerBlock = realModeToAuxx ... dbGetSerBlock
    dbGetSerUndo = realModeToAuxx ... dbGetSerUndo
    dbGetSerBlund = realModeToAuxx ... dbGetSerBlund

instance MonadDB AuxxMode where
    dbPut = realModeToAuxx ... dbPut
    dbWriteBatch = realModeToAuxx ... dbWriteBatch
    dbDelete = realModeToAuxx ... dbDelete
    dbPutSerBlunds = realModeToAuxx ... dbPutSerBlunds

instance MonadGState AuxxMode where
    gsAdoptedBVData = realModeToAuxx ... gsAdoptedBVData

instance MonadBListener AuxxMode where
    onApplyBlocks = realModeToAuxx ... onApplyBlocks
    onRollbackBlocks = realModeToAuxx ... onRollbackBlocks

instance MonadBalances AuxxMode where
    getOwnUtxos genesisData addrs = ifM isTempDbUsed
                                        (getOwnUtxosGenesis genesisData addrs)
                                        (getFilteredUtxo addrs)
    getBalance = getBalanceFromUtxo

instance MonadTxHistory AuxxMode where
    getBlockHistory = getBlockHistoryDefault
    getLocalHistory = getLocalHistoryDefault
    saveTx = saveTxDefault

instance (HasConfigurations, HasCompileInfo) =>
         MonadAddresses AuxxMode where
    type AddrData AuxxMode = PublicKey
    getNewAddress = makePubKeyAddressAuxx
    getFakeChangeAddress pc = do
        epochIndex <- siEpoch <$> getCurrentSlotInaccurate pc
        gsIsBootstrapEra epochIndex <&> \case
            False -> largestPubKeyAddressBoot
            True -> largestPubKeyAddressSingleKey

instance MonadKeysRead AuxxMode where
    getPublic = getPublicDefault
    getSecret = getSecretDefault

instance MonadKeys AuxxMode where
    modifyPublic = modifyPublicDefault
    modifySecret = modifySecretDefault

type instance MempoolExt AuxxMode = EmptyMempoolExt

instance MonadTxpLocal AuxxMode where
    txpNormalize pm = withReaderT acRealModeContext . txNormalize pm
    txpProcessTx coreConfig txpConfig = withReaderT acRealModeContext . txProcessTransaction coreConfig txpConfig

instance HasConfigurations =>
         MonadTxpLocal (BlockGenMode EmptyMempoolExt AuxxMode) where
    txpNormalize = withCompileInfo $ txNormalize
    txpProcessTx = withCompileInfo $ txProcessTransactionNoLock

-- | In order to create an 'Address' from a 'PublicKey' we need to
-- choose suitable stake distribution. We want to pick it based on
-- whether we are currently in bootstrap era.
makePubKeyAddressAuxx :: MonadAuxxMode m => SlotCount -> PublicKey -> m Address
makePubKeyAddressAuxx epochSlots pk = do
    epochIndex <- siEpoch <$> getCurrentSlotInaccurate epochSlots
    ibea <- IsBootstrapEraAddr <$> gsIsBootstrapEra epochIndex
    pure $ makePubKeyAddress ibea pk

-- | Similar to @makePubKeyAddressAuxx@ but create HD address.
deriveHDAddressAuxx
    :: MonadAuxxMode m => SlotCount -> EncryptedSecretKey -> m Address
deriveHDAddressAuxx epochSlots hdwSk = do
    epochIndex <- siEpoch <$> getCurrentSlotInaccurate epochSlots
    ibea <- IsBootstrapEraAddr <$> gsIsBootstrapEra epochIndex
    pure $ fst $ fromMaybe (error "makePubKeyHDAddressAuxx: pass mismatch") $
        deriveFirstHDAddress ibea emptyPassphrase hdwSk
