{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
{-# OPTIONS -fno-warn-unused-top-binds #-} -- for lenses

-- | Stack of monads used by light wallet.

module Pos.Wallet.Light.Mode
       ( LightWalletMode
       , LightWalletContext(..)
       ) where

import           Universum

import           Control.Lens                     (makeLensesWith)
import qualified Control.Monad.Reader             as Mtl
import           Ether.Internal                   (HasLens (..))
import           Mockable                         (Production)
import           System.Wlog                      (HasLoggerName (..), LoggerName)

import           Pos.Block.BListener              (MonadBListener (..), onApplyBlocksStub,
                                                   onRollbackBlocksStub)
import           Pos.Client.Txp.Addresses         (MonadAddresses (..))
import           Pos.Client.Txp.Balances          (MonadBalances (..), getBalanceFromUtxo)
import           Pos.Client.Txp.History           (MonadTxHistory (..))
import           Pos.Communication.Types.Protocol (NodeId)
import qualified Pos.Constants                    as Const
import           Pos.Core                         (BlockVersionData (..),
                                                   HasCoreConstants, SlotId (..),
                                                   addressHash, makePubKeyAddress)
import           Pos.Crypto                       (PublicKey)
import           Pos.DB                           (MonadGState (..))
import           Pos.Genesis                      (GenesisWStakeholders)
import           Pos.Reporting.MemState           (ReportingContext)
import           Pos.Slotting                     (HasSlottingVar (..), MonadSlots (..),
                                                   currentTimeSlottingSimple)
import           Pos.Slotting.MemState            (MonadSlotsData)
import           Pos.Ssc.GodTossing               (SscGodTossing)
import           Pos.Txp                          (filterUtxoByAddrs)
import           Pos.Txp.Toil                     (GenesisUtxo (..))
import           Pos.Util.JsonLog                 (HasJsonLogConfig (..), JsonLogConfig,
                                                   jsonLogDefault)
import           Pos.Util.LoggerName              (HasLoggerName' (..),
                                                   getLoggerNameDefault,
                                                   modifyLoggerNameDefault)
import           Pos.Util.TimeWarp                (CanJsonLog (..))
import           Pos.Util.UserSecret              (HasUserSecret (..))
import           Pos.Util.Util                    (postfixLFields)
import           Pos.Wallet.KeyStorage            (KeyData)
import           Pos.Wallet.WalletMode            (MonadBlockchainInfo (..),
                                                   MonadUpdates (..))

type LightWalletSscType = SscGodTossing

data LightWalletContext = LightWalletContext
    { lwcKeyData          :: !KeyData
    , lwcReportingContext :: !ReportingContext
    , lwcDiscoveryPeers   :: !(Set NodeId)
    , lwcJsonLogConfig    :: !JsonLogConfig
    , lwcLoggerName       :: !LoggerName
    , lwcGenStakeholders  :: !GenesisWStakeholders
    , lwcGenesisUtxo      :: !GenesisUtxo
    , lwcExplicitBootEra  :: !Bool
    }

makeLensesWith postfixLFields ''LightWalletContext

type LightWalletMode = Mtl.ReaderT LightWalletContext Production

instance HasUserSecret LightWalletContext where
    userSecret = lwcKeyData_L

instance HasLens GenesisWStakeholders LightWalletContext GenesisWStakeholders where
    lensOf = lwcGenStakeholders_L

instance HasLoggerName' LightWalletContext where
    loggerName = lwcLoggerName_L

instance HasJsonLogConfig LightWalletContext where
    jsonLogConfig = lwcJsonLogConfig_L

instance {-# OVERLAPPING #-} HasLoggerName LightWalletMode where
    getLoggerName = getLoggerNameDefault
    modifyLoggerName = modifyLoggerNameDefault

instance {-# OVERLAPPING #-} CanJsonLog LightWalletMode where
    jsonLog = jsonLogDefault

instance MonadBListener LightWalletMode where
    onApplyBlocks = onApplyBlocksStub
    onRollbackBlocks = onRollbackBlocksStub

-- FIXME: Dummy instance for lite-wallet.
instance HasSlottingVar LightWalletContext where
    slottingTimestamp = error "notImplemented"
    slottingVar       = error "notImplemented"

-- FIXME: Dummy instance for lite-wallet.
instance MonadBlockchainInfo LightWalletMode where
    networkChainDifficulty = error "notImplemented"
    localChainDifficulty   = error "notImplemented"
    blockchainSlotDuration = error "notImplemented"
    connectedPeers         = error "notImplemented"

-- FIXME: Dummy instance for lite-wallet.
instance MonadUpdates LightWalletMode where
    waitForUpdate   = error "notImplemented"
    applyLastUpdate = pure ()

-- FIXME: Dummy instance for lite-wallet.
instance (HasCoreConstants, MonadSlotsData ctx LightWalletMode)
      => MonadSlots ctx LightWalletMode
  where
    getCurrentSlot           = Just <$> getCurrentSlotInaccurate
    getCurrentSlotBlocking   = getCurrentSlotInaccurate
    getCurrentSlotInaccurate = pure (SlotId 0 minBound)
    currentTimeSlotting      = currentTimeSlottingSimple

instance MonadGState LightWalletMode where
    gsAdoptedBVData = do
        explicitBoot <- view lwcExplicitBootEra_L
        -- We set unlock stake epoch to 0 if we're asked explicitly to
        -- be out of the boot epoch. Since 'MonadSlots' instance
        -- defines current slot as hardcoded 0th, it effectively
        -- brings wallet into postboot epoch.
        let alt | explicitBoot = identity
                | otherwise = \BlockVersionData{..} ->
                               BlockVersionData{ bvdUnlockStakeEpoch = 0, ..}
        pure $ alt $ Const.genesisBlockVersionData

instance MonadBalances LightWalletMode where
    getOwnUtxos addrs = filterUtxoByAddrs addrs <$> asks (unGenesisUtxo . lwcGenesisUtxo)
    getBalance = getBalanceFromUtxo

instance HasCoreConstants => MonadTxHistory LightWalletSscType LightWalletMode where
    getBlockHistory = error "getBlockHistory is not implemented for light wallet"
    getLocalHistory = error "getLocalHistory is not implemented for light wallet"
    saveTx _ = pass

instance MonadAddresses LightWalletMode where
    type AddrData LightWalletMode = PublicKey
    getNewAddress pk = pure (makePubKeyAddress pk, Just $ addressHash pk)
