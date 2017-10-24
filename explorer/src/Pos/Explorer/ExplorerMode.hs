module Pos.Explorer.ExplorerMode
    ( ExplorerMode
    ) where

import           Universum

import           Pos.Explorer.ExtraContext        (HasExplorerCSLInterface,
                                                   HasGenesisRedeemAddressInfo)

import           Pos.DB.Block                     (MonadBlockDB)
import           Pos.DB.Class                     (MonadDBRead)
import           Pos.Slotting                     (MonadSlots (..))
import           Pos.Txp                          (MempoolExt, MonadTxpMem)

import           Control.Monad.Catch              (MonadMask)
import           Pos.DB                           (NodeDBs)
import           Pos.Ssc.GodTossing.Configuration (HasGtConfiguration)
import           Pos.Util                         (HasLens')
import           Pos.WorkMode                     (MinWorkMode)

-------------------------------------------------------------------------------------
-- Explorer mode
-------------------------------------------------------------------------------------

-- | We require much less then @WorkMode@, and this simplifies things later when
-- testing (and running).
type ExplorerMode ctx m =
    ( MonadBlockDB m
    , MonadDBRead m
    -- ^ Database operations
    , MonadSlots ctx m
    -- ^ Slotting
    , MonadThrow m
    , MonadCatch m
    , MonadMask m
    -- ^ General utility operations
    , HasExplorerCSLInterface m
    -- ^ For mocking external functions
    , HasGenesisRedeemAddressInfo m
    , HasGtConfiguration
    -- ^ Genesis operations
    , HasLens' ctx NodeDBs
    , MonadTxpMem (MempoolExt m) ctx m
    -- ^ Database and txp
    , MinWorkMode m
    -- ^ The rest of the constraints - logger, mockable, configurations
    )

