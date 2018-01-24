{-# LANGUAGE TypeFamilies #-}

module Pos.Explorer.ExplorerMode
    ( -- Explorer
      ExplorerMode
    ) where

import           Universum

import           Pos.DB.Class (MonadDBRead)
import           Pos.Slotting (MonadSlots (..))
import           Pos.Txp (MempoolExt, MonadTxpMem)

import           Pos.Explorer.ExtraContext (HasExplorerCSLInterface, HasGenesisRedeemAddressInfo)

-- Need Emulation because it has instance Mockable CurrentTime
import           Pos.WorkMode (MinWorkMode)


-------------------------------------------------------------------------------------
-- Explorer mode
-------------------------------------------------------------------------------------

-- | We require much less then @WorkMode@, and this simplifies things later when
-- testing (and running).
type ExplorerMode ctx m =
    ( MonadDBRead m
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
    -- ^ Genesis operations
    , MonadTxpMem (MempoolExt m) ctx m
    -- ^ Txp, could be @TxpLocalWorkMode@
    , MinWorkMode m
    -- ^ The rest of the constraints - logger, mockable, configurations
    )
