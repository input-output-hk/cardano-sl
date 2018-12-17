{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Type classes instances that exist just for the sake of the legacy handlers
--
-- NOTE:
--
-- * These should be removed once the legacy handlers are removed
-- * These not NOT be in scope anywhere else!

module Cardano.Wallet.API.V1.LegacyHandlers.Instances () where

import qualified Data.IxSet.Typed as IxSet

import qualified Pos.Core as Core

import           Cardano.Wallet.API.Indices
import           Cardano.Wallet.API.V1.Types
import           Cardano.Wallet.Kernel.DB.Util.IxSet (OrdByPrimKey, ixList)

instance IxSet.Indexable (V1 Core.Address ': SecondaryWalletAddressIxs)
                         (OrdByPrimKey WalletAddress) where
    indices = ixList
