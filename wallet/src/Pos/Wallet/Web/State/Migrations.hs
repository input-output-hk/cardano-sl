-- | This module contains types used for acid-state migrations
-- that would conflict if declared in the same module.
module Pos.Wallet.Web.State.Migrations (WalletTip(..)) where

import           Universum

import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Pos.Core (HeaderHash)
import           Pos.SafeCopy ()

data WalletTip
    = NotSynced
    | SyncedWith !HeaderHash
    deriving (Eq)

deriveSafeCopySimple 0 'base      ''WalletTip
