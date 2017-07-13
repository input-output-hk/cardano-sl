-- | Different utils for wallets

module Pos.Wallet.Web.Util
    ( getWalletAccountIds
    ) where

import           Universum

import           Pos.Wallet.Web.ClientTypes (AccountId (..), CId, Wal)
import           Pos.Wallet.Web.State       (WebWalletModeDB, getWAddressIds)

-- TODO: move more here from Methods.hs

getWalletAccountIds :: WebWalletModeDB m => CId Wal -> m [AccountId]
getWalletAccountIds cWalId = filter ((== cWalId) . aiWId) <$> getWAddressIds
