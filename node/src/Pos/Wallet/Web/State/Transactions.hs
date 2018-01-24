{-# LANGUAGE RankNTypes #-}
-- | This module contains higher level transctions atop of
--   'Post.Wallet.Web.State.Storage'. These are defined as
--   specific (named) functions in order to generate acidic
--   guarantees for them.
module Pos.Wallet.Web.State.Transactions
  ( createAccountWithAddress
  , deleteWallet
  )
  where

import           Universum

import qualified Data.HashMap.Strict          as HM
import           Pos.Wallet.Web.ClientTypes   (AccountId (..), CAccountMeta,
                                               CId, CWAddressMeta (..), Wal)
import           Pos.Wallet.Web.State.Storage (Update)
import qualified Pos.Wallet.Web.State.Storage as WS

-- | Create an account with an address.
createAccountWithAddress
  :: AccountId
  -> CAccountMeta
  -> CWAddressMeta
  -> Update ()
createAccountWithAddress accId accMeta addrMeta = do
  WS.createAccount accId accMeta
  WS.addWAddress addrMeta

-- | Delete a wallet (and all associated data).
--   Compared to the low-level 'removeWallet', this function:
--   - Removes all accounts associated with the wallet.
--   - Removes transaction metadata.
--   - Removes the history cache.
deleteWallet
  :: CId Wal
  -> Update ()
deleteWallet walId = do
  accIds <- filter ((== walId) . aiWId) . HM.keys <$> use WS.wsAccountInfos
  for_ accIds WS.removeAccount
  WS.removeWallet walId
  WS.removeTxMetas walId
  WS.removeHistoryCache walId
