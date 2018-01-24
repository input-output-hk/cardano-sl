{-# LANGUAGE RankNTypes #-}
-- | This module contains higher level transctions atop of
--   'Post.Wallet.Web.State.Storage'. These are defined as
--   specific (named) functions in order to generate acidic
--   guarantees for them.
module Pos.Wallet.Web.State.Transactions
  ( createAccountWithAddress )
  where

import           Pos.Wallet.Web.ClientTypes   (AccountId (..), CAccountMeta,
                                               CWAddressMeta (..))
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
