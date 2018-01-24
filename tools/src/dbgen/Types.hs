{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RankNTypes       #-}
module Types where

import           Prelude
import           Data.Map            (Map)
import           Pos.Wallet.Web.Mode

type UberMonad a = MonadWalletWebMode WalletWebMode => WalletWebMode a

data Method  =
  GetWallets
  deriving (Show, Read)

type Methods = MonadWalletWebMode WalletWebMode => Map Method (WalletWebMode ())

