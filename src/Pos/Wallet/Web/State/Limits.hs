{-# LANGUAGE UndecidableInstances #-}

-- | Keeps instance of `MonadDBLimits`.

module Pos.Wallet.Web.State.Limits () where

import           Universum

import qualified Pos.Constants               as Const
import           Pos.DB.Limits               (MonadDBLimits (..))
import           Pos.Wallet.Web.State.Holder (WalletWebDB)

-- TODO [CSL-803]: don't rely on constants
instance MonadIO m => MonadDBLimits (WalletWebDB m) where
    getMaxBlockSize = return $ fromIntegral Const.genesisMaxBlockSize
