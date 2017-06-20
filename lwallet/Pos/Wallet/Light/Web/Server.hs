{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Module for lite-wallet implementation of Daedalus API. We don't
-- maintain it because we don't know whether we will ever use at least
-- 30% of its current state.

module Pos.Wallet.Light.Web.Server
       ( walletServeWebLite
       ) where

import           Universum

import           Pos.Communication.Protocol (SendActions)
import           Pos.Wallet.Light.Mode      (LightWalletMode)

walletServeWebLite
    :: SendActions LightWalletMode
    -> FilePath
    -> Bool
    -> Word16
    -> LightWalletMode ()
walletServeWebLite __sendActions __dbPath __dbRebuild __port =
    error "lite wallet's web server is not implemented, sorry about that"
