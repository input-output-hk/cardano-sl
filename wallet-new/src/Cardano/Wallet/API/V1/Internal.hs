{-# LANGUAGE DeriveGeneric #-}

-- | This module contains the top level API definition for frontend-related
-- tasks.  The API endpoints presented here are intended for use with the
-- Daedalus client, and aren't useful for wallets, exchanges, and other users.
module Cardano.Wallet.API.V1.Internal where

import           Servant

import qualified Cardano.Wallet.API.V1.Internal.Update as Update

type API =
    "internal"
        :> Update.API
