module Cardano.Wallet.API.Development.Handlers
    ( handlers
    ) where

import           Universum

import           Cardano.Wallet.Server.CLI (RunMode (..))

import           Servant

import qualified Cardano.Wallet.API.Development as Dev

-- TODO: Add handlers for new wallet
handlers :: RunMode -> Server Dev.API
handlers _ = error "TODO"
