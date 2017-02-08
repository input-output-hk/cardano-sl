-- | Runtime context of wallet

module Pos.Wallet.Context.Context
       ( WalletContext (..)
       , fromNodeCtx
       ) where

import qualified Control.Concurrent.STM as STM

import           Pos.Context            (NodeContext (..), npSystemStart)
import           Pos.Slotting           (SlottingState)
import           Pos.Types              (Timestamp)

data WalletContext = WalletContext
    { wcSystemStart   :: !Timestamp -- ^ Time when system started working
    , wcSlottingState :: !(STM.TVar SlottingState)
    }

-- ctxFromParams :: WalletParams -> WalletContext
-- ctxFromParams WalletParams {..} = WalletContext
--     { wcSystemStart = wpSystemStart
--     }

fromNodeCtx :: NodeContext ssc -> WalletContext
fromNodeCtx NodeContext {..} = WalletContext
    { wcSystemStart = npSystemStart ncNodeParams
    , wcSlottingState = ncSlottingState
    }
