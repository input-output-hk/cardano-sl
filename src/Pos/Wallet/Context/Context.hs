-- | Runtime context of wallet

module Pos.Wallet.Context.Context
       ( WalletContext (..)
       , fromNodeCtx
       ) where

import qualified Control.Concurrent.STM    as STM
import           Data.Time.Units           (Microsecond)

import           Pos.Context               (NodeContext (..))
import           Pos.Types                 (Timestamp, SlotId)

data WalletContext = WalletContext
    { wcSystemStart :: !Timestamp -- ^ Time when system started working

    , wcNtpData     :: !(STM.TVar (Microsecond, Microsecond))
    , wcNtpLastSlot :: !(STM.TVar SlotId)
    }

-- ctxFromParams :: WalletParams -> WalletContext
-- ctxFromParams WalletParams {..} = WalletContext
--     { wcSystemStart = wpSystemStart
--     }

fromNodeCtx :: NodeContext ssc -> WalletContext
fromNodeCtx NodeContext {..} = WalletContext
    { wcSystemStart = ncSystemStart
    , wcNtpData = ncNtpData
    , wcNtpLastSlot = ncNtpLastSlot
    }
