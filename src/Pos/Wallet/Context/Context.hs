-- | Runtime context of wallet

module Pos.Wallet.Context.Context
       ( WalletContext (..)
       , fromNodeCtx
       ) where

import qualified Control.Concurrent.STM as STM
import           Data.Time.Units        (Microsecond)

import           Pos.Context            (NodeContext (..))
import           Pos.Types              (SlotId, Timestamp)

data WalletContext = WalletContext
    { wcSystemStart  :: !Timestamp -- ^ Time when system started working
    , wcSlotDuration :: !Microsecond

    , wcNtpData      :: !(STM.TVar (Microsecond, Microsecond))
    , wcNtpLastSlot  :: !(STM.TVar SlotId)
    }

-- ctxFromParams :: WalletParams -> WalletContext
-- ctxFromParams WalletParams {..} = WalletContext
--     { wcSystemStart = wpSystemStart
--     }

fromNodeCtx :: NodeContext ssc -> WalletContext
fromNodeCtx NodeContext {..} = WalletContext
    { wcSystemStart = ncSystemStart
    , wcSlotDuration = ncSlotDuration
    , wcNtpData = ncNtpData
    , wcNtpLastSlot = ncNtpLastSlot
    }
