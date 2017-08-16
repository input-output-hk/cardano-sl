-- | Assurance levels info

module Pos.Wallet.Web.Assurance
    ( highAssurance
    ) where

import           Pos.Core.Types (SlotCount)

highAssurance :: SlotCount
highAssurance = 10
