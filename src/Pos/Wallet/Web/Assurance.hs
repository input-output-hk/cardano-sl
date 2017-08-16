-- | Assurance levels info.

module Pos.Wallet.Web.Assurance
    ( AssuranceLevel (..)
    , assuredBlockDepth
    ) where

import           Pos.Core.Types             (SlotCount)
import           Pos.Wallet.Web.ClientTypes (CWalletAssurance (..))

data AssuranceLevel
    = HighAssurance

-- | For given assurance level, recommended transaction depth to assure that
-- transaction won't be canceled by some fork.
--
-- Values are taken from this table:
-- https://cardanodocs.com/cardano/transaction-assurance/
assuredBlockDepth :: CWalletAssurance -> AssuranceLevel -> SlotCount
assuredBlockDepth CWANormal HighAssurance = 9
assuredBlockDepth CWAStrict HighAssurance = 15
