-- | Assurance levels info.

module Pos.Wallet.Web.Assurance
    ( AssuranceLevel (..)
    , assuredBlockDepth
    ) where

import           Pos.Core.Common (BlockCount)
import           Pos.Wallet.Web.ClientTypes (CWalletAssurance (..))

data AssuranceLevel
    = HighAssurance

-- | For given assurance level, recommended transaction depth to assure that
-- transaction won't be canceled by some fork.
--
-- Values are taken from this table:
-- https://cardanodocs.com/cardano/transaction-assurance/
assuredBlockDepth :: AssuranceLevel -> CWalletAssurance -> BlockCount
assuredBlockDepth HighAssurance CWANormal = 9
assuredBlockDepth HighAssurance CWAStrict = 15
