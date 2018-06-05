module Pos.Core.Genesis.AvvmBalances
       ( GenesisAvvmBalances (..)
       ) where

import           Universum

import           Pos.Core.Common (Coin)
import           Pos.Crypto.Signing (RedeemPublicKey)

-- | Predefined balances of avvm entries.
newtype GenesisAvvmBalances = GenesisAvvmBalances
    { getGenesisAvvmBalances :: HashMap RedeemPublicKey Coin
    } deriving (Show, Eq, Semigroup, Monoid, ToList, Container)

type instance Element GenesisAvvmBalances = Coin
