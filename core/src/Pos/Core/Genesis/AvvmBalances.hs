module Pos.Core.Genesis.AvvmBalances
       ( GenesisAvvmBalances (..)
       ) where

import           Universum

import           Text.JSON.Canonical (FromJSON (..), ReportSchemaErrors,
                     ToJSON (..))

import           Pos.Core.Common (Coin)
import           Pos.Core.Genesis.Canonical ()
import           Pos.Crypto.Signing (RedeemPublicKey)

-- | Predefined balances of avvm entries.
newtype GenesisAvvmBalances = GenesisAvvmBalances
    { getGenesisAvvmBalances :: HashMap RedeemPublicKey Coin
    } deriving (Show, Eq, Semigroup, Monoid, Container)

instance Monad m => ToJSON m GenesisAvvmBalances where
    toJSON = toJSON . getGenesisAvvmBalances

instance ReportSchemaErrors m => FromJSON m GenesisAvvmBalances where
    fromJSON = fmap GenesisAvvmBalances . fromJSON
