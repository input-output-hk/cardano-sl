module Pos.Core.Genesis.AvvmBalances
       ( GenesisAvvmBalances (..)
       ) where

import           Universum

import qualified Data.Aeson as Aeson (FromJSON (..), ToJSON (..))
import qualified Text.JSON.Canonical as Canonical (FromJSON (..), ReportSchemaErrors,
                     ToJSON (..))

import           Pos.Core.Common (Coin)
import           Pos.Core.Genesis.Canonical ()
import           Pos.Crypto.Signing (RedeemPublicKey)

-- | Predefined balances of avvm entries.
newtype GenesisAvvmBalances = GenesisAvvmBalances
    { getGenesisAvvmBalances :: HashMap RedeemPublicKey Coin
    } deriving (Show, Eq, Semigroup, Monoid, Container)

instance Monad m => Canonical.ToJSON m GenesisAvvmBalances where
    toJSON = Canonical.toJSON . getGenesisAvvmBalances

instance Canonical.ReportSchemaErrors m => Canonical.FromJSON m GenesisAvvmBalances where
    fromJSON = fmap GenesisAvvmBalances . Canonical.fromJSON

deriving instance Aeson.ToJSON GenesisAvvmBalances
deriving instance Aeson.FromJSON GenesisAvvmBalances
