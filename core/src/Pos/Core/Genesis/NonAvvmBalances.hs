module Pos.Core.Genesis.NonAvvmBalances
       ( GenesisNonAvvmBalances (..)
       , convertNonAvvmDataToBalances
       ) where

import           Universum

import           Data.Semigroup ()

import           Control.Monad.Except (MonadError (throwError))
import qualified Data.Aeson as Aeson (FromJSON (..), ToJSON (..))
import qualified Data.HashMap.Strict as HM
import           Formatting (bprint, (%))
import qualified Formatting.Buildable as Buildable
import           Serokell.Util (mapJson)
import           Text.JSON.Canonical (FromJSON (..), ReportSchemaErrors,
                     ToJSON (..))

import           Pos.Core.Common (Address, Coin, decodeTextAddress,
                     unsafeAddCoin, unsafeGetCoin, unsafeIntegerToCoin)
import           Pos.Core.Genesis.Canonical ()
import           Pos.Util.Util (toAesonError)


-- | Predefined balances of non avvm entries.
newtype GenesisNonAvvmBalances = GenesisNonAvvmBalances
    { getGenesisNonAvvmBalances :: HashMap Address Coin
    } deriving (Show, Eq)

instance (Hashable Address) =>
         Buildable GenesisNonAvvmBalances where
    build (GenesisNonAvvmBalances m) =
        bprint ("GenesisNonAvvmBalances: " %mapJson) m

deriving instance Hashable Address => Semigroup GenesisNonAvvmBalances
deriving instance Hashable Address => Monoid GenesisNonAvvmBalances

instance Monad m => ToJSON m GenesisNonAvvmBalances where
    toJSON = toJSON . getGenesisNonAvvmBalances

instance ReportSchemaErrors m => FromJSON m GenesisNonAvvmBalances where
    fromJSON = fmap GenesisNonAvvmBalances . fromJSON

instance Aeson.ToJSON GenesisNonAvvmBalances where
    toJSON = Aeson.toJSON . convert . getGenesisNonAvvmBalances
      where
        convert :: HashMap Address Coin -> HashMap Text Integer
        convert = HM.fromList . map f . HM.toList
        f :: (Address, Coin) -> (Text, Integer)
        f = bimap pretty (toInteger . unsafeGetCoin)

instance Aeson.FromJSON GenesisNonAvvmBalances where
    parseJSON = toAesonError . convertNonAvvmDataToBalances <=< Aeson.parseJSON

-- | Generate genesis address distribution out of avvm
-- parameters. Txdistr of the utxo is all empty. Redelegate it in
-- calling funciton.
convertNonAvvmDataToBalances
    :: forall m .
       ( MonadError Text m )
    => HashMap Text Integer
    -> m GenesisNonAvvmBalances
convertNonAvvmDataToBalances balances = GenesisNonAvvmBalances <$> balances'
  where
    balances' :: m (HashMap Address Coin)
    balances' = HM.fromListWith unsafeAddCoin <$> traverse convert (HM.toList balances)
    convert :: (Text, Integer) -> m (Address, Coin)
    convert (txt, i) = do
        addr <- either throwError pure $ decodeTextAddress txt
        return (addr, unsafeIntegerToCoin i)
