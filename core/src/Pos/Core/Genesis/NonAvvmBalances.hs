module Pos.Core.Genesis.NonAvvmBalances
       ( GenesisNonAvvmBalances (..)
       , convertNonAvvmDataToBalances
       ) where

import           Universum

import           Control.Monad.Except (MonadError (throwError))
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Buildable as Buildable
import           Formatting (bprint, (%))
import           Serokell.Util (mapJson)

import           Pos.Core.Common (Address, Coin, decodeTextAddress, unsafeAddCoin,
                                  unsafeIntegerToCoin)

-- | Predefined balances of non avvm entries.
newtype GenesisNonAvvmBalances = GenesisNonAvvmBalances
    { getGenesisNonAvvmBalances :: HashMap Address Coin
    } deriving (Show, Eq)

instance (Hashable Address) =>
         Buildable GenesisNonAvvmBalances where
    build (GenesisNonAvvmBalances m) =
        bprint ("GenesisNonAvvmBalances: " %mapJson) m

deriving instance Hashable Address => Monoid GenesisNonAvvmBalances

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
