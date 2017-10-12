-- Classes and datatypes for managing extra context in Explorer.

module Pos.Explorer.ExtraContext
    ( ExtraContext
    , ExtraContextT
    , runExtraContextT
    , makeExtraCtx

    , HasGenesisRedeemAddressInfo (..)
    , GenesisRedeemAddressInfo
    ) where

import           Universum

import qualified Data.Vector as V
import qualified Ether

import           Pos.Core    (Address, Coin, HasConfiguration, isRedeemAddress)
import           Pos.Txp     (GenesisUtxo (..), genesisUtxo, utxoToAddressCoinPairs)

type ExtraContextT m = Ether.ReaderT' ExtraContext m

runExtraContextT :: Monad m => ExtraContext -> ExtraContextT m a -> m a
runExtraContextT = flip Ether.runReaderT

type GenesisRedeemAddressInfo = V.Vector (Address, Coin)

newtype ExtraContext = ExtraContext
    { ecAddressCoinPairs :: GenesisRedeemAddressInfo
    } deriving (Eq, Show)

class HasGenesisRedeemAddressInfo m where
    getGenesisRedeemAddressInfo :: m GenesisRedeemAddressInfo

instance Monad m => HasGenesisRedeemAddressInfo (ExtraContextT m) where
    getGenesisRedeemAddressInfo = do
        extraCtx <- Ether.ask @ExtraContext
        pure $ ecAddressCoinPairs extraCtx

makeExtraCtx :: HasConfiguration => ExtraContext
makeExtraCtx =
    let addressCoinPairs = utxoToAddressCoinPairs $ unGenesisUtxo genesisUtxo
        redeemOnly = filter (isRedeemAddress . fst) addressCoinPairs
    in ExtraContext $ V.fromList redeemOnly
