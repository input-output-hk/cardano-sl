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

-------------------------------------------------------------------------------------
-- Explorer mock mode
--
-- The simple data structure that encapsulates functions that use CSL. We want to "cut"
-- them out of the picture in order to be able to mock them.
-------------------------------------------------------------------------------------

-- TODO(KS): A reader `ReaderT (ExplorerMockMode m ssc) m a` would be convenient.
-- | A simple data structure that holds all the foreign functions Explorer needs to call.
-- `emm`.
{-data ExplorerMockMode m ssc = ExplorerMockMode
    { emmGetTipBlock
          :: MonadBlockDB ssc m
          => m (Block ssc)
    , emmGetPageBlocks
          :: MonadDBRead m
          => Page
          -> m (Maybe [HeaderHash])
    , emmGetBlundFromHH
          :: MonadBlockDB ssc m
          => HeaderHash
          -> m (Maybe (Blund ssc))
    , emmGetSlotStart
          :: forall ctx. MonadSlotsData ctx m
          => SlotId
          -> m (Maybe Timestamp)
    , emmGetLeadersFromEpoch
          :: MonadDBRead m
          => EpochIndex
          -> m (Maybe SlotLeaders)
    }

-- | This is what we use in production when we run Explorer.
prodMode :: forall m. ExplorerMockMode m SscGodTossing
prodMode = ExplorerMockMode {
      emmGetTipBlock            = getTipBlock,
      emmGetPageBlocks          = getPageBlocks,
      emmGetBlundFromHH         = blkGetBlund,
      emmGetSlotStart           = getSlotStart,
      emmGetLeadersFromEpoch    = getLeaders
    }

-- | So we can just reuse the default instance and change individial functions.
-- On one side, it removes the compile error(s) for having all functions implemented.
-- On the other side, it moves that error into runtime and enables simple mocking.
-- This is a good thing once we have a larger amount of functions, like in _explorer_,
-- and this gives us the flexibility to "mock" whichever we want.
instance Default (ExplorerMockMode m SscGodTossing) where
  def = ExplorerMockMode {
        emmGetTipBlock            = errorImpl,
        emmGetPageBlocks          = errorImpl,
        emmGetBlundFromHH         = errorImpl,
        emmGetSlotStart           = errorImpl,
        emmGetLeadersFromEpoch    = errorImpl
      }
    where
      errorImpl = error "Cannot be used, please implement this function!"
-}

-- External interface
-- TODO(ks): Add `ExplorerMode`.

-- class HasExplorerMockMode m ssc where
--     getExplorerMockMode :: ExplorerMockMode m SscGodTossing

{--- | We use this for an external CSL functions representation so we can replace them when
-- testing.
class HasExplorerCSLInterface ctx m ssc where
    emmGetTipBlock
          :: MonadBlockDB ssc m
          => m (Block ssc)
    emmGetPageBlocks
          :: MonadDBRead m
          => Page
          -> m (Maybe [HeaderHash])
    emmGetBlundFromHH
          :: MonadBlockDB ssc m
          => HeaderHash
          -> m (Maybe (Blund ssc))
    emmGetSlotStart
          :: MonadSlotsData ctx m
          => SlotId
          -> m (Maybe Timestamp)
    emmGetLeadersFromEpoch
          :: MonadDBRead m
          => EpochIndex
          -> m (Maybe SlotLeaders)

instance Monad m => HasExplorerCSLInterface ctx m ssc where
    emmGetTipBlock = getTipBlock
    emmGetPageBlocks = getPageBlocks
    emmGetBlundFromHH = blkGetBlund
    emmGetSlotStart = getSlotStart
    emmGetLeadersFromEpoch = getLeaders-}



