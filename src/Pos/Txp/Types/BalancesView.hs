{-# LANGUAGE TemplateHaskell #-}

module Pos.Txp.Types.BalancesView
       (
         MonadBalances (..)
       , BalancesHolder
       , BalancesView (..)
       ) where

import           Control.Lens        (makeClassy, over, use, uses, (.=))
import           Control.Monad.State (modify)
import qualified Data.HashMap.Strict as HM
import           Universum

import           Pos.DB              (DB)
import           Pos.DB.GState       (getFtsStakeFromDB)
import           Pos.Types           (Coin, StakeholderId)

data BalancesView ssc = BalancesView
    {
      _stakes :: !(HashMap StakeholderId Coin)
    , _total  :: !Coin
    , _balDB  :: !(DB ssc)
    }

makeClassy ''BalancesView

class Monad m => MonadBalances ssc m | m -> ssc where
    setStake :: StakeholderId -> Coin -> m ()
    setTotalStake :: Coin -> m ()
    getStake :: StakeholderId -> m (Maybe Coin)
    getTotalStake :: m Coin

type BalancesHolder ssc = StateT (BalancesView ssc)

instance (MonadIO m, MonadThrow m)
       => MonadBalances ssc (BalancesHolder ssc m) where
    getStake id = do
        db <- use balDB
        res <- uses stakes (HM.lookup id)
        case res of
            Just x  -> pure $ Just x
            Nothing -> lift $ getFtsStakeFromDB id db

    setStake id coin = modify (over stakes (HM.insert id coin))
    -- at lens ?
    getTotalStake = use total
    setTotalStake tot = total .= tot
