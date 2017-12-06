{-# LANGUAGE TypeFamilies #-}

-- | Instances of 'MonadUtxoRead', 'MonadStakesRead' which use DB.

module Pos.Txp.Toil.DBToil
       ( DBToil
       , runDBToil
       ) where

import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Coerce (coerce)
import qualified Ether

import           Pos.DB.Class (MonadDBRead)
import           Pos.DB.GState.Stakes (getRealStake, getRealTotalStake)
import           Pos.Txp.DB.Utxo (getTxOut)
import           Pos.Txp.Toil.Class (MonadStakesRead (..), MonadUtxoRead (..))

data DBToilTag

type DBToil = Ether.TaggedTrans DBToilTag IdentityT

runDBToil :: DBToil m a -> m a
runDBToil = coerce

instance (MonadDBRead m) => MonadUtxoRead (DBToil m) where
    utxoGet = getTxOut

instance (MonadDBRead m) => MonadStakesRead (DBToil m) where
    getTotalStake = getRealTotalStake
    getStake = getRealStake
