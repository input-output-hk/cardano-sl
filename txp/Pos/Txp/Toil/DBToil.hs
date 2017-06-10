{-# LANGUAGE TypeFamilies #-}

-- | Instances of 'MonadUtxoRead', 'MonadBalancesRead' and
-- 'MonadToilEnv' which use DB.

module Pos.Txp.Toil.DBToil
       ( DBToil
       , runDBToil
       ) where

import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Coerce                  (coerce)
import qualified Ether
import           Universum

import           Pos.Core                     (BlockVersionData (..))
import           Pos.DB.Class                 (MonadDBRead, MonadGState (gsAdoptedBVData))
import           Pos.DB.GState.Balances       (getRealStake, getRealTotalStake)
import           Pos.Txp.DB.Utxo              (getTxOut)
import           Pos.Txp.Toil.Class           (MonadBalancesRead (..), MonadToilEnv (..),
                                               MonadUtxoRead (..))
import           Pos.Txp.Toil.Types           (ToilEnv (..))

data DBToilTag

type DBToil = Ether.TaggedTrans DBToilTag IdentityT

runDBToil :: DBToil m a -> m a
runDBToil = coerce

instance (MonadDBRead m) => MonadUtxoRead (DBToil m) where
    utxoGet = getTxOut

instance (MonadDBRead m) => MonadBalancesRead (DBToil m) where
    getTotalStake = getRealTotalStake
    getStake = getRealStake

instance (MonadGState m) =>
         MonadToilEnv (DBToil m) where
    getToilEnv = constructEnv <$> gsAdoptedBVData
      where
        constructEnv BlockVersionData {..} =
            ToilEnv
            { teMaxBlockSize = bvdMaxBlockSize
            , teMaxTxSize = bvdMaxTxSize
            }
