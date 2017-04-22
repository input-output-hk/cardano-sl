{-# LANGUAGE CPP                  #-}
{-# LANGUAGE TypeFamilies         #-}

-- | Instances of MoandUtxoRead and MonadBalancesRead which use DB.

module Pos.Txp.Toil.DBTxp
       ( DBTxp
       , runDBTxp
       ) where

import           Control.Monad.Trans.Ether.Tagged (TaggedTrans (..))
import           Control.Monad.Trans.Identity     (IdentityT (..))
import           Data.Coerce                      (coerce)
import           Universum

import           Pos.DB.Class                     (MonadDB)
import qualified Pos.DB.GState                    as GS
import           Pos.DB.GState.Balances           (getRealStake, getRealTotalStake)
import           Pos.Update.Core                  (BlockVersionData (..))
import           Pos.Update.DB                    (getAdoptedBVData)

import           Pos.Txp.Toil.Class               (MonadBalancesRead (..),
                                                   MonadToilEnv (..), MonadUtxoRead (..))
import           Pos.Txp.Toil.Types               (ToilEnv (..))

data DBTxpTag

type DBTxp = TaggedTrans DBTxpTag IdentityT

runDBTxp :: DBTxp m a -> m a
runDBTxp = coerce

instance (Monad m, MonadDB m) => MonadUtxoRead (DBTxp m) where
    utxoGet = GS.getTxOut

instance (Monad m, MonadDB m) => MonadBalancesRead (DBTxp m) where
    getTotalStake = getRealTotalStake
    getStake = getRealStake

instance (Monad m, MonadDB m) => MonadToilEnv (DBTxp m) where
    getToilEnv = ToilEnv . bvdMaxTxSize <$> getAdoptedBVData
