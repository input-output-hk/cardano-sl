{-# LANGUAGE TypeFamilies #-}

module Pos.Ssc.Extra.Richmen
       (
         MonadSscRichmen (..)
       ) where

import           Control.Monad.Except (ExceptT)
import           Control.Monad.Trans  (MonadTrans)
import           Control.TimeWarp.Rpc (ResponseT)
import           Universum

import           Pos.DHT.Model.Class  (DHTResponseT)
import           Pos.DHT.Real         (KademliaDHT)
import           Pos.Types            (RichmenStake)

class Monad m => MonadSscRichmen m where
    readSscRichmen    :: m RichmenStake
    writeSscRichmen   :: RichmenStake -> m ()
    isEmptySscRichmen :: m Bool
    clearSscRichmen   :: m Bool -- True if it wat not empty

    default readSscRichmen :: (MonadTrans t, MonadSscRichmen m', t m' ~ m) => m RichmenStake
    readSscRichmen = lift readSscRichmen

    default writeSscRichmen :: (MonadTrans t, MonadSscRichmen m', t m' ~ m) => RichmenStake -> m ()
    writeSscRichmen = lift . writeSscRichmen

    default isEmptySscRichmen :: (MonadTrans t, MonadSscRichmen m', t m' ~ m) => m Bool
    isEmptySscRichmen = lift isEmptySscRichmen

    default clearSscRichmen :: (MonadTrans t, MonadSscRichmen m', t m' ~ m) => m Bool
    clearSscRichmen = lift clearSscRichmen

instance MonadSscRichmen m => MonadSscRichmen (ReaderT e m) where
instance MonadSscRichmen m => MonadSscRichmen (ExceptT e m) where
instance MonadSscRichmen m => MonadSscRichmen (ResponseT e m) where
instance MonadSscRichmen m => MonadSscRichmen (DHTResponseT s m) where
instance MonadSscRichmen m => MonadSscRichmen (KademliaDHT m) where
