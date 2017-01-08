{-# LANGUAGE TypeFamilies #-}

module Pos.Ssc.Extra.Richmen
       (
         MonadSscRichmen (..)
       , isEmptySscRichmen
       , tryReadSscRichmenEpoch
       ) where

import           Control.Monad.Except (ExceptT)
import           Control.Monad.Trans  (MonadTrans)
import           Control.TimeWarp.Rpc (ResponseT)
import           Data.Maybe           (isJust)
import           Universum

import           Pos.DHT.Model.Class  (DHTResponseT)
import           Pos.DHT.Real         (KademliaDHT)
import           Pos.Types            (EpochIndex, RichmenStake)

class Monad m => MonadSscRichmen m where
    writeSscRichmen   :: (EpochIndex, RichmenStake) -> m ()
    readSscRichmen    :: EpochIndex -> m RichmenStake
    tryReadSscRichmen :: m (Maybe (EpochIndex, RichmenStake))

    default readSscRichmen :: (MonadTrans t, MonadSscRichmen m', t m' ~ m) => EpochIndex -> m RichmenStake
    readSscRichmen = lift . readSscRichmen

    default writeSscRichmen :: (MonadTrans t, MonadSscRichmen m', t m' ~ m) => (EpochIndex, RichmenStake) -> m ()
    writeSscRichmen = lift . writeSscRichmen

    default tryReadSscRichmen :: (MonadTrans t, MonadSscRichmen m', t m' ~ m) => m (Maybe (EpochIndex, RichmenStake))
    tryReadSscRichmen = lift tryReadSscRichmen

isEmptySscRichmen :: MonadSscRichmen m => m Bool
isEmptySscRichmen = isJust <$> tryReadSscRichmen

tryReadSscRichmenEpoch :: MonadSscRichmen m => EpochIndex -> m (Maybe RichmenStake)
tryReadSscRichmenEpoch epoch = do
    dt <- tryReadSscRichmen
    pure $ maybe Nothing (\(e, l) -> if e == epoch then Just l else Nothing) dt

instance MonadSscRichmen m => MonadSscRichmen (ReaderT e m) where
instance MonadSscRichmen m => MonadSscRichmen (ExceptT e m) where
instance MonadSscRichmen m => MonadSscRichmen (ResponseT e m) where
instance MonadSscRichmen m => MonadSscRichmen (DHTResponseT s m) where
instance MonadSscRichmen m => MonadSscRichmen (KademliaDHT m) where
