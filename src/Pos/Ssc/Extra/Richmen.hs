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
import           Pos.Lrc.Types        (RichmenStake)
import           Pos.Types            (EpochIndex)

-- | Encapsulation manipulation of SSC richmen.
-- We store SSC richmen into database and inmemory.
class Monad m => MonadSscRichmen m where
    -- | Non-blocking write. Write value even if another value stored.
    writeSscRichmen   :: (EpochIndex, RichmenStake) -> m ()
    -- | Wait data for the specified epoch and return it
    readSscRichmen    :: EpochIndex -> m RichmenStake
    -- | Non-blocking tryRead, returns Just if data is presented
    -- Nothing otherwise.
    tryReadSscRichmen :: m (Maybe (EpochIndex, RichmenStake))

    default readSscRichmen :: (MonadTrans t, MonadSscRichmen m', t m' ~ m) => EpochIndex -> m RichmenStake
    readSscRichmen = lift . readSscRichmen

    default writeSscRichmen :: (MonadTrans t, MonadSscRichmen m', t m' ~ m) => (EpochIndex, RichmenStake) -> m ()
    writeSscRichmen = lift . writeSscRichmen

    default tryReadSscRichmen :: (MonadTrans t, MonadSscRichmen m', t m' ~ m) => m (Maybe (EpochIndex, RichmenStake))
    tryReadSscRichmen = lift tryReadSscRichmen

-- | Check that SSC richmen cache empty
isEmptySscRichmen :: MonadSscRichmen m => m Bool
isEmptySscRichmen = isJust <$> tryReadSscRichmen

-- | Returns Just if presented data corresponds to the specified epoch.
tryReadSscRichmenEpoch :: MonadSscRichmen m => EpochIndex -> m (Maybe RichmenStake)
tryReadSscRichmenEpoch epoch = do
    dt <- tryReadSscRichmen
    pure $ maybe Nothing (\(e, l) -> if e == epoch then Just l else Nothing) dt

instance MonadSscRichmen m => MonadSscRichmen (ReaderT e m) where
instance MonadSscRichmen m => MonadSscRichmen (ExceptT e m) where
instance MonadSscRichmen m => MonadSscRichmen (ResponseT e m) where
instance MonadSscRichmen m => MonadSscRichmen (DHTResponseT s m) where
instance MonadSscRichmen m => MonadSscRichmen (KademliaDHT m) where
