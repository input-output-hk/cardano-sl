{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}

module Pos.State.Storage.Stats
       ( StatsData
       , HasStatsData (statsData)
       , getStatRecords
       , newStatRecord
       ) where

import           Control.Lens        (at, makeClassy, use, view, (?=))
import           Data.Default        (Default (..))
import qualified Data.HashMap.Strict as HM
import           Data.SafeCopy       (base, deriveSafeCopySimple)
import           Serokell.AcidState  ()
import           Universum

-- TODO: maybe make configurable somehow?
maxRecordsCount :: Int
maxRecordsCount = 3600

-- | Timestamped data with types erased. Types are erased to derive
-- `SafeCopy` instances (and other ones) without a problem.
data IdTimestamp = IdTimestamp
    { itData      :: !LByteString
    , itTimestamp :: !Word64
    } deriving (Show, Eq)

deriveSafeCopySimple 0 'base ''IdTimestamp

newtype StatsData = StatsData
    { _getStatsData :: HashMap Text [IdTimestamp]
    }

makeClassy ''StatsData
deriveSafeCopySimple 0 'base ''StatsData

instance Default StatsData where
    def = StatsData HM.empty

type Query a = forall m x. (HasStatsData x, MonadReader x m) => m a
type Update a = forall m x. (HasStatsData x, MonadState x m) => m a

-- TODO: it should be doable more elegantly, but I lack lens familiarity
getStatRecords :: Text -> Query (Maybe [(Word64, LByteString)])
getStatRecords label = getEntries <$> view (getStatsData . at label)
  where getEntries = fmap $ map toEntry
        toEntry IdTimestamp{..} = (fromIntegral itTimestamp, itData)

newStatRecord :: Text -> Word64 -> LByteString -> Update ()
newStatRecord label ts entry = do
    ds <- use $ getStatsData . at label
    getStatsData . at label ?= update ds
  where update = take maxRecordsCount . (its :) . concat . maybeToList
        its = IdTimestamp entry ts
