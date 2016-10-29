{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
module Pos.State.Storage.Stats
       ( StatsData
       , HasStatsData (statsData)
       , IdTimestamp (..)
       , getStatRecords
       , addStatRecord
       ) where

import           Control.Lens        (at, makeClassy, use, view, (?=))
import           Data.Default        (Default, def)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe          (maybeToList)
import           Data.SafeCopy       (base, deriveSafeCopySimple)
import           Serokell.AcidState  ()
import           Universum

-- TODO: maybe make configurable somehow?
maxRecordsCount :: Int
maxRecordsCount = 1000

data IdTimestamp = IdTimestamp
    { itId        :: !LByteString
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

getStatRecords :: Text -> Query (Maybe [IdTimestamp])
getStatRecords label = view (getStatsData . at label)

-- TODO: it should be doable more elegantly, but I lack lens familiarity
addStatRecord :: Text -> IdTimestamp -> Update ()
addStatRecord label record = do
    mlist <- use $ getStatsData . at label
    getStatsData . at label ?= (take maxRecordsCount . (record :) . concat . maybeToList) mlist
