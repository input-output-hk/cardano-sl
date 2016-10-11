{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Acid-state wrapped operations.

module Pos.State.Acidic
       ( DiskState
       , closeState
       , openState
       , openMemState
       , tidyState

       , query
       , update

       , GetLeaders (..)

       , AddTx (..)
       -- , AddLeaders (..)
       -- , GetLeader (..)
       -- , AddEntry (..)
       -- , AdoptBlock (..)
       -- , SetLeaders (..)
       ) where

import           Data.Acid          (EventResult, EventState, Query, QueryEvent, Update,
                                     UpdateEvent, makeAcidic)
import           Data.Default       (def)
import           Serokell.AcidState (ExtendedState, closeExtendedState,
                                     openLocalExtendedState, openMemoryExtendedState,
                                     queryExtended, tidyExtendedState, updateExtended)
import           Universum

import           Pos.Crypto         (PublicKey)
import           Pos.State.Storage  (Storage)
import qualified Pos.State.Storage  as S
import           Pos.Types

----------------------------------------------------------------------------
-- Acid-state things
----------------------------------------------------------------------------

type DiskState = ExtendedState Storage

query
    :: (EventState event ~ Storage, QueryEvent event, MonadIO m)
    => DiskState -> event -> m (EventResult event)
query = queryExtended

update
    :: (EventState event ~ Storage, UpdateEvent event, MonadIO m)
    => DiskState -> event -> m (EventResult event)
update = updateExtended

openState :: MonadIO m => Bool -> FilePath -> m DiskState
openState deleteIfExists fp = openLocalExtendedState deleteIfExists fp def

openMemState :: MonadIO m => m DiskState
openMemState = openMemoryExtendedState def

closeState :: MonadIO m => DiskState -> m ()
closeState = closeExtendedState

tidyState :: MonadIO m => DiskState -> m ()
tidyState = tidyExtendedState

----------------------------------------------------------------------------
-- Redeclarations of operations
----------------------------------------------------------------------------

-- 'makeAcidic' demands that operations use Update and Query from acid-state,
-- even if our Update and Query are supersets of those (i.e. our Update is
-- “MonadState m => m a” and acid-state's Update is an instance of MonadState
-- so it fits). Hence we have to redefine all operations in order to be able
-- to use 'makeAcidic' on them.

getLeaders :: EpochIndex -> Query Storage [PublicKey]
getLeaders = S.getLeaders

addTx :: Tx -> Update Storage Bool
addTx = S.addTx

-- addLeaders :: Int -> [NodeId] -> Update Storage ()
-- addLeaders = S.addLeaders

-- getLeader :: Int -> Int -> Query Storage (Maybe NodeId)
-- getLeader = S.getLeader

-- addEntry :: Entry -> Update Storage ()
-- addEntry = S.addEntry

-- adoptBlock :: Blockkk -> Update Storage ()
-- adoptBlock = S.adoptBlock

-- setLeaders :: Int -> [NodeId] -> Update Storage ()
-- setLeaders = S.setLeaders

makeAcidic ''Storage
    [ 'getLeaders
    , 'addTx
    -- , 'addLeaders
    -- , 'getLeader
    -- , 'addEntry
    -- , 'adoptBlock
    -- , 'setLeaders
    ]
