{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Storage with node local state.

module Pos.State.Storage
       (
         Storage

       , Update
       , addEntry
       , addLeaders
       , adoptBlock
       , createBlock
       , getLeader
       , getLeaders
       , setLeaders
       ) where

import           Control.Lens    (at, ix, makeLenses, preuse, use, (%=), (.=), (<<.=))
import           Data.Default    (Default, def)
import qualified Data.Set        as Set (fromList, insert, toList, (\\))
import           Universum

import           Serokell.Util   ()

import           Pos.Types.Types (Block, Entry (..), NodeId (..))


data Storage = Storage
    { -- | List of entries that the node has received but that aren't included
      -- into any block yet
      _pendingEntries :: Set Entry
      -- | Leaders for epochs (currently it just stores leaders for all
      -- epochs, but we really only need the leader list for this epoch and
      -- the next epoch)
    , _epochLeaders   :: Map Int [NodeId]
      -- | Blocks
    , _blocks         :: [Block]
    }

makeLenses ''Storage

instance Default Storage where
    def =
        Storage
        { _pendingEntries = mempty
        , _epochLeaders = mempty
        , _blocks = mempty
        }

type Update a = forall m . MonadState Storage m => m a

-- Empty the list of pending entries and create a block
createBlock :: Update Block
createBlock = do
    es <- pendingEntries <<.= mempty
    return (Set.toList es)

addLeaders :: Int -> [NodeId] -> Update ()
addLeaders epoch leaders =
    pendingEntries %= Set.insert (ELeaders (epoch + 1) leaders)

getLeader :: Int -> Int -> Update (Maybe NodeId)
getLeader epoch slot = preuse (epochLeaders . ix epoch . ix slot)

getLeaders :: Int -> Update (Maybe [NodeId])
getLeaders epoch = use (epochLeaders . at epoch)

addEntry :: Entry -> Update ()
addEntry e = pendingEntries %= Set.insert e

adoptBlock :: Block -> Update ()
adoptBlock es = do
    pendingEntries %= (Set.\\ Set.fromList es)
    blocks %= (es :)

setLeaders :: Int -> [NodeId] -> Update ()
setLeaders epoch leaders = epochLeaders . at epoch .= Just leaders
