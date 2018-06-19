{-# LANGUAGE TemplateHaskell #-}
module Data.Acid.Replication where

import Data.Acid.Local
import Data.Acid.Core
import Data.Acid.Log
import Data.Acid.TemplateHaskell
import Data.Acid.Abstract

import System.FilePath

import Data.Map (Map)
import qualified Data.Map as Map

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.ByteString as Strict

import qualified System.ZMQ as ZMQ
import Control.Concurrent
import Data.SafeCopy

type NodeId = String
type OrderCounter = Int
type SerializeCounter = Int

data NodeMode = Booting | Running

data ReplicatedState st =
  ReplicatedState { repLocalState :: AcidState st
                  , repBroadcast :: ZMQ.Socket ZMQ.Pub
                  , repUnicast :: Map NodeId (ZMQ.Socket ZMQ.Pub)
                  , repReceive :: ZMQ.Socket ZMQ.Sub
                  , repNodeModes :: Map NodeId NodeMode
                  , repOrderingQueue :: IntMap Strict.ByteString
                  , repOrderedMsg :: Chan Strict.ByteString
                  , repNodeOrderCounter :: Map NodeId OrderCounter
                  , repNodeSerializeCounter :: Map NodeId SerializeCounter
                  }

-- Never forget: NodeState is not consistent across nodes.
data NodeState = NodeState { nodeGuaranteedSafe :: EntryId
                           , nodeMembership     :: Set NodeId
                           }
instance SafeCopy NodeState where
  getCopy = undefined
  putCopy = undefined

makeAcidic ''NodeState []

openReplicatedState' port stateDirectory stateTag initialState
  = do ctx <- ZMQ.init 1
       clusterState <- openLocalStateFrom "cluster/" (NodeState 0 Set.empty)
       broadcast <- ZMQ.socket ctx ZMQ.Pub
       ZMQ.bind broadcast ("tcp://*:" ++ show port)
       receive <- ZMQ.socket ctx ZMQ.Sub
       undefined
