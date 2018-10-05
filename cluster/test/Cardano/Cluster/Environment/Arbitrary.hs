{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE TypeOperators     #-}

module Cardano.Cluster.Environment.Arbitrary
    (
    -- * Types
      Cluster (..)
    , Port (..)
    ) where

import           Universum

import           Test.QuickCheck (Arbitrary (..), elements, listOf1, scale,
                     shrinkList, suchThat)

import           Cardano.Cluster (NodeName (..), NodeType (..))


-- * Types

data Cluster (nodeTypes :: [NodeType]) =
    Cluster { getCluster :: [(NodeName, NodeType)] }
    deriving (Show)

data Port =
    Port { getPort :: Word16 }
    deriving (Show)

-- * Instances

instance DemoteNodeTypes nodeTypes => Arbitrary (Cluster nodeTypes) where
    arbitrary = do
        nodeTypes <- scale (`mod` 10) $ listOf1 $ elements (demoteNodeTypes (Proxy @nodeTypes))
        let cluster = evalState (mapM fullyQualifiedNode nodeTypes) 0
        return (Cluster cluster)
      where
        fullyQualifiedNode :: NodeType -> State Int (NodeName, NodeType)
        fullyQualifiedNode typ = do
            n <- get
            put (n + 1)
            return $ case typ of
                NodeCore  -> (NodeName ("core-" <> show n), NodeCore)
                NodeRelay -> (NodeName ("relay-" <> show n), NodeRelay)
                NodeEdge  -> (NodeName ("edge-" <> show n), NodeEdge)

    shrink (Cluster xs) =
        Cluster <$> filter (not . null) (shrinkList (const []) xs)

instance Arbitrary Port where
    arbitrary = Port <$> suchThat arbitrary (< 50000)
    shrink _ = []

-- * Internal

-- | Get a value representation of nodeTypes
class DemoteNodeTypes (nodeTypes :: [NodeType]) where
    demoteNodeTypes :: Proxy nodeTypes -> [NodeType]

instance DemoteNodeTypes ('[] :: [NodeType]) where
    demoteNodeTypes _ = []

instance (DemoteNodeTypes xs) => DemoteNodeTypes ('NodeCore ': xs) where
    demoteNodeTypes _ = NodeCore : demoteNodeTypes (Proxy @xs)

instance (DemoteNodeTypes xs) => DemoteNodeTypes ('NodeRelay ': xs) where
    demoteNodeTypes _ = NodeRelay : demoteNodeTypes (Proxy @xs)

instance (DemoteNodeTypes xs) => DemoteNodeTypes ('NodeEdge ': xs) where
    demoteNodeTypes _ = NodeEdge : demoteNodeTypes (Proxy @xs)
