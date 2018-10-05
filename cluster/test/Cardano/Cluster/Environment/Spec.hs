{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}

module Cardano.Cluster.Environment.Spec
    (
    -- * Environment
      prop_generatedEnvironmentIsValid

    -- * Topology
    , prop_coresAndRelaysTopologyStatic
    , prop_edgesTopologyBehindNat
    , prop_edgesConnectedToAllRelays
    ) where

import           Universum

import           Control.Lens (at, (?~))
import           Data.Map.Strict ((!))
import           Test.QuickCheck (Property, classify, conjoin, counterexample,
                     property, (===), (==>))

import           Cardano.Cluster (NodeName (..), NodeType (..))
import           Cardano.Cluster.Environment
import           Cardano.Cluster.Util (nextNtwrkAddr, ntwrkAddrToString,
                     unsafeNetworkAddressFromString)
import           Pos.Core.NetworkAddress (NetworkAddress)
import           Pos.Infra.Network.DnsDomains (DnsDomains (..), NodeAddr (..))
import           Pos.Infra.Network.Yaml (Topology (..))

import           Cardano.Cluster.Environment.Arbitrary (Cluster (..), Port (..))
import           Cardano.Cluster.Util.Arbitrary (SeparatedBy (..))


-- * Environment

prop_generatedEnvironmentIsValid
    :: Cluster '[ 'NodeCore, 'NodeRelay, 'NodeEdge ]
    -> Maybe Port
    -> SeparatedBy "/"
    -> Property
prop_generatedEnvironmentIsValid (Cluster nodes) mport (SeparatedBy stateDir) = do
    let (cPort, wPort, env0) = case mport of
          Nothing   -> (3000, 8090, mempty)
          Just (Port port) -> (port, port+1000, mempty
            & at "LISTEN" ?~ ntwrkAddrToString ("localhost", port)
            & at "WALLET_ADDRESS" ?~ ntwrkAddrToString (nextNtwrkAddr 1000 ("localhost", port))
            )

    let envs = map (\x -> (x, snd $ prepareEnvironment x nodes stateDir env0)) nodes

    (length nodes <= 10) ==>
        conjoin $ flip map envs $ \((_, nodeType), env) -> case nodeType of
            NodeCore -> conjoin
                [ prop_commonEnvironment env
                , prop_environmentCore env (cPort, Just $ cPort + 100)
                ]

            NodeRelay -> conjoin
                [ prop_commonEnvironment env
                , prop_environmentRelay env (cPort + 100, Nothing)
                ]

            NodeEdge -> conjoin
                [ prop_commonEnvironment env
                , prop_environmentEdge env ((wPort, Just $ wPort + 100), (wPort +100, Nothing))
                ]
  where
    prop_commonEnvironment env = conjoinWithContext "prop_commonEnvironment"
        [ prop_hasEnvVar  env "CONFIGURATION_FILE"
        , prop_hasEnvVar  env "CONFIGURATION_KEY"
        , prop_hasEnvVar  env "DB_PATH"
        , prop_hasEnvVar  env "NODE_ID"
        , prop_hasEnvVar  env "LOG_CONFIG"
        , prop_noEnvVar   env "LOG_SEVERITY"
        , prop_hasEnvVarP env "REBUILD_DB" (`elem` ["True", "False"])
        ]

    prop_environmentCore env portRange = conjoinWithContext "prop_environmentCore"
        [ prop_portWithin env "LISTEN" portRange
        , prop_noEnvVar   env "WALLET_ADDRESS"
        , prop_noEnvVar   env "WALLET_DOC_ADDRESS"
        , prop_noEnvVar   env "WALLET_DB_PATH"
        , prop_noEnvVar   env "WALLET_REBUILD_DB"
        , prop_noEnvVar   env "NO_CLIENT_AUTH"
        , prop_noEnvVar   env "TLSCERT"
        , prop_noEnvVar   env "TLSKEY"
        , prop_noEnvVar   env "TLSCA"
        ]

    prop_environmentRelay env portRange = conjoinWithContext "prop_environmentRelay"
        [ prop_portWithin env "LISTEN" portRange
        , prop_noEnvVar   env "WALLET_ADDRESS"
        , prop_noEnvVar   env "WALLET_DOC_ADDRESS"
        , prop_noEnvVar   env "WALLET_DB_PATH"
        , prop_noEnvVar   env "WALLET_REBUILD_DB"
        , prop_noEnvVar   env "NO_CLIENT_AUTH"
        , prop_noEnvVar   env "TLSCERT"
        , prop_noEnvVar   env "TLSKEY"
        , prop_noEnvVar   env "TLSCA"
        ]

    prop_environmentEdge env (apiRange, docRange) = conjoinWithContext "prop_environmentEdge"
        [ prop_portWithin env "WALLET_ADDRESS" apiRange
        , prop_portWithin env "WALLET_DOC_ADDRESS" docRange
        , prop_hasEnvVar  env "WALLET_DB_PATH"
        , prop_hasEnvVar  env "TLSCERT"
        , prop_hasEnvVar  env "TLSKEY"
        , prop_hasEnvVar  env "TLSCA"
        , prop_hasEnvVarP env "WALLET_REBUILD_DB" (`elem` ["True", "False"])
        , prop_hasEnvVarP env "NO_CLIENT_AUTH" (`elem` ["True", "False"])
        ]

conjoinWithContext :: String -> [String -> Property] -> Property
conjoinWithContext ctx = conjoin . map (\prop -> prop ctx)

prop_hasEnvVarP :: Env -> String -> (String -> Bool) -> String -> Property
prop_hasEnvVarP env var predicate ctx =
    case (env ^. at var) of
        Nothing ->
            counterexample (ctx <> ": ENV var <" <> var <> "> expected but not present in\n" <> show env ) $ False
        Just x | not (predicate x) ->
            counterexample (ctx <> ": ENV var <" <> var <> "> expected but fails predicate in\n" <> show env ) $ False
        Just _ ->
            property True

prop_hasEnvVar :: Env -> String -> String -> Property
prop_hasEnvVar env var =
    prop_hasEnvVarP env var (/= "")

prop_noEnvVar :: Env -> String -> String -> Property
prop_noEnvVar env var ctx =
    case (env ^. at var) of
        Just _ ->
            counterexample (ctx <> ": ENV var <" <> var <> "> not expected but present in\n" <> show env ) $ False
        Nothing ->
            property True

prop_portWithin :: Env -> String -> (Word16, Maybe Word16) -> String -> Property
prop_portWithin env var (minPort, mmaxPort) ctx =
    let
        (_, port) = unsafeNetworkAddressFromString (env ! var)
    in
        case mmaxPort of
            Nothing ->
                counterexample (ctx <> ": <" <> show var <> "> should be above "
                    <> show minPort <> " in " <> show env) $ port >= minPort

            Just maxPort ->
                counterexample (ctx <> ": <" <> show var <> "> should be within ("
                    <> show minPort <> ", " <> show maxPort <> ") in " <> show env)
                    $ port >= minPort && port < maxPort


-- * Topology

prop_coresAndRelaysTopologyStatic
    :: Cluster ' [ 'NodeCore, 'NodeRelay ]
    -> SeparatedBy "/"
    -> Property
prop_coresAndRelaysTopologyStatic (Cluster nodes) =
    conjoin . map prop_nodeTopologyIsStatic . getTopologies nodes
  where
    prop_nodeTopologyIsStatic (_, TopologyStatic{}) = property True
    prop_nodeTopologyIsStatic _                     = property False

prop_edgesTopologyBehindNat
    :: Cluster ' [ 'NodeEdge ]
    -> SeparatedBy "/"
    -> Property
prop_edgesTopologyBehindNat (Cluster nodes) =
    conjoin . map prop_nodeTopologyIsBehindNat . getTopologies nodes
  where
    prop_nodeTopologyIsBehindNat (_, TopologyBehindNAT {}) = property True
    prop_nodeTopologyIsBehindNat _                         = property False

prop_edgesConnectedToAllRelays
    :: Cluster ' [ 'NodeCore, 'NodeRelay, 'NodeEdge ]
    -> SeparatedBy "/"
    -> Property
prop_edgesConnectedToAllRelays (Cluster nodes) =
    withRelays prop_edgeConnectedToAllRelays . getTopologies nodes
  where
    withRelays prop topologies =
        let
            edges  =
                filter ((== NodeEdge) . (^. _2) . (^. _1)) topologies
            relays =
                map ((^. _3) . (^. _1)) . filter ((== NodeRelay) . (^. _2) . (^. _1))
                $ topologies
        in
            conjoin $ map (prop relays) edges

    prop_edgeConnectedToAllRelays relays (_, TopologyBehindNAT _ _ (DnsDomains [domains])) =
        classify (null relays) "no relays" $ sort relays === sort (domainToNtwrkAddr <$> domains)
    prop_edgeConnectedToAllRelays _ (_, topology) =
        counterexample ("expected TopologyBehindNAT for edge nodes, got:" <> show topology) False

getTopologies
    :: [(NodeName, NodeType)]
    -> SeparatedBy "/"
    -> [((NodeName, NodeType, NetworkAddress), Topology)]
getTopologies nodes (SeparatedBy stateDir) =
    zip (zip3 nodeNames nodeTypes nodeAddresses) (extractTopologies envs)
  where
    (nodeNames, nodeTypes) = unzip nodes
    nodeAddresses          = extractAddresses envs
    envs                   = map (\x -> prepareEnvironment x nodes stateDir mempty) nodes
    extractTopologies      = map (getArtifact . (^. _2) . (^. _1))
    extractAddresses       = map (getNtwrkAddr . (^. _2))
    getNtwrkAddr env       = fromMaybe
        (unsafeNetworkAddressFromString (env ! "WALLET_ADDRESS"))
        (unsafeNetworkAddressFromString <$> (env ^. at "LISTEN"))

domainToNtwrkAddr :: NodeAddr a -> NetworkAddress
domainToNtwrkAddr (NodeAddrExact ip (Just port)) = (show ip, port)
domainToNtwrkAddr _ = error "expected only NodeAddrExact constructor with port"
