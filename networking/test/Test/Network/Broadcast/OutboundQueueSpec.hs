{-# LANGUAGE ScopedTypeVariables #-}
module Test.Network.Broadcast.OutboundQueueSpec
       ( spec
       -- TODO define elsewhere.
       , arbitraryNodeType
       , arbitraryRoutes
       , arbitraryPeers
       ) where

import           Control.Monad
import           Data.List (delete)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Semigroup ((<>))
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Network.Broadcast.OutboundQueue as OutQ
import           Network.Broadcast.OutboundQueue.Demo
import           Network.Broadcast.OutboundQueue.Types hiding (simplePeers)
import           System.Wlog
import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck (Gen, Property, choose, forAll, ioProperty, property,
                                  suchThat, (===))
import qualified Test.QuickCheck as QC

arbitraryNodeType :: Gen NodeType
arbitraryNodeType = QC.elements [minBound .. maxBound]

-- | An arbitrary 'Routes nid' must respect the invariant that an 'nid' does
-- not appear in multiple classifications.
-- You also get the map classifying each node in the routes.
arbitraryRoutes :: Ord nid => Gen nid -> Gen (Routes nid, Map nid NodeType)
arbitraryRoutes genNid = do
    -- First, generate arbitrary sets of cores, relays, and edges.
    coreSet  <- Set.fromList <$> QC.listOf genNid
    let usedNids = coreSet
    relaySet <- Set.fromList <$> QC.listOf (genNid `suchThat` (not . flip Set.member usedNids))
    let usedNids' = usedNids `Set.union` relaySet
    edgeSet  <- Set.fromList <$> QC.listOf (genNid `suchThat` (not . flip Set.member usedNids'))
    -- Now randomly spread them into conjunctinos of disjunctions.
    cores  <- spread coreSet
    relays <- spread relaySet
    edges  <- spread edgeSet
    let coreMap  = M.fromList (flip (,) NodeCore  <$> Set.toList coreSet)
        relayMap = M.fromList (flip (,) NodeRelay <$> Set.toList relaySet)
        edgeMap  = M.fromList (flip (,) NodeEdge  <$> Set.toList edgeSet)
        classification = coreMap <> relayMap <> edgeMap
    pure (Routes cores relays edges, classification)
  where
    -- None of the lists will be empty.
    spread :: Set t -> Gen [[t]]
    spread s = go [] (Set.size s) (Set.toList s)
      where
        go :: [[t]] -> Int -> [t] -> Gen [[t]]
        go acc 0 [] = pure acc
        go acc n ts = do
            toTake <- choose (1, n)
            let (ac, ts') = splitAt toTake ts
            go (ac : acc) (n - toTake) ts'

-- | There are invariants of 'Peers nid' that must be respected (see docs on
-- that type from 'Network.Broadcast.OutboundQueue.Types') so we can't use
-- 'Peers <$> arbitrary <*> arbitrary'.
--
-- Uses 'arbitraryRoutes' then throws in arbitrary-many extra, unclassified
-- peers.
arbitraryPeers :: Ord nid => Gen nid -> Gen NodeType -> Gen (Peers nid)
arbitraryPeers genNid genNodeType = do
    (routes, classification) <- arbitraryRoutes genNid
    extras <- QC.listOf ((,) <$> (genNid `suchThat` flip M.notMember classification) <*> genNodeType)
    pure $ Peers routes (classification <> M.fromList extras)

-- | FIXME should study this test to find out what exactly is does. Also, why
-- it's so slow.
--
-- Potentital confusion: in the text of this definition "node" really means
-- "outbound queue".
testInFlight :: IO Bool
testInFlight = do
    removeAllHandlers

    -- Set up some test nodes
    allNodes <- do
      ns <- forM [1..4] $ \nodeIdx -> newNode (C nodeIdx) NodeCore (CommsDelay 0)
      forM_ ns $ \theNode -> setPeers theNode (delete theNode ns)
      return ns

    runEnqueue $ do
      -- Send messages asynchronously
      forM_ [1..1000] $ \n -> do
        send Asynchronous (allNodes !! 0) (MsgTransaction OriginSender) (MsgId n)
      -- Abruptly unsubscribe whilst messages are getting delivered
      forM_ allNodes $ \theNode -> setPeers theNode []

    -- Verify the invariants
    let queues = map nodeOutQ allNodes
    forM_ queues OutQ.flush

    allInFlights <- mapM OutQ.currentlyInFlight queues
    return $ all allGreaterThanZero allInFlights

allGreaterThanZero :: M.Map NodeId (M.Map OutQ.Precedence Int) -> Bool
allGreaterThanZero imap = all (>= 0) $ (concatMap M.elems (M.elems imap))

spec :: Spec
spec = describe "OutBoundQ" $ do
    -- We test that `removePeer` will never yield something like
    -- `[[]]`. See: https://issues.serokell.io/issue/CSL-1652
    it "removePeer doesn't yield empty singletons" $ property prop_removePeer
    it "removePeer does preserve order" $ property prop_removePeer_ordering

    -- This test takes quite a long time so we'll drop the max successes.
    modifyMaxSuccess (const 10) $ do
      -- Simulate a multi-peer conversation and then check
      -- that after that we never have a negative count for
      -- the `qInFlight` field of a `OutBoundQ`.
      it "inflight conversations" $ ioProperty $ testInFlight

arbitraryFiniteInt :: Gen Int
arbitraryFiniteInt = choose (0, 1024)

prop_removePeer :: Property
prop_removePeer = forAll (arbitraryPeers arbitraryFiniteInt arbitraryNodeType) $
    \(peers :: Peers Int) ->
        let ints = Set.toList (peersRouteSet peers)
        -- For every key in the route set, we check the property.
        in  forAll (QC.choose (0, Set.size (peersRouteSet peers) - 1)) $ \idx ->
            let toRemove = ints !! idx
                Peers{..} = removePeer toRemove peers
            in and $ map checkProp [_routesCore peersRoutes, _routesEdge peersRoutes , _routesRelay peersRoutes]
  where
    checkProp = all (not . null)

-- We purposefully try to remove something which is not there, to make sure
-- removePeer doesn't alter the ordering of the forwading sets.
prop_removePeer_ordering :: Property
prop_removePeer_ordering = forAll (arbitraryPeers arbitraryFiniteInt arbitraryNodeType) $
    \(peers :: Peers Int) ->
         let stripped = filterEmptySingletons peers
             peers' = removePeer (2000 :: Int) stripped
         in  peers' === stripped
  where
    filterEmptySingletons p =
      let newRoutes = Routes (filter (not . null) (_routesCore  . peersRoutes $ p))
                             (filter (not . null) (_routesRelay . peersRoutes $ p))
                             (filter (not . null) (_routesEdge  . peersRoutes $ p))
      in p { peersRoutes = newRoutes }
