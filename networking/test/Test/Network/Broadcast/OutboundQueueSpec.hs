{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Network.Broadcast.OutboundQueueSpec
       ( spec
       ) where

import           Control.Monad
import           Control.Concurrent.MVar (modifyMVar_, newMVar, readMVar)
import           Data.List (delete)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Mockable as M
import qualified Network.Broadcast.OutboundQueue as OutQ
import           Network.Broadcast.OutboundQueue.Demo
import           Network.Broadcast.OutboundQueue.Types hiding (simplePeers)
import           System.Wlog
import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck (Arbitrary (..), Property, choose, forAll, ioProperty, property,
                                  (===), (==>))

setupNodes :: Int -> IO [Node]
setupNodes n = M.runProduction $ do
    ns <- forM [1..n] $ \nodeIdx -> newNode (C nodeIdx) NodeCore (CommsDelay 0) onConnChange
    forM_ ns $ \theNode -> setPeers theNode (delete theNode ns)
    return ns
    where
        onConnChange :: OutQ.ConnectionChangeAction IO NodeId
        onConnChange = OutQ.ConnectionChangeAction (const $ return ())

testInFlight :: IO Bool
testInFlight = do
    -- disable logging
    removeAllHandlers

    -- Set up some test nodes
    allNodes <- setupNodes 4

    runEnqueue $ do
      -- Send messages asynchronously
      forM_ [1..1000] $ \n -> do
        send Asynchronous
             (allNodes !! 0)
             (MsgTransaction OriginSender)
             (MsgId n)
             (OutQ.ConnectionChangeAction $ \_ -> return ())
      -- Abruptly unsubscribe whilst messages are getting delivered
      forM_ allNodes $ \theNode -> setPeers theNode []

    -- Verify the invariants
    let queues = map nodeOutQ allNodes
    forM_ queues OutQ.flush

    allInFlights <- mapM OutQ.currentlyInFlight queues
    return $ all allGreaterThanZero allInFlights

allGreaterThanZero :: M.Map NodeId (M.Map OutQ.Precedence Int) -> Bool
allGreaterThanZero imap = all (>= 0) $ (concatMap M.elems (M.elems imap))

testConnStatus :: Property
testConnStatus = forAll arbitrary $ \(as :: [Bool]) ->
    ioProperty $ do
      -- disable logging
      removeAllHandlers

      -- Set up test nodes
      allNodes <- setupNodes 4

      let sendingNode = allNodes !! 0

      nss <- runEnqueue $ do
        -- Send messages synchronously
        forM (zip [1..] as) $ \(n, success) -> do
          if success
            then send Synchronous
                      sendingNode
                      (MsgTransaction OriginSender)
                      (MsgId n)
                      (OutQ.ConnectionChangeAction $ \_ -> return ())
            else sendError Synchronous
                           sendingNode
                           (MsgTransaction OriginSender)
                           (MsgId n)
                           (OutQ.ConnectionChangeAction $ \_ -> return ())

      -- Flush all queues
      forM_ (map nodeOutQ allNodes) OutQ.flush

      -- Node ids to which we sent messages
      let ns :: [NodeId_]
          ns = map nodeId_ $ concat nss
      let expected :: M.Map NodeId_ OutQ.ConnectionStatus
          expected = M.union
            (M.fromList
                $ zip ns
                $ map (\noErr -> if noErr then OutQ.Connected else OutQ.ConnectionBroken) as)
            (M.fromList $ map ((,OutQ.NotConnected) . nodeId_ . nodeId) (tail allNodes))

      checkConnections expected sendingNode

    where
    checkConnections :: M.Map NodeId_ OutQ.ConnectionStatus -> Node -> IO Property
    checkConnections expected n = do
        cns <- readMVar (OutQ.qConnections . nodeOutQ $ n)
        return $ expected === M.mapKeys nodeId_ cns

testConnChangeAction :: Property
testConnChangeAction = forAll arbitrary $ \(as :: [Bool]) ->
    ioProperty $ do
        -- disable logging
        removeAllHandlers

        -- Define connection change action
        connsMVar <- newMVar M.empty
        let onConnChange :: OutQ.ConnectionChangeAction IO NodeId
            onConnChange = OutQ.ConnectionChangeAction $ \conns ->
                modifyMVar_ connsMVar (const $ return conns)

        -- Set up test nodes
        (sendingNode, allNodes) <- setupNodes' 4 onConnChange

        runEnqueue $ do
          -- Send messages asynchronously
          forM_ (zip [1..] as) $ \(n, success) -> do
            if success
              then send Asynchronous
                        sendingNode
                        (MsgTransaction OriginSender)
                        (MsgId n)
                        (OutQ.liftConnectionChangeAction onConnChange)
              else sendError Asynchronous
                             sendingNode
                             (MsgTransaction OriginSender)
                             (MsgId n)
                             (OutQ.liftConnectionChangeAction onConnChange)

        -- Flush all queues
        forM_ (map nodeOutQ allNodes) OutQ.flush

        conns <- readMVar connsMVar
        qConns <- readMVar (OutQ.qConnections . nodeOutQ $ sendingNode)

        return $ conns === qConns

    where
        setupNodes' :: Int -> OutQ.ConnectionChangeAction IO NodeId -> IO (Node, [Node])
        setupNodes' n onConnsChange = do
            nodes <- setupNodes (n - 1)
            sendingNode <- newNode (C n) NodeCore (CommsDelay 0) onConnsChange
            return (sendingNode, sendingNode : nodes)

spec :: Spec
spec = describe "OutBoundQ" $ do
    -- We test that `removePeer` will never yield something like
    -- `[[]]`. See: https://issues.serokell.io/issue/CSL-1652
    it "removePeer doesn't yield empty singletons" $ property prop_removePeer
    it "removePeer does preserve order" $ property prop_removePeer_ordering

    modifyMaxSuccess (const 100) $ do
      -- Simulate a multi-peer conversation and then check
      -- that after that we never have a negative count for
      -- the `qInFlight` field of a `OutBoundQ`.
      it "inflight conversations" $ ioProperty $ testInFlight

    it "connection status" testConnStatus

    it "connection change action" testConnChangeAction

newtype FiniteInt = FI Int deriving (Show, Eq, Ord)

instance Arbitrary FiniteInt where
    arbitrary = FI <$> choose (0, 1024)

finiteToList :: [FiniteInt] -> [Int]
finiteToList = map (\(FI x) -> x)

prop_removePeer :: Property
prop_removePeer = forAll arbitrary $ \(peers :: Peers FiniteInt) ->
    forAll arbitrary $ \(toRemove :: FiniteInt) ->
       toRemove `Set.member` peersRouteSet peers ==>
         let Peers{..} = removePeer toRemove peers
         in and $ map checkProp [_routesCore peersRoutes, _routesEdge peersRoutes , _routesRelay peersRoutes]
  where
    checkProp = all (not . null . finiteToList)

-- We purposefully try to remove something which is not there, to make sure
-- removePeer doesn't alter the ordering of the forwading sets.
prop_removePeer_ordering :: Property
prop_removePeer_ordering = forAll arbitrary $ \(peers :: Peers FiniteInt) ->
         let stripped = filterEmptySingletons peers
             peers' = removePeer (FI 2000) stripped
         in  peers' === stripped
  where
    filterEmptySingletons p =
      let newRoutes = Routes (filter (not . null) (_routesCore . peersRoutes $ p))
                             (filter (not . null) (_routesRelay . peersRoutes $ p))
                             (filter (not . null) (_routesEdge . peersRoutes $ p))
      in p { peersRoutes = newRoutes }
