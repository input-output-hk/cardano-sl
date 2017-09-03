{-# LANGUAGE ScopedTypeVariables #-}
module Test.Network.Broadcast.OutboundQueueSpec
       ( spec
       ) where

import           Control.Monad
import           Data.List                             (delete)
import qualified Data.Map.Strict                       as M
import qualified Mockable                              as M
import qualified Network.Broadcast.OutboundQueue       as OutQ
import           Network.Broadcast.OutboundQueue.Demo
import           Network.Broadcast.OutboundQueue.Types hiding (simplePeers)
import           System.Wlog
import           Test.Hspec                            (Spec, describe, it)
import           Test.Hspec.QuickCheck                 (modifyMaxSuccess)
import           Test.QuickCheck                       (ioProperty)

-- disable logging
testInFlight :: IO Bool
testInFlight = do
    removeAllHandlers

    -- Set up some test nodes
    allNodes <- M.runProduction $ do
      ns <- forM [1..4] $ \nodeIdx -> newNode (C nodeIdx) NodeCore  (CommsDelay 0)
      forM_ ns $ \theNode -> setPeers theNode  (delete theNode ns)
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
spec = describe "OutBoundQ" $ modifyMaxSuccess (const 100) $ do
  -- Simulate a multi-peer conversation and then check
  -- that after that we never have a negative count for
  -- the `qInFlight` field of a `OutBoundQ`.
  it "inflight conversations" $ ioProperty $ testInFlight
