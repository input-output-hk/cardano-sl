{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Launcher of node.

module Pos.Launcher
       ( runNodesReal
       ) where

import           Control.TimeWarp.Logging (logInfo, setLoggerName, usingLoggerName)
import           Control.TimeWarp.Timed   (runTimedIO, sleepForever, virtualTime)
import           Data.IORef               (modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map                 as Map
import           Formatting               (int, sformat, (%))
import           Protolude                hiding (for, wait, (%))
import           Serokell.Util            ()

import           Pos.Communication        (Node, inSlot, systemStart)
import           Pos.Constants            (n, slotDuration)
import           Pos.Crypto               (keyGen)
import           Pos.Types.Types          (NodeId (..))
import           Pos.WorkMode             (RealMode, WorkMode)

runNodes :: WorkMode m => [Node m] -> m ()
runNodes nodes = setLoggerName "xx" $ do
    keys <- Map.fromList . zip [NodeId 0 .. NodeId (n-1)] <$>
                replicateM n keyGen
    -- The system shall start working in a bit of time. Not exactly right now
    -- because due to the way inSlot implemented, it'd be nice to wait a bit
    -- – if we start right now then all nodes will miss the first slot of the
    -- first epoch.
    now <- virtualTime
    liftIO $ writeIORef systemStart (now + slotDuration `div` 2)
    inSlot False $ \epoch slot -> do
        when (slot == 0) $
            logInfo $ sformat ("========== EPOCH "%int%" ==========") epoch
        logInfo $ sformat ("---------- slot "%int%" ----------") slot
    nodeCallbacks <- liftIO $ newIORef mempty
    let send n_from n_to message = do
            f <- (Map.! n_to) <$> liftIO (readIORef nodeCallbacks)
            f n_from message
    for_ (zip [0..] nodes) $ \(i, nodeFun) -> do
        let nid = NodeId i
        f <- nodeFun nid (keys Map.! nid) (fmap fst keys) (send nid)
        liftIO $ modifyIORef' nodeCallbacks (Map.insert nid f)
    sleepForever

runNodesReal :: [Node RealMode] -> IO ()
runNodesReal = runTimedIO . usingLoggerName mempty . runNodes
