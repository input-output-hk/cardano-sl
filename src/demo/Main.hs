module Main where

import           Data.String  (fromString)
import           Universum

import           Pos.Launcher (NodeParams (..), getCurTimestamp, runNodeReal)
import           Pos.Slotting (Timestamp)

runSingleNode :: Timestamp -> Word -> IO ()
runSingleNode start i = runNodeReal params
  where
    params =
        NodeParams
        { npDbPath = Just ("node-db-" ++ show i)
        , npRebuildDb = True
        , npSystemStart = Just start
        , npLoggerName = "node" <> fromString (show i)
        }

main :: IO ()
main = do
    let n = 3
    -- let loggers = "xx" : map (LoggerName . toS . sformat nodeF)
    --                         [NodeId 0 .. NodeId (n - 1)]
    -- initLogging loggers Info
    systemStart <- getCurTimestamp
    mapM_ (runSingleNode systemStart) [0 .. n - 1]
