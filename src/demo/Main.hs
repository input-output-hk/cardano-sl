module Main where

import           Universum

import           Data.String  (fromString)
import           Pos.Launcher (NodeParams (..), runNodeReal)

runSingleNode :: Word -> IO ()
runSingleNode i = runNodeReal params
  where
    params =
        NodeParams
        { npDbPath = Just ("node-db-" ++ show i)
        , npRebuildDb = True
        , npSystemStart = Nothing
        , npLoggerName = "node" `mappend` fromString (show i)
        }

main :: IO ()
main = do
    let n = 3
    -- let loggers = "xx" : map (LoggerName . toS . sformat nodeF)
    --                         [NodeId 0 .. NodeId (n - 1)]
    -- initLogging loggers Info
    mapM_ runSingleNode [0 .. n - 1]
