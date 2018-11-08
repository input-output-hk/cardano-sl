module Main
    ( main
    ) where

import           Universum

import           Control.Concurrent.Async (concurrently_)

import           Pos.Client.CLI (SimpleNodeArgs (..), getSimpleNodeOptions,
                     loggingParams)
import           Pos.Launcher (actionWithCoreNode, launchNode)
import           Pos.Util.CompileInfo (withCompileInfo)


main :: IO ()
main = withCompileInfo $ do
    SimpleNodeArgs cArgs nArgs <- getSimpleNodeOptions
    let lArgs = loggingParams "node" cArgs
    launchNode nArgs cArgs lArgs
        $ \genConfig walConfig txpConfig ntpConfig nodeParams sscParams nodeResources -> do
            concurrently_
                (actionWithCoreNode genConfig walConfig txpConfig ntpConfig nodeParams sscParams nodeResources)
                (launchNodeServer)

launchNodeServer :: IO ()
launchNodeServer = putText "hello world"
