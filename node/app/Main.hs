module Main
    ( main
    ) where

import           Universum

import           Control.Concurrent.Async (concurrently_)

import           Cardano.Node.API (launchNodeServer)
import           Pos.Client.CLI (SimpleNodeArgs (..), getSimpleNodeOptions,
                     loggingParams)
import           Pos.Launcher (actionWithCoreNode, launchNode)
import           Pos.Util.CompileInfo (withCompileInfo)


main :: IO ()
main = withCompileInfo $ do
    SimpleNodeArgs cArgs nArgs <- getSimpleNodeOptions
    let lArgs = loggingParams "node" cArgs
    launchNode nArgs cArgs lArgs
        $ \genConfig walConfig txpConfig ntpConfig nodeParams sscParams nodeResources ->
            actionWithCoreNodeAlso
                genConfig walConfig txpConfig ntpConfig
                nodeParams sscParams nodeResources
                $ \diffusion ->
                    launchNodeServer
                        diffusion
                        ntpConfig

