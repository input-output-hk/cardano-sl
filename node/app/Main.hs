module Main
    ( main
    ) where

import           Universum

import           Cardano.Node.API (launchNodeServer)
import           Pos.Chain.Update (updateConfiguration)
import           Pos.Client.CLI (SimpleNodeArgs (..), getSimpleNodeOptions,
                     loggingParams)
import           Pos.Launcher (actionWithCoreNode, launchNode)
import           Pos.Util.CompileInfo (compileInfo, withCompileInfo)

main :: IO ()
main = withCompileInfo $ do
    SimpleNodeArgs cArgs nArgs <- getSimpleNodeOptions
    let lArgs = loggingParams "node" cArgs
    launchNode nArgs cArgs lArgs
        $ \genConfig walConfig txpConfig ntpConfig nodeParams sscParams nodeResources ->
            actionWithCoreNode
                (launchNodeServer
                    ntpConfig
                    nodeResources
                    updateConfiguration
                    compileInfo)
                genConfig walConfig txpConfig
                ntpConfig nodeParams sscParams nodeResources
