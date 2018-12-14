module Main
    ( main
    ) where

import           Universum

import           Cardano.Node.API (launchNodeServer)
import           Pos.Chain.Update (updateConfiguration)
import           Pos.Client.CLI (NodeWithApiArgs (..), getNodeApiOptions,
                     loggingParams)
import           Pos.Launcher (actionWithCoreNode, launchNode)
import           Pos.Util.CompileInfo (compileInfo, withCompileInfo)

main :: IO ()
main = withCompileInfo $ do
    NodeWithApiArgs cArgs nArgs apiArgs <- getNodeApiOptions
    let lArgs = loggingParams "node" cArgs
    launchNode nArgs cArgs lArgs
        $ \genConfig walConfig txpConfig ntpConfig nodeParams sscParams nodeResources ->
            actionWithCoreNode
                (launchNodeServer
                    apiArgs
                    ntpConfig
                    nodeResources
                    updateConfiguration
                    compileInfo
                    genConfig)
                genConfig walConfig txpConfig
                ntpConfig nodeParams sscParams nodeResources
