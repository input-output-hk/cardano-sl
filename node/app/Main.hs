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
import           Pos.Util.Wlog (logInfo)

main :: IO ()
main = withCompileInfo $ do
    NodeWithApiArgs cArgs nArgs mApiArgs <- getNodeApiOptions
    let lArgs = loggingParams "node" cArgs

    let nodeServer = case mApiArgs of
            Nothing ->
                \_ _ _ _ _ _ -> logInfo "Monitoring API is disabled."
            Just apiArgs ->
                launchNodeServer apiArgs

    launchNode nArgs cArgs lArgs
        $ \genConfig walConfig txpConfig ntpConfig nodeParams sscParams nodeResources ->
            actionWithCoreNode
                (nodeServer ntpConfig nodeResources updateConfiguration compileInfo genConfig)
                genConfig walConfig txpConfig
                ntpConfig nodeParams sscParams nodeResources
