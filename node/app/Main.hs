module Main
    ( main
    ) where

import           Universum

import           Cardano.Node.API (launchNodeServer)
import           Pos.Chain.Update (updateConfiguration)
import           Pos.Client.CLI (SimpleNodeArgs (..), getSimpleNodeOptions,
                     loggingParams)
import           Pos.Launcher (actionWithCoreNodeAlso, launchNode)
import           Pos.Util.CompileInfo (compileInfo, withCompileInfo)

main :: IO ()
main = withCompileInfo $ do
    SimpleNodeArgs cArgs nArgs <- getSimpleNodeOptions
    let lArgs = loggingParams "node" cArgs
    launchNode nArgs cArgs lArgs
        $ \genConfig _walConfig txpConfig ntpConfig _nodeParams _sscParams nodeResources ->
            actionWithCoreNodeAlso genConfig txpConfig nodeResources
                $ \diffusion ->
                    launchNodeServer
                        diffusion
                        ntpConfig
                        nodeResources
                        updateConfiguration
                        compileInfo
