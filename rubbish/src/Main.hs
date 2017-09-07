module Main
       ( main
       ) where

import           Universum

import qualified Data.Set                   as S (fromList)
import           Formatting                 (sformat, shown, (%))
import           Mockable                   (Production, runProduction)
import           Network.Transport.Abstract (Transport, hoistTransport)
import qualified Network.Transport.TCP      as TCP (TCPAddr (..))
import           System.IO                  (hFlush, stdout)
import           System.Wlog                (logInfo)

import qualified Pos.Client.CLI             as CLI
import           Pos.Constants              (isDevelopment)
import           Pos.Core.Context           (giveStaticConsts)
import           Pos.Genesis                (devBalancesDistr, devGenesisContext,
                                             genesisContextProduction,
                                             genesisDevSecretKeys, gtcUtxo)
import           Pos.Launcher               (BaseParams (..), LoggingParams (..),
                                             bracketTransport, loggerBracket)
import           Pos.Rubbish                (LightWalletMode, WalletParams (..),
                                             runWalletStaticPeers)
import           Pos.Txp                    (unGenesisUtxo)
import           Pos.Util.Util              (powerLift)

import           Command                    (CmdCtx (..))
import           Plugin                     (rubbishPlugin)
import           RubbishOptions             (RubbishOptions (..), getRubbishOptions)

main :: IO ()
main = giveStaticConsts $ do
    opts@RubbishOptions {..} <- getRubbishOptions
    --filePeers <- maybe (return []) CLI.readPeersFile
    --                   (CLI.dhtPeersFile woCommonArgs)
    let allPeers = woPeers -- ++ filePeers
    let logParams =
            LoggingParams
            { lpRunnerTag     = "smart-wallet"
            , lpHandlerPrefix = CLI.logPrefix woCommonArgs
            , lpConfigPath    = CLI.logConfig woCommonArgs
            }
        baseParams = BaseParams { bpLoggingParams = logParams }

    print logParams

    let sysStart = CLI.sysStart woCommonArgs
    let devBalanceDistr =
            devBalancesDistr
                (CLI.flatDistr woCommonArgs)
                (CLI.richPoorDistr woCommonArgs)
                (CLI.expDistr woCommonArgs)
    let wpGenesisContext
            | isDevelopment = devGenesisContext devBalanceDistr
            | otherwise = genesisContextProduction
    let params =
            WalletParams
            { wpDbPath      = Just woDbPath
            , wpRebuildDb   = woRebuildDb
            , wpKeyFilePath = woKeyFilePath
            , wpSystemStart = sysStart
            , wpGenesisKeys = woDebug
            , wpBaseParams  = baseParams
            , ..
            }

    loggerBracket logParams $ runProduction $
      bracketTransport TCP.Unaddressable $ \transport -> do
        logInfo $ if isDevelopment
            then "Development Mode"
            else "Production Mode"
        logInfo $ sformat ("Length of genesis utxo: "%shown)
            (length $ unGenesisUtxo $ wpGenesisContext ^. gtcUtxo)
        let transport' :: Transport LightWalletMode
            transport' = hoistTransport
                (powerLift :: forall t . Production t -> LightWalletMode t)
                transport

            cmdCtx = CmdCtx
                      { skeys = if isDevelopment then genesisDevSecretKeys else []
                      , na = woPeers
                      , genesisBalanceDistr = devBalanceDistr
                      }

        logInfo "Using MPC coin tossing"
        liftIO $ hFlush stdout
        let plugins = first one (rubbishPlugin cmdCtx opts)
        runWalletStaticPeers woNodeDbPath transport' (S.fromList allPeers) params plugins
