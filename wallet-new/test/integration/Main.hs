module Main where

import           Universum

import           Control.Concurrent.STM (newTQueueIO)
import           Data.Default (Default (..))
import           System.Environment (lookupEnv)

import           Pos.Client.CLI.NodeOptions (CommonNodeArgs (..), NodeArgs (..))
import           Pos.Client.CLI.Options (CommonArgs (..))
import           Pos.Client.CLI.Params (getNodeParams, gtSscParams)
import           Pos.Core (ProtocolMagic)
import           Pos.Core.Configuration (defaultGenesisSpec, epochSlots,
                     withProtocolConstants)
import           Pos.Core.Constants (accountGenesisIndex, wAddressGenesisIndex)
import           Pos.Core.Genesis (GeneratedGenesisData (..),
                     GeneratedSecrets (..), GenesisProtocolConstants (..),
                     GenesisSpec (..), PoorSecret, generateGenesisData,
                     genesisProtocolConstantsToProtocolConstants,
                     poorSecretToEncKey)
import           Pos.Core.JsonLog.LogEvents (jsonLogConfigFromHandle)
import           Pos.Core.Reporting (noReporter)
import           Pos.Crypto.Signing (PassPhrase)
import           Pos.DB (NodeDBs, closeNodeDBs, openNodeDBs)
import           Pos.DB.DB (initNodeDBs)
import           Pos.Launcher (ConfigurationOptions (..), HasConfigurations,
                     NodeParams (..), NodeResources (..), bracketNodeResources,
                     withConfigurations)
import           Pos.Txp (txpGlobalSettings)
import           Pos.Util.UserSecret (WalletUserSecret (..), usVss)
import           Pos.Wallet.Web.Methods (importWalletDo)
import           Pos.Wallet.Web.Mode (WalletWebMode, WalletWebModeContext (..))
import           Pos.Wallet.Web.State.Acidic (closeState, openState)
import           Pos.WorkMode (RealModeContext (..))
import           System.Wlog (HasLoggerName (askLoggerName))


data ExtraNodeArgs = ExtraNodeArgs
    { _nodePath   :: FilePath
    , _walletPath :: FilePath
    }


-- | Run a particular wallet action in a default context.
--
-- Note that a few things are configurable through environment variable:
--
runWalletWebMode
    :: (CommonNodeArgs, NodeArgs, ExtraNodeArgs)
    -> (HasConfigurations => WalletWebMode a)
    -> IO a
runWalletWebMode (commonNodeArgs, nodeArgs, ExtraNodeArgs{..}) action =
    withConfigurations Nothing def
        $ \_ protocolMagic -> bracket (openState True _walletPath) closeState
        $ \walletState -> bracket (openNodeDBs True _nodePath) closeNodeDBs
        $ \nodeDBs -> do
            ctx <- WalletWebModeContext
                <$> pure walletState
                <*> newTVarIO def
                <*> liftIO newTQueueIO
                <*> newRealModeContext protocolMagic nodeDBs
            action `runReaderT` ctx
  where
    -- | Create a new Real Mode context needed to run wallet's actions
    newRealModeContext
        :: HasConfigurations
        => ProtocolMagic
        -> NodeDBs
        -> IO (RealModeContext ())
    newRealModeContext pm dbs = do
        loggerName <- askLoggerName
        nodeParams <- getNodeParams loggerName commonNodeArgs nodeArgs
        let (Just vssSK) = npUserSecret nodeParams ^. usVss
        let gtParams = gtSscParams commonNodeArgs vssSK (npBehaviorConfig nodeParams)
        bracketNodeResources @() nodeParams gtParams (txpGlobalSettings pm) (initNodeDBs pm epochSlots)
            $ \NodeResources{..} -> RealModeContext
                <$> pure dbs
                <*> pure nrSscState
                <*> pure nrTxpState
                <*> pure nrDlgState
                <*> jsonLogConfigFromHandle stdout
                <*> pure loggerName
                <*> pure nrContext
                <*> pure noReporter


genesisSpecToGenesisData :: GenesisSpec -> GeneratedGenesisData
genesisSpecToGenesisData spec =
    let
        gsCst             = gsProtocolConstants spec
        protocolConstants = genesisProtocolConstantsToProtocolConstants gsCst
        protocolMagic     = gpcProtocolMagic gsCst
        initializer       = gsInitializer spec
        balances          = gsAvvmDistr spec
    in
        withProtocolConstants protocolConstants $
            generateGenesisData protocolMagic initializer balances


genesisDataToWalletUserSecrets :: GeneratedGenesisData -> [(PassPhrase, WalletUserSecret)]
genesisDataToWalletUserSecrets genData =
    map poorSecretToWalletUserSecrets (gsPoorSecrets . ggdSecrets $ genData)
  where
    poorSecretToWalletUserSecrets :: PoorSecret -> (PassPhrase, WalletUserSecret)
    poorSecretToWalletUserSecrets secret =
        let
            walUserSecret = WalletUserSecret
                { _wusRootKey    = poorSecretToEncKey secret
                , _wusWalletName = "Genesis Test Wallet (Poor)"
                , _wusAccounts   = [(accountGenesisIndex, "Genesis Test Account")]
                , _wusAddrs      = [(accountGenesisIndex, wAddressGenesisIndex)]
                }
        in
            (mempty, walUserSecret)


-- | Generate an initial state for the integration tests
--
-- Example:
--    import Pos.Core.Configuration(defaultGenesisSpec)
--
--    generateInitialState defaultGenesisSpec
generateInitialState
    :: (CommonNodeArgs, NodeArgs, ExtraNodeArgs)
    -> GenesisSpec
    -> IO ()
generateInitialState args spec = do
    let wallets = genesisDataToWalletUserSecrets . genesisSpecToGenesisData $ spec
    runWalletWebMode args $ forM_ wallets (uncurry importWalletDo)


--      TEST_INTEGRATION_NODE_PATH   = Path to a valid rocksdb database
--      TEST_INTEGRATION_WALLET_PATH = Path to a valid acid-state database
--      TEST_INTEGRATIoN_DB_PATH     = Path to directory with all DBs used by the node
main :: IO ()
main = do
    nodePath    <- lookupEnvE "INTEGRATION_TESTS_NODE_PATH"
    walletPath  <- lookupEnvE "INTEGRATION_TESTS_WALLET_PATH"
    dbPath      <- lookupEnvE "INTEGRATION_TESTS_DB_PATH"

    let args =
            ( def
                { dbPath    = Just dbPath
                , rebuildDB = True
                }
            , def
            , ExtraNodeArgs
                { _nodePath   = nodePath
                , _walletPath = walletPath
                }
            )

    generateInitialState args defaultGenesisSpec
  where
    lookupEnvE :: String -> IO String
    lookupEnvE var = do
        let msg = "Missing required ENV var '" <> var <> "'"
        maybe (fail msg) return =<< lookupEnv var
