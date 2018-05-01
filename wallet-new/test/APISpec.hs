{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NumDecimals          #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module APISpec (spec) where

import qualified Prelude
import           Universum

import           Control.Concurrent (threadDelay)
import           Data.Default (def)
import           Data.List.NonEmpty (fromList)
import           Data.Maybe (fromJust)

import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Time.Units (Microsecond, fromMicroseconds)
import           Formatting (format, shown, string, (%))
import           Network.HTTP.Client hiding (Proxy)
import           Network.HTTP.Types
import           System.Wlog (HasLoggerName (..), LoggerName (..))

import           Pos.Block.Types (Blund)
import           Pos.Client.CLI (CommonArgs (..), CommonNodeArgs (..), NodeArgs (..), getNodeParams,
                                 gtSscParams)
import           Pos.Core (GenesisBlock, MainBlock, ProtocolMagic, Timestamp (..), headerHash,
                           protocolMagic)
import           Pos.DB.DB (initNodeDBs)
import           Pos.DB.Rocks.Functions (openNodeDBs)
import           Pos.DB.Rocks.Types (NodeDBs)
import qualified Pos.Diffusion.Types as D
import           Pos.Launcher (ConfigurationOptions (..), HasConfigurations, NodeResources (..),
                               allocateNodeResources, defaultConfigurationOptions, npBehaviorConfig,
                               npUserSecret, withConfigurations)
import           Pos.Network.CLI (NetworkConfigOpts (..))
import           Pos.Txp (txpGlobalSettings)
import           Pos.Util.CompileInfo (HasCompileInfo, retrieveCompileTimeInfo, withCompileInfo)

import           Cardano.Wallet.API.V1.LegacyHandlers.Accounts (newAccount)
import           Cardano.Wallet.API.V1.LegacyHandlers.Addresses (newAddress)
import           Cardano.Wallet.API.V1.LegacyHandlers.Wallets (newWallet)

import           Pos.DB.GState.Common (getTip, initGStateCommon, setInitialized)
import           Pos.StateLock (StateLock (..))
import           Pos.Wallet.Web.Mode (MonadWalletWebMode, WalletWebMode, WalletWebModeContext (..))
import           Pos.Wallet.Web.State (WalletDB, openState)

import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text
import           Ntp.Client (withoutNtpClient)

import           Pos.WorkMode (RealModeContext (..))

import           Pos.DB.Block (prepareBlockDB, putBlunds)

import           Pos.Util.Util (lensOf)
import           System.Directory (createDirectoryIfMissing, doesPathExist, getCurrentDirectory,
                                   removeDirectoryRecursive)

import           TestUtil (BlockNumber, SlotsPerEpoch, createEmptyUndo,
                           produceBlocksByBlockNumberAndSlots, produceSecretKeys,
                           produceSlotLeaders)

import           Mockable (Production, runProduction)
import           Pos.Util.JsonLog (jsonLogConfigFromHandle)
import           Pos.Util.UserSecret (usVss)

import           Servant
import           Servant.QuickCheck
import           Servant.QuickCheck.Internal

import           System.Directory

import           Test.Hspec
import           Test.Pos.Configuration (withDefConfigurations)
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response (WalletResponse (..))
import           Cardano.Wallet.API.Types
import qualified Cardano.Wallet.API.V1 as V0
import qualified Cardano.Wallet.API.V1 as V1
import qualified Cardano.Wallet.API.V1.LegacyHandlers as V0
import qualified Cardano.Wallet.API.V1.LegacyHandlers as V1
import qualified Cardano.Wallet.API.V1.Migration as Migration
import           Cardano.Wallet.API.V1.Parameters
import           Cardano.Wallet.API.V1.Types (Wallet (..))

-- Our API apparently is returning JSON Arrays which is considered bad practice as very old
-- browsers can be hacked: https://haacked.com/archive/2009/06/25/json-hijacking.aspx/
-- The general consensus, after discussing this with the team, is that we can be moderately safe.
-- stack test cardano-sl-wallet-new --fast --test-arguments '-m "Servant API Properties"'
spec :: Spec
spec = do
    describe "Servant Layout" $ around_ withTestDirectory $ do
        let layoutPath = "./test/golden/api-layout.txt"
            newLayoutPath = layoutPath <> ".new"
        it "has not changed" $ do
            oldLayout <- BS.readFile layoutPath `catch` \(_err :: SomeException) -> pure ""
            when (oldLayout /= serverLayout) $ do
                BS.writeFile newLayoutPath serverLayout
                expectationFailure $ Prelude.unlines
                    [ "The API layout has changed!!! The new layout has been written to:"
                    , "    " <> newLayoutPath
                    , "If this was intentional and correct, move the new layout path to:"
                    , "    " <> layoutPath
                    , "Command:"
                    , "    mv " <> newLayoutPath <> " " <> layoutPath
                    ]

-- | This is a hack that sets the CWD to the correct directory to access
-- golden tests. `stack` will run tests at the top level of the git
-- project, while `cabal` and the Nix CI will run tests at the `wallet-new`
-- directory. This function ensures that we are in the `wallet-new`
-- directory for the execution of this test.
withTestDirectory :: IO () -> IO ()
withTestDirectory action = void . runMaybeT $ do
    dir <- lift getCurrentDirectory
    entries <- lift $ listDirectory dir
    guard ("cardano-sl-wallet-new.cabal" `notElem` entries)
    guard ("wallet-new" `elem` entries)
    lift $ do
        bracket_ (setCurrentDirectory =<< makeAbsolute "wallet-new")
                 (setCurrentDirectory dir)
                 action

serverLayout :: ByteString
serverLayout = Text.encodeUtf8 (layout (Proxy @V1.API))
