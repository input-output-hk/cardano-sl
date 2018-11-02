{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Universum

import           Cardano.Wallet.Client.Http (WalletClient)
import           Network.HTTP.Client (Manager)
import qualified QuickCheckSpecs as QuickCheck
import           System.Environment (withArgs)
import           Test.Hspec (Spec, hspec)

import           Pos.Crypto.Configuration (ProtocolMagic (..),
                     ProtocolMagicId (..),
                     RequiresNetworkMagic (RequiresNoMagic))

import           AccountSpecs (accountSpecs)
import           AddressSpecs (addressSpecs)
import           CLI (CLIOptions (..), getOptions)
import           HardwareWalletSpecs (hardwareWalletSpecs)
import           RandomStateWalk (randomStateWalkTest)
import           SetupTestEnv (setupClient)
import           System.IO (hSetEncoding, stdout, utf8)
import           TransactionSpecs (transactionSpecs)
import           Util (WalletRef, newWalletRef, printT)
import           WalletSpecs (walletSpecs)

{-
This hardcoded value of protocolMagic is taken from lib/configuration.yaml.
The protocolMagic is needed in the hardware wallet tests for running crypt-functions
outside of the wallet (simmulating an external ledger device).
TODO: remove this hardcoded value in favour of a CLI argument.
TODO: add a test for validating the ProtocalMagic.
-}

unsafeProtocolMagic :: ProtocolMagic
unsafeProtocolMagic = ProtocolMagic
    { getProtocolMagicId = ProtocolMagicId 55550001
    , getRequiresNetworkMagic = RequiresNoMagic
    }

-- | Here we want to run main when the (local) nodes have started.
main :: IO ()
main = do
    hSetEncoding stdout utf8
    options@CLIOptions {..} <- getOptions

    printT "Starting the integration testing for wallet."
    (walletClient, manager) <- setupClient options
    -- Acquire the initial state for the deterministic tests
    wRef <- newWalletRef

-- main uses its own argument parsing.
-- Don't use 'hspec $ do' it would cause strange error messages,
-- because hspec would try to interpret the integration-test cmd arguments.
    withArgs hSpecOptions $ do
        printT "Starting deterministic tests."
        printT $ "HSpec options: " <> show hSpecOptions
        hspec $ deterministicTests wRef walletClient manager

        printT $ "The 'runActionCheck' tests were disabled because they were highly un-reliable."
        when False $ randomStateWalkTest walletClient

deterministicTests :: WalletRef -> WalletClient IO -> Manager -> Spec
deterministicTests wref wc manager = do
    accountSpecs wref wc
    addressSpecs wref wc
    walletSpecs wref wc
    transactionSpecs wref wc
    hardwareWalletSpecs unsafeProtocolMagic 1 wc
    QuickCheck.mkSpec manager
