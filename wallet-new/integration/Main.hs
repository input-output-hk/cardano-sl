{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Universum

import           Cardano.Wallet.Client.Http (WalletClient)
import           Network.HTTP.Client (Manager)
import qualified QuickCheckSpecs as QuickCheck
import           System.Environment (withArgs)
import           Test.Hspec (Spec, hspec)

import           AccountSpecs (accountSpecs)
import           AddressSpecs (addressSpecs)
import           CLI (CLIOptions (..), getOptions)
import           RandomStateWalk (randomStateWalkTest)
import           SetupTestEnv (setupClient)
import           System.IO (hSetEncoding, stdout, utf8)
import           TransactionSpecs (transactionSpecs)
import           Util (WalletRef, newWalletRef, printT)
import           WalletSpecs (walletSpecs)

-- | Here we want to run main when the (local) nodes have started.
main :: IO ()
main = do
    hSetEncoding stdout utf8
    options@CLIOptions {..} <- getOptions

    printT "Starting the integration testing for wallet."
    (walletClient, manager) <- setupClient options
    -- Acquire the initial state for the deterministic tests
    wRef <- newWalletRef

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
    QuickCheck.mkSpec manager
