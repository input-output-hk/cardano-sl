module Main where

import           Universum

import           Cardano.Wallet.Client.Http (WalletClient)
import           Network.HTTP.Client (Manager)
import qualified QuickCheckSpecs as QuickCheck
import           System.Environment (withArgs)
import           Test.Hspec (Spec, hspec)

import           AccountSpecs (accountSpecs)
import           AddressSpecs (addressSpecs)
import           RandomStateWalk (randomStateWalkTest)
import           SetupTestEnv (setupClients)
import           TransactionSpecs (transactionSpecs)
import           WalletSpecs (externalWalletSpecs, internalWalletSpecs)

main :: IO ()
main = do
    clients@((client0, _),_ ,_ ,_) <- setupClients
    randomStateWalkTest client0

    -- NOTE Our own CLI options interfere with `hspec` which parse them for
    -- itself when executed, leading to a VERY unclear message:
    --
    --     cardano-integration-test: unrecognized option `--tls-ca-cert'
    --     Try `cardano-integration-test --help' for more information.
    --
    -- See also: https://github.com/hspec/hspec/issues/135
    withArgs [] . hspec $ integrationTests clients

integrationTests
  :: ((WalletClient IO, Manager)
     ,(WalletClient IO, Manager)
     ,(WalletClient IO, Manager)
     ,(WalletClient IO, Manager))
  -> Spec
integrationTests ((wc0, m0),(wc1, _) ,(wc2, _) ,(wc3, _)) = do
    accountSpecs wc0
    addressSpecs wc0
    internalWalletSpecs  wc0
    externalWalletSpecs  wc0
    transactionSpecs (wc0, wc1, wc2, wc3)
    QuickCheck.mkSpec m0
