-- | Wallet unit tests
module Main (main) where

import           Universum

import           Formatting (build, sformat)
import           Test.Hspec (Spec, describe, hspec)

import           Pos.Core (HasConfiguration)

import           InputSelection.Evaluation (evaluateInputPolicies)
import           InputSelection.Evaluation.Generic (defaultPlotParams)
import           UTxO.Bootstrap (bootstrapTransaction)
import           UTxO.Context (Addr, TransCtxt)
import           UTxO.DSL (GivenHash, Transaction)
import           UTxO.Translate (runTranslateNoErrors, withConfig)

import qualified Test.Spec.CoinSelection
import qualified Test.Spec.Kernel
import qualified Test.Spec.Models
import qualified Test.Spec.Submission
import qualified Test.Spec.Translation
import qualified Test.Spec.WalletWorker
import           TxMetaStorageSpecs (txMetaStorageSpecs)

{-------------------------------------------------------------------------------
  Main test driver
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    args <- getArgs
    case args of
      ["--show-context"] ->
        showContext
      ["--evaluate-input-policies", prefix] ->
        evaluateInputPolicies (defaultPlotParams prefix)
      _otherwise ->
        runTranslateNoErrors $ withConfig $ return $ hspec tests

-- | Debugging: show the translation context
showContext :: IO ()
showContext = do
    putStrLn $ runTranslateNoErrors $ withConfig $
      sformat build <$> ask
    putStrLn $ runTranslateNoErrors $
      let bootstrapTransaction' :: TransCtxt -> Transaction GivenHash Addr
          bootstrapTransaction' = bootstrapTransaction
      in sformat build . bootstrapTransaction' <$> ask

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

tests :: Spec
tests = describe "Wallet unit tests" $ do
    Test.Spec.Translation.spec
    Test.Spec.Models.spec
    Test.Spec.Kernel.spec
    Test.Spec.WalletWorker.spec
    Test.Spec.Submission.spec
    txMetaStorageSpecs
    Test.Spec.CoinSelection.spec
