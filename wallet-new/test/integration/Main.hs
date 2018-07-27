module Main where

import           Universum

import           Test.Hspec (before_, describe, hspec, it, shouldBe)

import           Clients (WWebModeRunner (..), mkWWebModeRunner)
import           Fixtures (generateInitialState)


-- INTEGRATION_TESTS_NODE_PATH    = Path to a valid rocksdb database
-- INTEGRATION_TESTS_WALLET_PATH  = Path to a valid acid-state database
-- INTEGRATION_TESTS_DB_PATH      = Path to directory with all DBs used by the node
-- INTEGRATION_TESTS_CONFIG_PATH  = Path to the yaml configuration file
-- INTEGRATION_TESTS_CONFIG_KEY   = Key to use within that config file (e.g. development, test)
-- INTEGRATION_TESTS_SYSTEM_START = Timestamp at which the system has started, in us
main :: IO ()
main = do
    WWebModeRunner{..} <- mkWWebModeRunner -- Get its config from ENV

    hspec $ before_ (runWWebMode generateInitialState) $ describe "Integration Tests" $ do
        it "A first test" $
            True `shouldBe` True

        it "A second test" $
            False `shouldBe` False
