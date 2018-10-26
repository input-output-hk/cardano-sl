{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Universum

import           GHC.Conc (getNumProcessors, setNumCapabilities)
import           Test.Hspec (Spec, after, around, before, describe, hspec, it,
                     parallel)
import           Test.QuickCheck (expectFailure, once, withMaxSuccess)

import           TicketDispenser
import           Wallet

------------------------------------------------------------------------

tests :: Spec
tests =
    describe "Tests" $ parallel $ do
        -- NOTE: ticket dispenser example is provided by quickcheck-state-machine lib
        -- This serves as an reference example. It will be removed
--        before setupLock $ after cleanupLock $ do
--            describe "Ticket dispenser" $ do
--                it "sequential" $ prop_ticketDispenser
--                it "parallel with exclusive lock" $ withMaxSuccess 30 . prop_ticketDispenserParallelOK
--                it "parallel with shared lock" $ expectFailure . prop_ticketDispenserParallelBad
        -- TODO: test wallet layer which is not in memory. Maybe there are race condition bugs when we are using filesystem persisted db instead of in-memory one
        around (withWalletLayer . curry) $ do
            describe "Wallet" $ do
                it "normal postcondition failure" $ withMaxSuccess 10 . prop_fail
                it "sqlite postcondition failure?" $ withMaxSuccess 100 . prop_wallet
--                it "parallel" $ withMaxSuccess 30 . prop_walletParallel

------------------------------------------------------------------------

-- TODO: factor out functionality from Parallel.Parallelize module into a lib and reuse
parallelizeAllCores :: IO ()
parallelizeAllCores = getNumProcessors >>= setNumCapabilities

main :: IO ()
main = do
--    parallelizeAllCores
    hspec tests
