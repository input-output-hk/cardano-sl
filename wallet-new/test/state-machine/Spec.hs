{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Universum

import           Test.Hspec (Spec, after, around, before, describe, hspec, it,
                     parallel)
import           Test.QuickCheck (expectFailure, withMaxSuccess)

import           TicketDispenser
import           Wallet

------------------------------------------------------------------------

tests :: Spec
tests =
    describe "Tests" $ parallel $ do
        -- NOTE: ticket dispenser example is provided by quickcheck-state-machine lib
        -- This serves as an reference example. It will be removed
        before setupLock $ after cleanupLock $ do
            describe "Ticket dispenser" $ do
                it "sequential" $ prop_ticketDispenser
                it "parallel with exclusive lock" $ withMaxSuccess 30 . prop_ticketDispenserParallelOK
                it "parallel with shared lock" $ expectFailure . prop_ticketDispenserParallelBad
        around (withWalletLayer . curry) $ do
            describe "Wallet" $ do
                it "sequential" $ prop_wallet
                it "parallel" $ withMaxSuccess 2 . prop_walletParallel

------------------------------------------------------------------------

main :: IO ()
main = do
    -- TODO: enable more cores
    -- parallelizeAllCores
    hspec tests
