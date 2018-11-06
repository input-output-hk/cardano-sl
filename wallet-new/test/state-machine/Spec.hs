{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Universum

import           Test.Hspec (Spec, after, around, before, describe, hspec, it)
import           Test.QuickCheck (expectFailure, withMaxSuccess)

import           Cardano.Wallet.Kernel.Internal (PassiveWallet)
import qualified Cardano.Wallet.WalletLayer as WL

import           TicketDispenser
import           Wallet

------------------------------------------------------------------------

tests :: WL.PassiveWalletLayer IO -> PassiveWallet -> Spec
tests pwl pw =
    describe "Tests" $ do
        -- NOTE: ticket dispenser example is provided by quickcheck-state-machine lib
        -- This serves as an reference example. It will be removed
--        before setupLock $ after cleanupLock $ do
--            describe "Ticket dispenser" $ do
--                it "sequential" $ prop_ticketDispenser
--                it "parallel with exclusive lock" $ withMaxSuccess 30 . prop_ticketDispenserParallelOK
--                it "parallel with shared lock" $ expectFailure . prop_ticketDispenserParallelBad
        -- TODO: test wallet layer which is not in memory. Maybe there are race condition bugs when we are using filesystem persisted db instead of in-memory one
        describe "Wallet" $ do
            it "sequential" $ withMaxSuccess 300 $ prop_wallet pwl pw
            -- TODO: check how would Test.QuickCheck.parallel and including multiple cores
            -- react to parallel executing from quickcheck-state-machine
--                it "parallel" $ withMaxSuccess 3 . prop_walletParallel

------------------------------------------------------------------------

main :: IO ()
main =
    -- see CO-438 why we didn't do this with hspec's `around`
    withWalletLayer $ \pwl pw -> hspec $ tests pwl pw
