{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Universum

import           Test.Hspec (Spec, describe, hspec, it)
import           Test.QuickCheck (withMaxSuccess)

import           Cardano.Wallet.Kernel.Internal (PassiveWallet)
import qualified Cardano.Wallet.WalletLayer as WL

import           Wallet

------------------------------------------------------------------------

tests :: WL.PassiveWalletLayer IO -> PassiveWallet -> Spec
tests pwl pw =
    describe "Tests" $ do
       describe "Wallet" $ do
            it "sequential" $ withMaxSuccess 10 $ prop_wallet pwl pw

------------------------------------------------------------------------

main :: IO ()
main =
    -- see CO-438 why we didn't do this with hspec's `around`
    withWalletLayer $ \pwl pw -> hspec $ tests pwl pw
