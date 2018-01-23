{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Universum
import Formatting (format, build)
import Test.Hspec

import UTxO.Bootstrap
import UTxO.Context
import UTxO.DSL
import UTxO.Interpreter
import UTxO.Translate

main :: IO ()
main = do
    putStrLn $ runTranslate (ask >>= \tc -> withConfig $ return (format build tc))
    putStrLn $ runTranslate (ask >>= \tc -> return $ dumpStr (bootstrapTransaction tc))
    hspec tests

tests :: Spec
tests = describe "Wallet unit tests" $ do
    testSanityChecks

testSanityChecks :: Spec
testSanityChecks = describe "Test sanity checks" $ do
    it "can construct and verify empty block" $
      shouldBeValid emptyBlock

    it "can construct and verify block with one transaction" $
      shouldBeValid oneTrans
  where
    emptyBlock :: Chain Addr
    emptyBlock = Chain [[]]

    -- TODO: We need access to the "bootstrap transaction".
    -- TODO: We also need to make the rest available!
    -- TODO: We should also sanity check that the translations are valid
    -- according to the DSL definition of validity.
    t1 :: Transaction Addr
    t1 = Transaction {
             trIns  = []
           , trOuts = [Output (AddrOrdinary (Addr (IxRich 2) 0)) 10]
           }

    oneTrans :: Chain Addr
    oneTrans = Chain [[t1]]

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

shouldBeValid :: Chain Addr -> Expectation
shouldBeValid chain =
    isRight (runTranslate (int chain >>= verifyBlocksPrefix))
      `shouldBe` True
