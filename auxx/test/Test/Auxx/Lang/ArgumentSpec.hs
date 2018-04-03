{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE OverloadedLists #-}

module Test.Auxx.Lang.ArgumentSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec (Expectation, Spec, describe, it, shouldBe)

import           Command.TyProjection (tyInt, tyString)
import           Lang.Argument (ArgumentConsumer, ArgumentError (..), ProcError (..),
                                TypeError (..), consumeArguments, getArg)
import           Lang.Syntax (Arg (..))
import           Lang.Value (Value (..))

spec :: Spec
spec = describe "Auxx.Lang.Argument" $ do
    describe "Example 1" $ do
        let s = ValueString "patak"
            i = ValueNumber 42
        it "handles positional arguments" $
            unitSuccessExample1 [ArgPos s, ArgPos i]
        it "handles ordered keyword arguments" $
            unitSuccessExample1 [ArgKw "s" s, ArgKw "i" i]
        it "handles disordered keyword arguments" $
            unitSuccessExample1 [ArgKw "i" i, ArgKw "s" s]
        it "handles ordered positional+keyword arguments" $
            unitSuccessExample1 [ArgPos s, ArgKw "i" i]
        it "handles ordered keyword+positional arguments" $
            unitSuccessExample1 [ArgKw "s" s, ArgPos i]
        it "handles disordered keyword+positional arguments" $
            unitSuccessExample1 [ArgKw "i" i, ArgPos s]
        it "reports multiple type errors" $
            unitFailureExample1 [ArgKw "i" s, ArgKw "s" i]
            mempty { peTypeErrors = [TypeError "String" i, TypeError "Int" s] }
        it "reports irrelevant+missing keyword arguments" $
            unitFailureExample1 [ArgKw "x" ValueUnit, ArgKw "i" i, ArgKw "y" ValueUnit]
            mempty { peArgumentError =
                        mempty { aeMissingKeys = ["s"]
                               , aeIrrelevantKeys = ["x", "y"]
                               }
                   }

unitSuccessExample1 :: [Arg Value] -> Expectation
unitSuccessExample1 args = do
    consumeArguments acExample1 args `shouldBe`
        Right ("patak", 42)

unitFailureExample1 :: [Arg Value] -> ProcError -> Expectation
unitFailureExample1 args err = do
    consumeArguments acExample1 args `shouldBe`
        Left err

acExample1 :: ArgumentConsumer (Text, Int)
acExample1 = do
    x <- getArg tyString "s"
    y <- getArg tyInt "i"
    return (x, y)
