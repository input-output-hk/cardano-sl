module Test.Auxx.Lang.ParserSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec (Expectation, Spec, describe, it, shouldSatisfy)

import           Lang.Parser (parse)

spec :: Spec
spec = describe "Auxx.Lang.Parser" $ do
    it "handles sample-1" unitParserSample1

unitParserSample1 :: Expectation
unitParserSample1 = parse input `shouldSatisfy` isRight
  where
    input = "wait (epoch 6);; propose-patak kek:15 \"lel\" mem:(epoch 7) zaz:(get (rekt mem: to-next ses: \"zaz\" ))"
