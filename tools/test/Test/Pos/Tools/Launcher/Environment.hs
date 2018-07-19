{-# LANGUAGE OverloadedStrings #-}
module Test.Pos.Tools.Launcher.Environment
       ( spec
       ) where

import           Test.Hspec

import qualified Data.Aeson as AE
import           Pos.Tools.Launcher.Environment (substituteEnvVarsValue)
import qualified System.Directory as Sys
import           System.Environment (setEnv)
import           System.IO.Temp

spec :: Spec
spec = describe "Test.Pos.Tools.Launcher.Environment" $ do
    it "substitutes defined vars" unitParserSample1
    it "fails on undefined vars"  unitParserSample2

unitParserSample1 :: Expectation
unitParserSample1 = action `shouldReturn` AE.String expected
  where
    input    = "this ${FOO} a drill"
    expected = "this is not a drill"
    action = do
      setEnv "FOO" "is not"
      substituteEnvVarsValue "Catching a fly" $ AE.String input

unitParserSample2 :: Expectation
unitParserSample2 = action `shouldThrow` errorCall "Catching another fly\nReference to an undefined environment variable 'PLEASE_DONT_SET_THIS_VAR_OR_ELSE'"
  where
    input    = "this ${PLEASE_DONT_SET_THIS_VAR_OR_ELSE} a drill"
    action = do
      withSystemTempDirectory "test-XXXXXX" $
        \tmpdir-> do
          setEnv "HOME" tmpdir
          Sys.getXdgDirectory Sys.XdgData "" >>= setEnv "XDG_DATA_HOME"
          substituteEnvVarsValue "Catching another fly" $ AE.String input
