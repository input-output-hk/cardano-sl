module Main where

import Launcher

import qualified Data.ByteString as BS
import qualified Data.Yaml as Y
import Data.Yaml ((.:), (.:?), (.!=))
import qualified Filesystem.Path as FP
import Filesystem.Path.CurrentOS (fromText)
import System.Environment (getEnv)
import System.FilePath ((</>))

instance Y.FromJSON LauncherOptions where
  parseJSON (Y.Object v) =
    LO <$>
    v .: "loNodePath" <*>
    v .:? "loNodeArgs" .!= [] <*>
    v .:? "loNodeLogConfig" <*>
    v .:? "loNodeLogPath" <*>
    v .:? "loWalletPath" <*>
    v .:? "loWalletArgs" .!= [] <*>
    v .: "loUpdaterPath" <*>
    v .:? "loUpdaterArgs" .!= [] <*>
    v .:? "loUpdateArchive" <*>
    v .: "loNodeTimeoutSec" <*>
    v .:? "loReportServer"
  parseJSON _ = fail "Expected Object for LauncherOptions value"

instance Y.FromJSON FP.FilePath where
  parseJSON (Y.String v) = pure $ fromText v
  parseJSON _ = fail "Expected String for FilePath value"

config :: IO BS.ByteString
config = do
  appData <- getEnv "APPDATA"
  yaml <- BS.readFile $ appData </> "Daedalus" </> "launcherConfig.yaml"
  return yaml

main :: IO ()
main = do
  yaml <- config
  case Y.decode yaml of
    Just options -> do
      print options
      launch options
    Nothing -> fail "Invalid configuration!"
