{-# LANGUAGE LambdaCase #-}

module Main where

import Turtle hiding (proc)
import Prelude hiding (FilePath)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Filesystem.Path.CurrentOS as FP
import System.Process
import System.IO (openFile, IOMode(..))
import Control.Exception (finally)
import Control.Concurrent.Async (concurrently_)

parser :: Parser Text
parser = T.toLower <$> argText "network" "Network to run test against"

main :: IO ()
main = do
  network <- options "Acceptance tests script" parser
  let st = format ("state-acceptance-test-"%s) network
      -- This will limit heap size to 1GB, along with the usual RTS options.
      rts = "-N2 -qg -A1m -I0 -T -M1G -h";

  setupState rts network st

  ph <- launchWallet rts network st

  getPid ph >>= \case
    Just pid -> concurrently_
      (waitForWallet ph)
      (acceptanceTests network st pid `finally` terminateProcess ph)
    Nothing -> putStrLn "Wallet did not start successfully"

launchWallet :: Text -> Text -> Text -> IO ProcessHandle
launchWallet rts network st = do
  let args = walletArgs Sync rts network st
  T.putStrLn (T.unwords (connectScript:args))
  h <- openFile (FP.encodeString $ walletLogFile st) WriteMode
  (_,_,_,p) <- createProcess $
    (proc (T.unpack connectScript) (map T.unpack args))
    { std_out = UseHandle h, std_err = UseHandle h }
  pure p

walletLogFile :: Text -> FilePath
walletLogFile st = FP.fromText st </> "logs" </> "wallet.log"

setupState :: Text -> Text -> Text -> IO ()
setupState rts network st = run connectScript $ walletArgs Setup rts network st

data Phase = Setup | Sync
  deriving (Show, Eq)

walletListen :: Text
walletListen = "localhost:8090"

walletArgs :: Phase -> Text -> Text -> Text -> [Text]
walletArgs phase rts network st = [ phaseArg, "wallet", network
                                  , "--state-dir", st
                                  , "--rts", rts ] ++ deleteArg ++
                                  [ "--", "--wallet-address", walletListen ]
  where
    phaseArg = case phase of
      Setup -> "--only-setup"
      Sync -> "--skip-setup"
    deleteArg = if phase == Setup then ["--delete-state"] else []

connectScript :: Text
connectScript = "cardano-sl-connect"

backupPhrase :: Text
backupPhrase = "session ring phone arrange notice gap media olympic water road spider rate"

walletID :: Text
walletID = "2cWKMJemoBakcSaWXEpvRNiAsnsVaNFkyVHaxxPghSZizwVaLqpGebJwjYSG6q1f9sw5i"

acceptanceTests :: Text -> Text -> Pid -> IO ()
acceptanceTests network st pid = do
  printf ("Running acceptance tests for "%s%"\n") network

  let pidArg = [ "--pid", format d pid ]
      walletTool args = run "cardano-sl-wallet-tool" $ [ "--state-dir", st, walletListen ] ++ args

  walletTool $ ["wait-for-sync"] ++ pidArg ++ ["--out", "sync-stats.json"]

  putStrLn "\nGoing to restore a wallet"
  walletTool ("restore-wallet":["--backup-phrase", backupPhrase, "--name", "Acceptance Wallet"])

  putStrLn "\nNow waiting for restore to complete"
  walletTool $ ["wait-for-restore"] ++ pidArg ++ ["--out", "restore-stats.json"]

run :: Text -> [Text] -> IO ()
run exe args = do
  T.putStrLn (T.unwords (exe:args))
  procs exe args empty

waitForWallet :: ProcessHandle -> IO ()
waitForWallet ph = waitForProcess ph >>= \case
  ExitSuccess -> putStrLn "Wallet exited successfully."
  ExitFailure c -> printf ("Wallet exited with status "%d%"\n") c

-- would be good to have this
-- function bomb() {
--   echo
--   echo "***"
--   echo "*** Here are the last 200 lines of ${stateDir}/logs/wallet.log"
--   echo "***"
--   tail -n200 ${stateDir}/logs/wallet.log
--   echo
--   echo "***"
--   echo "*** Wallet is no longer running -- exiting"
--   echo "***"
--   exit 1
-- }
