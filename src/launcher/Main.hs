{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent.Async (async, cancel, wait, waitAny)
import qualified Data.Text                as T
import           Options.Applicative      (Parser, ParserInfo, execParser, fullDesc, help,
                                           helper, info, long, metavar, progDesc,
                                           strOption)
import           Prelude                  hiding (FilePath)
import           System.Process.Internals (translate)
import           Turtle

data LauncherOptions = LO
    { loNode          :: Text
    , loWallet        :: Maybe Text
    , loUpdater       :: Text
    , loUpdateArchive :: FilePath
    }

optsParser :: Parser LauncherOptions
optsParser = LO <$> nodeOpt <*> walletOpt <*> updaterOpt <*> updateArchiveOpt
  where
    nodeOpt = T.pack <$>
      strOption (long "node" <>
                 metavar "CMD" <>
                 help "Command that starts the node")
    walletOpt = optional $ T.pack <$>
      strOption (long "wallet" <>
                 metavar "CMD" <>
                 help "Command that starts the wallet")
    updaterOpt = T.pack <$>
      strOption (long "updater" <>
                 metavar "CMD" <>
                 help "Command that starts the updater")
    updateArchiveOpt = fromString <$>
      strOption (long "update-archive" <>
                 metavar "PATH" <>
                 help "Contents of the update")

optsInfo :: ParserInfo LauncherOptions
optsInfo = info (helper <*> optsParser) $
    fullDesc `mappend` progDesc "Tool to launch Cardano SL"

main :: IO ()
main = do
    LO {..} <- execParser optsInfo
    case loWallet of
        Nothing -> serverScenario loNode (loUpdater, loUpdateArchive)
        Just wp -> clientScenario loNode wp (loUpdater, loUpdateArchive)

-- | If we are on server, we want the following algorithm:
--
-- * Update (if we are already up-to-date, nothing will happen).
-- * Launch the node.
-- * If it exits with code 20, then update and restart, else quit.
serverScenario :: Text -> (Text, FilePath) -> IO ()
serverScenario nodeCmd upd = do
    runUpdater upd
    -- TODO: somehow signal updater failure if it fails? would be nice to
    -- write it into the log, at least
    exitCode <- wait =<< async (shell nodeCmd mempty)
    case exitCode of
        ExitFailure 20 -> serverScenario nodeCmd upd
        _              -> return ()

-- | If we are on desktop, we want the following algorithm:
--
-- * Update (if we are already up-to-date, nothing will happen).
-- * Launch the node and the wallet.
-- * If the wallet exits with code 20, then update and restart, else quit.
clientScenario :: Text -> Text -> (Text, FilePath) -> IO ()
clientScenario nodeCmd walletCmd upd = do
    runUpdater upd
    nodeAsync   <- async (shell nodeCmd mempty)
    walletAsync <- async (shell walletCmd mempty)
    (someAsync, exitCode) <- waitAny [nodeAsync, walletAsync]
    if | someAsync == nodeAsync ->
             -- when the node exits, just wait for wallet to die
             void $ wait walletAsync
       | exitCode == ExitFailure 20 -> do
             -- when the wallet exits with 20, kill the node and update
             cancel nodeAsync
             clientScenario nodeCmd walletCmd upd
       | otherwise ->
             -- otherwise just kill the node
             cancel nodeAsync

-- | We run the updater and delete the update file if the update was
-- successful.
runUpdater :: (Text, FilePath) -> IO ()
runUpdater (updaterCmd, updateArchive) = do
    let cmd = updaterCmd <> " " <>
              T.pack (translate (T.unpack (format fp updateArchive)))
    exitCode <- shell cmd mempty
    when (exitCode == ExitSuccess) $
        -- this will throw an exception if the file doesn't exist but
        -- hopefully if the updater has succeeded it *does* exist
        rm updateArchive
