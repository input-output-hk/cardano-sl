{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent.Async (cancel, waitAny)
import           Options.Applicative      (Parser, ParserInfo, execParser, fullDesc, help,
                                           helper, info, long, metavar, progDesc,
                                           strOption)
import           System.Process.Internals (translate)
import           Turtle                   hiding (toText)
import           Universum                hiding (FilePath)

data LauncherOptions = LO
    { loNode          :: Text
    , loWallet        :: Maybe Text
    , loUpdater       :: Text
    , loUpdateArchive :: FilePath
    }

optsParser :: Parser LauncherOptions
optsParser = LO <$> nodeOpt <*> walletOpt <*> updaterOpt <*> updateArchiveOpt
  where
    nodeOpt = toText <$>
      strOption (long "node" <>
                 metavar "CMD" <>
                 help "Command that starts the node")
    walletOpt = optional $ toText <$>
      strOption (long "wallet" <>
                 metavar "CMD" <>
                 help "Command that starts the wallet")
    updaterOpt = toText <$>
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
    sh $ case loWallet of
        Nothing -> serverScenario loNode (loUpdater, loUpdateArchive)
        Just wp -> clientScenario loNode wp (loUpdater, loUpdateArchive)

-- | If we are on server, we want the following algorithm:
--
-- * Update (if we are already up-to-date, nothing will happen).
-- * Launch the node.
-- * If it exits with code 20, then update and restart, else quit.
serverScenario :: Text -> (Text, FilePath) -> Shell ()
serverScenario nodeCmd upd = do
    echo "Running the updater"
    runUpdater upd
    -- TODO: somehow signal updater failure if it fails? would be nice to
    -- write it into the log, at least
    echo "Starting the node"
    exitCode <- wait =<< fork (shell nodeCmd mempty)
    printf ("The node has exited with "%s%"\n") (show exitCode)
    case exitCode of
        ExitFailure 20 -> serverScenario nodeCmd upd
        _              -> return ()

-- | If we are on desktop, we want the following algorithm:
--
-- * Update (if we are already up-to-date, nothing will happen).
-- * Launch the node and the wallet.
-- * If the wallet exits with code 20, then update and restart, else quit.
clientScenario :: Text -> Text -> (Text, FilePath) -> Shell ()
clientScenario nodeCmd walletCmd upd = do
    echo "Running the updater"
    runUpdater upd
    echo "Starting the node"
    nodeAsync <- fork (shell nodeCmd mempty)
    echo "Starting the wallet"
    walletAsync <- fork (shell walletCmd mempty)
    (someAsync, exitCode) <- liftIO $ waitAny [nodeAsync, walletAsync]
    if | someAsync == nodeAsync -> do
             printf ("The node has exited with "%s%"\n") (show exitCode)
             echo "Waiting for the wallet to die"
             void $ wait walletAsync
       | exitCode == ExitFailure 20 -> do
             echo "The wallet has exited with code 20"
             echo "Killing the node"
             liftIO $ cancel nodeAsync
             clientScenario nodeCmd walletCmd upd
       | otherwise -> do
             printf ("The wallet has exited with "%s%"\n") (show exitCode)
             liftIO $ cancel nodeAsync

-- | We run the updater and delete the update file if the update was
-- successful.
runUpdater :: (Text, FilePath) -> Shell ()
runUpdater (updaterCmd, updateArchive) = do
    let cmd = updaterCmd <> " " <>
              toText (translate (toString (format fp updateArchive)))
    exitCode <- shell cmd mempty
    printf ("The updater has exited with "%s%"\n") (show exitCode)
    when (exitCode == ExitSuccess) $ do
        -- this will throw an exception if the file doesn't exist but
        -- hopefully if the updater has succeeded it *does* exist
        rm updateArchive
