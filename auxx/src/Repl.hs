{-# LANGUAGE RankNTypes #-}
{- |

This module contains the Haskeline-based user interface of auxx. The REPL is
designed to run in a separate thread, while the commands are executed by a
separate plugin for the node. This architecture has several advantages over
alternating between user interaction and command execution in a single thread:

* from the user standpoint, the interface is more responsive: even while the
  node is in the process of initialization, the user can start typing commands
  immediately

* from the implementation standpoint, there are no concerns with combining monad
  transformer stacks of the node and the REPl, which is especially important
  since Haskeline defines a custom class to handle exceptions

* featurewise, this architecture makes it comparatively easy to add background
  commands in the future, although right now all interactions are synchronized

IF YOU WANT TO MODIFY THIS MODULE, a note on architecture: I made this module as
independent as possible from other parts of Cardano (including auxx). It doesn't
know how commands are executed, what commands there are, or how to parse them.
I find this rather nice, as it's possible to reason about the REPL in separation
from the rest of the system.

-}

module Repl
       ( WithCommandAction(..)
       , createAuxxRepl
       , withAuxxRepl
       , PrintAction
       ) where

import           Universum

import           Control.Concurrent.Async (race_)
import           Control.Exception.Safe (displayException, handleAsync)
import           Data.Text (strip)
import           System.Console.Haskeline (InputT)
import qualified System.Console.Haskeline as Haskeline

-- | An action used to print messages to the terminal. We can't hardcode
-- 'putText' because Haskeline defines its own printing method.
type PrintAction m = Text -> m ()

-- | The resurt of executing a command. In case there are no exceptions thrown,
-- the result is 'CommandSuccess', otherwise it's 'CommandFailure' with the
-- exception attached.
data CommandResult
    = CommandSuccess
    -- ^ The command has been executed and there were no uncaught exceptions.
    | CommandFailure SomeException
    -- ^ The command failed due to an uncaught exception.

-- | The external interface of the REPL, used by the auxx plugin to receive new
-- commands to execute (via 'withCommand') and get the Haskeline-compatible
-- printing action (via 'getPrintAction').
data WithCommandAction = WithCommandAction
    { withCommand    :: forall m. (MonadIO m, MonadCatch m) => (Text -> m ()) -> m ()
        -- ^ Get the next command to execute. Rather than using simple @m
        -- 'Command'@ method, we use a CPS-ed version to guarantee valid
        -- exception handling, automate 'CommandResult' detection, and avoid
        -- accidental deadlocks.
        --
        -- This action blocks until there are commands available.
        --
    , printAction :: forall m. MonadIO m => PrintAction m
        -- ^ Get the printing action that doesn't disrupt the Haskeline prompt.
        -- The node (and the auxx plugin) should use this action exclusively for printing.
        --
        -- This action blocks until Haskeline is initialized (happens almost immediately).
        --
    }

-- | The action to post a command to be executed by the auxx plugin and wait for
-- its result. This action blocks until the result is available: that is, until
-- command execution completes or an exception is thrown.
type PutCommandAction = Text -> AuxxRepl CommandResult

-- | Create the auxx REPL (as an abstract @'IO' ()@ action) and the interface to
-- interact with it ('WithCommandAction'). You are supposed to fork the REPL
-- action into a separate thread: 'withAuxxRepl' does it for you, so use it
-- instead unless you need to add special logic.
createAuxxRepl :: IO (WithCommandAction, IO ())
createAuxxRepl = do
    nextCommandVar <- newEmptyMVar
    lastResultVar <- newEmptyMVar
    (printActionVar :: MVar (PrintAction IO)) <- newEmptyMVar
    let
        withCommand :: forall m. (MonadIO m, MonadCatch m) => (Text -> m ()) -> m ()
        withCommand cont = do
            cmd <- takeMVar nextCommandVar
            res <-
                -- We do not want the REPL to crash when a command is interrupted
                -- with an async exception, hence 'handleAsync'.
                handleAsync
                    (\e -> return $ CommandFailure e)
                    (CommandSuccess <$ cont cmd)
            putMVar lastResultVar res
        putCommand cmd = do
            putMVar nextCommandVar cmd
            takeMVar lastResultVar
        printAction :: forall m. MonadIO m => PrintAction m
        printAction t = do
            c <- readMVar printActionVar
            liftIO $ c t
    return (WithCommandAction{..}, auxxReplOn printActionVar putCommand)

-- | Fork a REPL and provide 'WithCommandAction' to a continuation. Under the
-- hood, 'withAuxxRepl' uses 'race_' from the @async@ package, which means that
-- when either the REPL or the continuation completes (or throws an exception),
-- 'withAuxxRepl' as a whole completes. Assuming we run the node in the
-- continuation, using 'race_' has the desired consequence that in order to quit
-- auxx, we merely need to end the REPL, and the node will be killed
-- automatically (and vice versa, if the node dies for some reason, the user
-- won't be left in an unusable REPL).
withAuxxRepl :: (WithCommandAction -> IO ()) -> IO ()
withAuxxRepl action = do
    (c, auxxRepl) <- createAuxxRepl
    race_ auxxRepl (action c)

-- Internal function, not exported.
-- | Initialize Haskeline, put the 'PrintAction' to the provided MVar (assumes
-- the MVar is empty), and run the REPL.
auxxReplOn :: MonadIO m => MVar (PrintAction m) -> PutCommandAction -> IO ()
auxxReplOn printActionVar putCommand = Haskeline.runInputT settings $ do
    printAction <- Haskeline.getExternalPrint
    putMVar printActionVar (liftIO . printAction . toString)
    greetings
    loop (prompt putCommand)
  where
    greetings = outputTextLn "Welcome to Cardano SL Auxx REPL"
    settings = Haskeline.Settings
        { Haskeline.complete       = Haskeline.noCompletion
        , Haskeline.historyFile    = Nothing -- TODO: CSL-1779, pass it from CLI
        , Haskeline.autoAddHistory = True
        }

-- Internal type, not exported.
-- | The monad in which the REPL runs. Completely independent from the monad in
-- which the node (and the auxx plugin) run.
type AuxxRepl =
    -- Can add ReaderT/StateT here if needed.
    InputT IO

-- Internal function, not exported.
-- | Print a prompt, read a single command and send it for execution using the
-- provided 'PutCommandAction'. Blocks until the command has been executed.
prompt :: PutCommandAction -> AuxxRepl LoopDecision
prompt putCommand = do
    getInputTextLine "auxx> " >>= \case
        Nothing -> return Stop
        Just line -> if isQuitCommand line
            then return Stop
            else Continue <$ do
                res <- putCommand line
                case res of
                    CommandSuccess -> return ()
                    CommandFailure exc -> do
                        outputTextLn $ "Command failed: "
                        Haskeline.outputStrLn $ displayException exc

isQuitCommand :: Text -> Bool
isQuitCommand t = strip t `elem` ["quit", "q", ":quit", ":q"]

------- Util ---------

-- | Similar to 'Haskeline.outputStrLn', but for 'Text' rather than 'String'.
outputTextLn :: Text -> AuxxRepl ()
outputTextLn = Haskeline.outputStrLn . toString

-- | Similar to 'Haskeline.getInputLine', but for 'Text' rather than 'String'.
getInputTextLine :: Text -> AuxxRepl (Maybe Text)
getInputTextLine = (fmap.fmap) toText . Haskeline.getInputLine . toString

data LoopDecision = Continue | Stop

-- | Repeat an action as long as it returns 'Continue'.
loop :: Monad m => m LoopDecision -> m ()
loop step = fix $ \go ->
    step >>= \case
        Continue -> go
        Stop -> return ()
