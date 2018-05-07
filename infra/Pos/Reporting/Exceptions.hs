-- | Exceptions related to error reporting.

module Pos.Reporting.Exceptions
       ( ReportingError(..)
       , PackingFailure(..)
       ) where

import           Universum

import           Control.Exception.Safe (Exception (..))
import qualified Data.Text.Buildable
import           Formatting (bprint, stext, string, (%))
import           Serokell.Util (listJson)

import           Pos.Exception (cardanoExceptionFromException, cardanoExceptionToException)

data ReportingError
    = CantRetrieveLogs [FilePath]
    | SendingError !SomeException
    | NoPubFiles
    | PackingError PackingFailure
    deriving Show

instance Exception ReportingError where
    toException = cardanoExceptionToException
    fromException = cardanoExceptionFromException
    displayException = toString . pretty

instance Buildable ReportingError where
    build =
        \case
            CantRetrieveLogs paths ->
                bprint
                    ("Can't retrieve logs from these paths: " %listJson)
                    paths
            SendingError exc ->
                bprint
                    ("Failed to send a report, the exception was: " %string)
                    (displayException exc)
            NoPubFiles ->
                "can't find any .pub file in logconfig. " <>
                "Most probably public logging is misconfigured. Either set reporting " <>
                "servers to [] or include .pub files in log config"
            PackingError (LogFileNotFound path) ->
                bprint
                    ("Can't pack log file "%string%" because it doesn't exist or isn't a file")
                    path
            PackingError (TarPathError t) ->
                bprint ("Couldn't pack log file because: "%stext) t

data PackingFailure
    = LogFileNotFound FilePath
    | TarPathError Text
    deriving Show
