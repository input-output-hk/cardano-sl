-- | Exceptions related to error reporting.
--
-- FIXME rename. These are log-warper and HTTP specific.

module Pos.Reporting.Exceptions
       ( ReportingError(..)
       ) where

import           Universum

import           Control.Exception.Safe (Exception (..))
import qualified Data.Text.Buildable
import           Formatting (bprint, shown, stext, string, (%))

import           Pos.Exception (cardanoExceptionFromException, cardanoExceptionToException)

data ReportingError
    = CantRetrieveLogs FilePath
    | SendingError !SomeException
    | NoPubFiles
    | PackingError Text
    deriving Show

instance Exception ReportingError where
    toException = cardanoExceptionToException
    fromException = cardanoExceptionFromException
    displayException = toString . pretty

instance Buildable ReportingError where
    build =
        \case
            CantRetrieveLogs path ->
                bprint
                    ("Can't retrieve logs from theis path: " %shown)
                    path
            SendingError exc ->
                bprint
                    ("Failed to send a report, the exception was: " %string)
                    (displayException exc)
            NoPubFiles ->
                "can't find any .pub file in logconfig. " <>
                "Most probably public logging is misconfigured. Either set reporting " <>
                "servers to [] or include .pub files in log config"
            PackingError t ->
                bprint ("Couldn't pack log files, reason: "%stext) t
