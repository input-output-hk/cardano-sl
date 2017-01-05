{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}

-- | This module implements functionality of NTP client.

module NTP.Example
    ( NtpClientSettings (..)
    , runNtpClientIO
    , def
    -- there is no stop button, since `runNtpClientIO` does `initLogging`
    ) where

import Data.Default        (def)
import Mockable.Production (runProduction)
import NTP.Client          (NtpClientSettings (..), NtpStopButton (..), startNtpClient)
import System.Wlog         (CanLog (..), HasLoggerName (..), Severity (..), initLogging)

import Mockable.Production (Production (..))

runNtpClientIO :: NtpClientSettings -> IO NtpStopButton
runNtpClientIO settings = do
    initLogging Info
    runProduction $ startNtpClient settings


-- * Temporal instances, till we get proper instances of `Mockable` for existing monads

instance HasLoggerName Production where
    getLoggerName = return "ntp-example"
    modifyLoggerName = const id

instance CanLog Production where
    dispatchMessage name severity msg = do
        Production $ dispatchMessage name severity msg
