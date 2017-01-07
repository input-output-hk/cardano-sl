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

import           Data.Default (def)
import           Mockable     (runProduction)
import           NTP.Client   (NtpClientSettings (..), NtpStopButton (..), startNtpClient)
import           System.Wlog  (Severity (..), initLogging, usingLoggerName)

runNtpClientIO :: NtpClientSettings -> IO NtpStopButton
runNtpClientIO settings = do
    initLogging Info
    runProduction $ usingLoggerName "ntp-example" $ startNtpClient settings
