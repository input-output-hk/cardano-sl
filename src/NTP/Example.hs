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

import           Data.Default        (def)
import           System.Wlog         (Severity (..), initTerminalLogging, usingLoggerName)

import           Mockable.Instances  ()
import           Mockable.Production (runProduction)
import           NTP.Client          (NtpClientSettings (..), NtpStopButton (..),
                                      startNtpClient)

runNtpClientIO :: NtpClientSettings -> IO NtpStopButton
runNtpClientIO settings = do
    initTerminalLogging True (Just Info)
    runProduction $ usingLoggerName "ntp-example" $ startNtpClient settings
