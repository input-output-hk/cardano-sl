{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RankNTypes            #-}

-- | This module implements functionality of NTP client.

module NTP.Example
    ( NtpClientSettings (..)
    , runNtpClientIO
    ) where

import           Universum

import           Control.Monad (void)
import           System.Wlog (LoggerNameBox, Severity (..), defaultConfig, setupLogging,
                              severityPlus, termSeveritiesOutB, usingLoggerName)

import           Mockable.Instances ()
import           Mockable.Production (Production (..))
import           NTP.Client (NtpClientSettings (..), spawnNtpClient)

type WorkMode = LoggerNameBox Production

runNtpClientIO :: NtpClientSettings WorkMode -> IO ()
runNtpClientIO settings = do
    setupLogging Nothing $
        defaultConfig "ntp-example" <> termSeveritiesOutB (severityPlus Debug)
    void $ runProduction $
        usingLoggerName "ntp-example" $
        spawnNtpClient settings
