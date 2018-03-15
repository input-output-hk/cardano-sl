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
import           System.Wlog (Severity (..), defaultConfig, setupLogging,
                              severityPlus, termSeveritiesOutB, usingLoggerName)

import           Mockable.Instances ()
import           Mockable.Production (Production (..))
import           NTP.Client (NtpClientSettings (..), NtpStatus, spawnNtpClient)

runNtpClientIO :: NtpClientSettings -> TVar NtpStatus -> IO ()
runNtpClientIO ntpSettings ntpStatus = do
    setupLogging Nothing $
        defaultConfig "ntp-example" <> termSeveritiesOutB (severityPlus Debug)
    void $ runProduction $
        usingLoggerName "ntp-example" $
        spawnNtpClient ntpSettings ntpStatus
