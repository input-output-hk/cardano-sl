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

import           Control.Monad       (void)
import           Data.Default        (def)
import           System.Wlog         (LoggerNameBox, Severity (..), initTerminalLogging,
                                      usingLoggerName)

import           Mockable.Instances  ()
import           Mockable.Production (Production (..))
import           NTP.Client          (NtpClientSettings (..), startNtpClient)

type WorkMode = LoggerNameBox Production

runNtpClientIO :: NtpClientSettings WorkMode -> IO ()
runNtpClientIO settings = do
    initTerminalLogging True (Just Debug)
    void $ runProduction $ usingLoggerName "ntp-example" $
        startNtpClient settings
