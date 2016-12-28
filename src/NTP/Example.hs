{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}

-- | This module implements functionality of NTP client.

module NTP.Example
    ( NtpClientSettings (..)
    , runNtpClientIO
    , pressIO
    , def
    ) where

import Data.Default        (def)
import Data.Default        (def)
import Mockable.Production (runProduction)
import NTP.Client          (NtpClientSettings (..), NtpStopButton (..), startNtpClient)
import System.Wlog         (CanLog (..), HasLoggerName (..), Severity (..), initLogging)

import Mockable.Production (Production (..))

runNtpClientIO :: NtpClientSettings -> IO NtpStopButton
runNtpClientIO settings = runProduction $ startNtpClient settings

pressIO :: NtpStopButton -> IO ()
pressIO = runProduction . press

-- * Temporal instances, till we get proper instances of `Mockable` for existing monads

instance HasLoggerName Production where
    getLoggerName = return "ntp-example"
    modifyLoggerName = const id

instance CanLog Production where
    dispatchMessage name severity msg = do
        initLogging Debug
        Production $ dispatchMessage name severity msg
