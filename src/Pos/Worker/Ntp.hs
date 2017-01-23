module Pos.Worker.Ntp
       (
         ntpWorker
       ) where

import qualified Control.Concurrent.STM as STM
import           Data.List              ((!!))
import           Data.Time.Units        (Microsecond)
import           NTP.Client             (NtpClientSettings (..), startNtpClient)
import           NTP.Example            ()
import           System.Wlog            (WithLogger, logDebug)
import           Universum

import qualified Pos.Constants          as C
import           Pos.Context            (NodeContext (..), getNodeContext)
import           Pos.Slotting           (ssNtpDataL)
import           Pos.WorkMode           (WorkMode)


settings :: NodeContext ssc -> NtpClientSettings
settings nc = NtpClientSettings
        { -- list of servers addresses
          ntpServers         = [ "pool.ntp.org"
                               , "time.windows.com"
                               , "clock.isc.org"
                               , "ntp5.stratum2.ru"]
        -- got time margin callback
        , ntpHandler         = ntpHandlerDo nc
        -- logger name modifier
        , ntpLogName         = "ntp"
        -- delay between making requests and response collection;
        -- it also means that handler will be invoked with this lag
        , ntpResponseTimeout = C.ntpResponseTimeout
        -- how often to send responses to server
        , ntpPollDelay       = C.ntpPollDelay
        -- way to sumarize results received from different servers.
        , ntpMeanSelection   = \l -> let len = length l in sort l !! ((len - 1) `div` 2)
        }

-- | Worker for synchronization of local time and global time
ntpWorker :: WorkMode ssc m => m ()
ntpWorker = getNodeContext >>=
    void . startNtpClient . settings

ntpHandlerDo :: (MonadIO m, WithLogger m)
             => NodeContext ssc
             -> (Microsecond, Microsecond)
             -> m ()
ntpHandlerDo nc (newMargin, transtimTime) = do
    logDebug $ "Callback on new margin: " <> show newMargin
    let realTime = transtimTime + newMargin
    atomically $ STM.modifyTVar (ncSlottingState nc)
                                (ssNtpDataL .~ (newMargin, realTime))
