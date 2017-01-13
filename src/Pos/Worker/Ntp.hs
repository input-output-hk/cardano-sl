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
import           Pos.WorkMode           (WorkMode)


settings :: NodeContext ssc -> NtpClientSettings
settings nc = NtpClientSettings
        { ntpServers         = [ "ntp5.stratum2.ru"
                               , "ntp1.stratum1.ru"
                               , "clock.isc.org"
                               ]
          -- ^ list of servers addresses
        , ntpHandler         = ntpHandlerDo nc
        -- ^ got time margin callback
        , ntpLogName         = "ntp"
        -- ^ logger name modifier
        , ntpResponseTimeout = C.ntpResponseTimeout
        -- ^ delay between making requests and response collection;
        -- it also means that handler will be invoked with this lag
        , ntpPollDelay       = C.ntpPollDelay
        -- ^ how often to send responses to server
        , ntpMeanSelection   = \l -> let len = length l in sort l !! ((len - 1) `div` 2)
        -- ^ way to sumarize results received from different servers.
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
    atomically $ STM.writeTVar (ncNtpData nc) (newMargin, realTime)
