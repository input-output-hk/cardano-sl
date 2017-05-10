{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Monadic represantion of something that has @json@ journaled log
-- of operations.

module Pos.Util.JsonLog
       ( JLEvent(..)
       , JLBlock (..)
       , JLTimedEvent (..)
       , jlCreatedBlock
       , jlAdoptedBlock
       , MonadJL
       , jlLog
       , appendJL
       , fromJLSlotId
       , JLFile(..)
       ) where

import           Control.Concurrent.MVar (withMVar)
import           Data.Aeson              (encode)
import           Data.Aeson.TH           (deriveJSON)
import qualified Data.ByteString.Lazy    as LBS
import qualified Ether
import           Formatting              (sformat, shown, (%))
import           Mockable                (Catch, Mockable, catchAll)
import           Serokell.Aeson.Options  (defaultOptions)
import           System.Wlog             (CanLog, HasLoggerName, logWarning)
import           Universum               hiding (catchAll)

import           Pos.Binary.Block        ()
import           Pos.Binary.Core         ()
import           Pos.Crypto              (Hash, hash, hashHexF)
import           Pos.Ssc.Class.Types     (Ssc)
import           Pos.Types               (BiSsc, Block, SlotId (..), blockHeader,
                                          blockTxs, epochIndexL, gbHeader, gbhPrevBlock,
                                          headerHash, headerSlot)
import           Pos.Util.TimeWarp       (currentTime)

type BlockId = Text
type TxId = Text
type JLSlotId = (Word64, Word16)

-- | Json log of one block with corresponding 'BlockId'.
data JLBlock = JLBlock
    { jlHash      :: BlockId
    , jlPrevBlock :: BlockId
    , jlTxs       :: [TxId]
    , jlSlot      :: JLSlotId
    } deriving (Show)

-- | Get 'SlotId' from 'JLSlotId'.
fromJLSlotId :: JLSlotId -> SlotId
fromJLSlotId (ep, sl) = SlotId (fromIntegral ep) (fromIntegral sl)

-- | Json log event.
data JLEvent = JLCreatedBlock JLBlock
             | JLAdoptedBlock BlockId
             | JLTpsStat Int
  deriving Show

-- | 'JLEvent' with 'Timestamp' -- corresponding time of this event.
data JLTimedEvent = JLTimedEvent
    { jlTimestamp :: Integer
    , jlEvent     :: JLEvent
    } deriving (Show)

$(deriveJSON defaultOptions ''JLBlock)
$(deriveJSON defaultOptions ''JLEvent)
$(deriveJSON defaultOptions ''JLTimedEvent)

-- | Return event of created block.
jlCreatedBlock :: BiSsc ssc => Block ssc -> JLEvent
jlCreatedBlock block = JLCreatedBlock $ JLBlock {..}
  where
    jlHash = showHash $ headerHash block
    jlPrevBlock = showHash $ either (view gbhPrevBlock) (view gbhPrevBlock) (block ^. blockHeader)
    jlSlot = (fromIntegral $ siEpoch slot, fromIntegral $ siSlot slot)
    jlTxs = case block of
              Left _   -> []
              Right mB -> map fromTx . toList $ mB ^. blockTxs
    slot :: SlotId
    slot = either (\h -> SlotId (h ^. epochIndexL) 0) (view $ gbHeader . headerSlot) $ block
    fromTx = showHash . hash

showHash :: Hash a -> Text
showHash = sformat hashHexF

-- | Returns event of created 'Block'.
jlAdoptedBlock :: Ssc ssc => Block ssc -> JLEvent
jlAdoptedBlock = JLAdoptedBlock . showHash . headerHash

-- | Append event into log by given 'FilePath'.
appendJL :: (MonadIO m) => FilePath -> JLEvent -> m ()
appendJL path ev = liftIO $ do
  time <- currentTime
  LBS.appendFile path . encode $ JLTimedEvent (fromIntegral time) ev

newtype JLFile = JLFile (Maybe (MVar FilePath))

-- | Monad for things that can log Json log events.
type MonadJL m =
    ( Ether.MonadReader' JLFile m
    , MonadIO m
    , Mockable Catch m
    , HasLoggerName m
    , CanLog m )

jlLog :: MonadJL m => JLEvent -> m ()
jlLog ev = do
    JLFile jlFileM <- Ether.ask'
    whenJust jlFileM $ \logFileMV ->
        (liftIO . withMVar logFileMV $ flip appendJL ev)
        `catchAll` \e ->
            logWarning $ sformat ("Can't write to json log: "%shown) e
