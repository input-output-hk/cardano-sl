{-# LANGUAGE TemplateHaskell #-}
module Pos.Util.JsonLog
    ( JLEvent(..)
    , JLBlock (..)
    , JLTimedEvent (..)
    , jlCreatedBlock
    , jlAdoptedBlock
    , MonadJL (..)
    , appendJL
    , fromJLSlotId
    ) where

import           Control.Lens           (view, (^.))
import           Control.TimeWarp.Timed (currentTime, runTimedIO)
import           Data.Aeson             (encode)
import           Data.Aeson.TH          (deriveJSON)
import qualified Data.ByteString.Lazy   as LBS
import           Formatting             (sformat)
import           Pos.Crypto             (Hash, hash, hashHexF)
import           Pos.DHT                (DHTResponseT)
import           Pos.Ssc.Class.Types    (Ssc)
import           Pos.Types              (Block, SlotId (..), blockHeader, blockTxs,
                                         epochIndexL, gbHeader, gbhPrevBlock, headerHash,
                                         headerSlot)
import           Serokell.Aeson.Options (defaultOptions)
import           Universum

type BlockId = Text
type TxId = Text
type JLSlotId = (Word64, Word16)

data JLBlock =
  JLBlock
    { jlHash      :: BlockId
    , jlPrevBlock :: BlockId
    , jlTxs       :: [TxId]
    , jlSlot      :: JLSlotId
    }
  deriving Show

fromJLSlotId :: JLSlotId -> SlotId
fromJLSlotId (ep, sl) = SlotId (fromIntegral ep) (fromIntegral sl)

data JLEvent = JLCreatedBlock JLBlock
             | JLAdoptedBlock BlockId
             | JLTpsStat Int
  deriving Show

data JLTimedEvent =
  JLTimedEvent
    { jlTimestamp :: Integer
    , jlEvent     :: JLEvent
    }
  deriving Show

$(deriveJSON defaultOptions ''JLBlock)
$(deriveJSON defaultOptions ''JLEvent)
$(deriveJSON defaultOptions ''JLTimedEvent)

jlCreatedBlock :: Ssc ssc => Block ssc -> JLEvent
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

jlAdoptedBlock :: Ssc ssc => Block ssc -> JLEvent
jlAdoptedBlock = JLAdoptedBlock . showHash . headerHash

appendJL :: MonadIO m => FilePath -> JLEvent -> m ()
appendJL path ev = liftIO $ do
  time <- runTimedIO currentTime
  LBS.appendFile path . encode $ JLTimedEvent (fromIntegral time) ev

class Monad m => MonadJL m where
  jlLog :: JLEvent -> m ()

instance MonadJL m => MonadJL (DHTResponseT m) where
    jlLog = lift . jlLog

instance MonadJL m => MonadJL (ReaderT s m) where
    jlLog = lift . jlLog

instance MonadJL m => MonadJL (StateT s m) where
    jlLog = lift . jlLog
