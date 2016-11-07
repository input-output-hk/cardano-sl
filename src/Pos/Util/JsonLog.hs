{-# LANGUAGE TemplateHaskell #-}
module Pos.Util.JsonLog
    ( JLEvent(..)
    , JLBlock (..)
    , JLTimedEvent (..)
    , jlCreatedBlock
    , jlAdoptedBlock
    , MonadJL (..)
    , appendJL
    ) where

import           Control.Lens           ((^.))
import           Control.TimeWarp.Timed (currentTime, runTimedIO)
import           Data.Aeson             (encode)
import           Data.Aeson.TH          (deriveJSON)
import qualified Data.ByteString.Lazy   as LBS
import           Pos.Crypto             (hash)
import           Pos.DHT                (DHTResponseT)
import           Pos.Ssc.Class.Types    (SscTypes)
import           Pos.Types              (Block, HeaderHash, MainBlock, SlotId (..),
                                         gbBody, gbHeader, gbhPrevBlock, headerHash,
                                         headerSlot, mbTxs)
import           Serokell.Aeson.Options (defaultOptions)
import           Universum

type BlockId = Text
type TxId = Text
type JLSlotId = (Word64, Word16)

data JLBlock =
  JLBlock
    { jlBlock     :: BlockId
    , jlPrevBlock :: BlockId
    , jlTxs       :: [TxId]
    , jlSlot      :: JLSlotId
    }

data JLEvent = JLCreated JLBlock
             | JLAdopted BlockId

data JLTimedEvent =
  JLTimedEvent
    { jlTimestamp :: Integer
    , jlEvent     :: JLEvent
    }

$(deriveJSON defaultOptions ''JLBlock)
$(deriveJSON defaultOptions ''JLEvent)
$(deriveJSON defaultOptions ''JLTimedEvent)

headerHashB :: SscTypes ssc => Block ssc -> HeaderHash ssc
headerHashB = headerHash

headerHashMB :: SscTypes ssc => MainBlock ssc -> HeaderHash ssc
headerHashMB = headerHash

jlCreatedBlock :: SscTypes ssc => MainBlock ssc -> JLEvent
jlCreatedBlock block = JLCreated $ JLBlock {..}
  where
    jlBlock = show $ headerHashMB block
    jlPrevBlock = show $ block ^. gbHeader . gbhPrevBlock
    jlSlot = (fromIntegral $ siEpoch slot, fromIntegral $ siSlot slot)
    jlTxs = map fromTx . toList $ block ^. gbBody . mbTxs
    slot = block ^. gbHeader . headerSlot
    fromTx = show . hash

jlAdoptedBlock :: SscTypes ssc => Block ssc -> JLEvent
jlAdoptedBlock = JLAdopted . show . headerHashB

appendJL :: MonadIO m => FilePath -> JLEvent -> m ()
appendJL path ev = liftIO $ do
  time <- runTimedIO currentTime
  LBS.appendFile path . encode $ JLTimedEvent (fromIntegral time) ev

class Monad m => MonadJL m where
  jlLog :: JLEvent -> m ()

instance MonadJL m => MonadJL (DHTResponseT m) where
    jlLog = lift . jlLog

