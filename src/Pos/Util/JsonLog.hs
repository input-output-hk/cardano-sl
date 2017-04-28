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
       , MonadJL (..)
       , appendJL
       , fromJLSlotId
       , JsonLogFilePathBox
       , usingJsonLogFilePath
       ) where

import           Control.Lens             (iso)
import           Control.Monad.Fix        (MonadFix)
import           Control.Monad.Trans      (MonadTrans (..))
import           Data.Aeson               (encode)
import           Data.Aeson.TH            (deriveJSON)
import qualified Data.ByteString.Lazy     as LBS
import           Formatting               (sformat)
import           Mockable                 (liftMockableWrappedM, MFunctor')
import           Mockable.Channel         (ChannelT)
import           Mockable.Class           (Mockable (..))
import           Mockable.Concurrent      (ThreadId, Promise)
import           Mockable.Metrics         (Counter, Distribution, Gauge)
import           Mockable.SharedAtomic    (SharedAtomicT)
import           Mockable.SharedExclusive (SharedExclusiveT)
import           Serokell.Aeson.Options   (defaultOptions)
import           Serokell.Util.Lens       (WrappedM (..))
import           System.Wlog              (CanLog)
import           Universum

import           Pos.Binary.Block         ()
import           Pos.Binary.Core          ()
import           Pos.Crypto               (Hash, hash, hashHexF)
import           Pos.Ssc.Class.Types      (Ssc)
import           Pos.Types                (BiSsc, Block, SlotId (..), blockHeader, blockTxs,
                                           epochIndexL, gbHeader, gbhPrevBlock, headerHash,
                                           headerSlot)
import           Pos.Util.TimeWarp        (currentTime)

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
             | JLMemPoolSize Int
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

-- | Monad for things that can log Json log events.
class Monad m => MonadJL m where
    jlLog :: JLEvent -> m ()

    default jlLog :: (MonadTrans t, MonadJL m', t m' ~ m) => JLEvent -> m ()
    jlLog = lift . jlLog

instance {-# OVERLAPPABLE #-}
    (MonadJL m, MonadTrans t, Monad (t m)) =>
        MonadJL (t m)

---------------------------------------------------------------
-- JsonLogFilePathBox monad transformer
---------------------------------------------------------------

newtype JsonLogFilePathBox m a = JsonLogFilePathBox { jsonLogFilePathBoxEntry :: ReaderT (Maybe FilePath) m a }
    deriving (Functor, Applicative, Monad, MonadTrans,
              CanLog, MonadIO, MonadFix, MonadMask, MonadCatch, MonadThrow)

type instance ThreadId (JsonLogFilePathBox m) = ThreadId m

type instance Promise (JsonLogFilePathBox m) = Promise m

type instance SharedAtomicT (JsonLogFilePathBox m) = SharedAtomicT m

type instance SharedExclusiveT (JsonLogFilePathBox m) = SharedExclusiveT m

type instance ChannelT (JsonLogFilePathBox m) = ChannelT m

type instance Counter (JsonLogFilePathBox m) = Counter m

type instance Distribution (JsonLogFilePathBox m) = Distribution m

type instance Gauge (JsonLogFilePathBox m) = Gauge m

instance Monad m => WrappedM (JsonLogFilePathBox m) where

    type UnwrappedM (JsonLogFilePathBox m) = ReaderT (Maybe FilePath) m

    _WrappedM = iso jsonLogFilePathBoxEntry JsonLogFilePathBox

instance ( Mockable d m
         , MFunctor' d (JsonLogFilePathBox m) (ReaderT (Maybe FilePath) m)
         , MFunctor' d (ReaderT (Maybe FilePath) m) m
         ) => Mockable d (JsonLogFilePathBox m) where
    liftMockable = liftMockableWrappedM

instance MonadIO m => MonadJL (JsonLogFilePathBox m) where

    jlLog event = JsonLogFilePathBox $ whenJustM ask $ flip appendJL event
        
usingJsonLogFilePath :: Maybe FilePath -> JsonLogFilePathBox m a -> m a
usingJsonLogFilePath path = flip runReaderT path . jsonLogFilePathBoxEntry
