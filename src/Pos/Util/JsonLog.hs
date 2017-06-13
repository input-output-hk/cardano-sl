{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Some types for json logging.
module Pos.Util.JsonLog
       ( JLEvent(..)
       , JLBlock (..)
       , JLTxS (..)
       , JLTxR (..)
       , JLMemPool (..)
       , jlCreatedBlock
       , jlAdoptedBlock
       , fromJLSlotId
       , fromJLSlotIdUnsafe
       ) where

import           Control.Monad.Except          (MonadError)
import           Data.Aeson                    (FromJSON (..), withObject, (.:),
                                                (.:?), genericParseJSON)
import           Data.Aeson.TH                 (deriveJSON, deriveToJSON)
import           Formatting                    (sformat)
import           Serokell.Aeson.Options        (defaultOptions)
import           Universum                     hiding (modify)

import           Pos.Binary.Block              ()
import           Pos.Binary.Core               ()
import           Pos.Block.Core.Genesis.Lens   (genBlockEpoch)
import           Pos.Block.Core.Main.Lens      (mainBlockTxPayload, mainBlockSlot)
import           Pos.Block.Core.Union.Types    (BiSsc, Block)
import           Pos.Communication.Relay.Logic (InvReqDataFlowLog)
import           Pos.Communication.Types.Relay (RelayLogEvent)
import           Pos.Crypto                    (hash, hashHexF)
import           Pos.Ssc.Class.Helpers         (SscHelpersClass)
import           Pos.Txp.Core.Types            (txpTxs)
import           Pos.Types                     (SlotId (..), EpochIndex (..),
                                                LocalSlotIndex (..), mkLocalSlotIndex, 
                                                gbHeader, gbhPrevBlock, 
                                                HeaderHash, headerHash, headerHashF)
import           Pos.Txp.MemState.Types        (MemPoolModifyReason (Custom, Unknown))

type BlockId = Text
type TxId = Text
type JLSlotId = (Word64, Word16)

-- | Json log of one block with corresponding 'BlockId'.
data JLBlock = JLBlock
    { jlHash      :: BlockId
    , jlPrevBlock :: BlockId
    , jlTxs       :: [TxId]
    , jlSlot      :: JLSlotId
    } deriving Show

-- | Json log of one transaction sent from the (light) wallet.
data JLTxS = JLTxS
    { jlsNodeId :: Text
    , jlsTxId   :: Text
    , jlsInvReq :: InvReqDataFlowLog
    } deriving Show

-- | Json log of one transaction being received by a node.
data JLTxR = JLTxR
    { jlrTxId     :: Text
    , jlrReceived :: Integer
    , jlrError    :: Maybe Text
    } deriving Show

-- | Get 'SlotId' from 'JLSlotId'.
fromJLSlotId :: MonadError Text m => JLSlotId -> m SlotId
fromJLSlotId (ep, sl) = SlotId (EpochIndex ep) <$> mkLocalSlotIndex sl

-- | Get 'SlotId' from 'JLSlotId'.
fromJLSlotIdUnsafe :: JLSlotId -> SlotId
fromJLSlotIdUnsafe x = case fromJLSlotId x of
    Right y -> y
    Left  _ -> error "illegal slot id"

-- | Json log of one mempool modification.
data JLMemPool = JLMemPool
    { -- | Reason for modifying the mempool
      jlmReason      :: MemPoolModifyReason
      -- | Queue length when trying to modify the mempool (not including this
      --   modifier, so it could be 0).
    , jlmQueueLength :: Int
      -- | Time spent waiting for the lock (microseconds)
    , jlmWait        :: Integer
      -- | Time spent doing the modification (microseconds, while holding the lock).
    , jlmModify      :: Integer
      -- | Size of the mempool before the modification.
    , jlmSizeBefore  :: Int
      -- | Size of the mempool after the modification.
    , jlmSizeAfter   :: Int
      -- | How much memory was allocated during the modification.
    , jlmAllocated   :: Int
    } deriving Show

-- | Json log event.
data JLEvent = JLCreatedBlock JLBlock
             | JLAdoptedBlock BlockId
             | JLTpsStat Int
             | JLTxSent JLTxS
             | JLTxReceived JLTxR 
             | JLMemPoolEvent JLMemPool
             | JLRelayEvent RelayLogEvent
  deriving (Show, Generic)

$(deriveJSON defaultOptions ''JLBlock)
$(deriveJSON defaultOptions ''JLTxS)
$(deriveJSON defaultOptions ''JLTxR)
$(deriveJSON defaultOptions ''JLMemPool)
$(deriveToJSON defaultOptions ''JLEvent)

instance FromJSON JLEvent where

    parseJSON = \v -> 
        (    (genericParseJSON defaultOptions v)
         -- Second iteration of JLMemPoolEvent: the 'reason' was Text and
         -- the allocation field was optional.
         <|> (flip (withObject "JLEvent") v $ \oEvent -> do
                 oMemPool <- oEvent .: "memPoolEvent"
                 reason <- oMemPool .: "reason"
                 queueLength <- oMemPool .: "queueLength"
                 wait <- oMemPool .: "wait"
                 modify <- oMemPool .: "modify"
                 sizeBefore <- oMemPool .: "sizeBefore"
                 sizeAfter <- oMemPool .: "sizeAfter"
                 mAllocated <- oMemPool .:? "allocated"
                 pure $ JLMemPoolEvent $ JLMemPool
                     { jlmReason = Custom reason
                     , jlmQueueLength = queueLength
                     , jlmWait = wait
                     , jlmModify = modify
                     , jlmSizeBefore = sizeBefore
                     , jlmSizeAfter = sizeAfter
                     , jlmAllocated = fromMaybe 0 mAllocated
                     }
             )
         -- First iteration of JLMemPoolEvent: only the mempool size was recorded.
         <|> (flip (withObject "JLEvent") v $ \o -> do
                 sizeAfter <- o .: "memPoolSize"
                 pure $ JLMemPoolEvent $ JLMemPool
                     { jlmReason = Unknown
                     , jlmQueueLength = 0
                     , jlmWait = 0
                     , jlmModify = 0
                     , jlmSizeBefore = 0
                     , jlmSizeAfter = sizeAfter
                     , jlmAllocated = 0
                     }
             )
        )

-- | Return event of created block.
jlCreatedBlock :: BiSsc ssc => Block ssc -> JLEvent
jlCreatedBlock block = JLCreatedBlock $ JLBlock {..}
  where
    jlHash = showHeaderHash $ headerHash block
    jlPrevBlock = showHeaderHash $ case block of
        Left  gB -> view gbhPrevBlock (gB ^. gbHeader)
        Right mB -> view gbhPrevBlock (mB ^. gbHeader)
    jlSlot = (getEpochIndex $ siEpoch slot, getSlotIndex $ siSlot slot)
    jlTxs = case block of
              Left _   -> []
              Right mB -> map fromTx . toList $ mB ^. mainBlockTxPayload . txpTxs
    slot :: SlotId
    slot = case block of
        Left  gB -> let slotZero = case mkLocalSlotIndex 0 of
                                        Right sz -> sz
                                        Left _   -> error "impossible branch"
                    in SlotId (gB ^. genBlockEpoch) slotZero
        Right mB -> mB ^. mainBlockSlot
    fromTx = sformat hashHexF . hash

showHeaderHash :: HeaderHash -> Text
showHeaderHash = sformat headerHashF

-- | Returns event of created 'Block'.
jlAdoptedBlock :: SscHelpersClass ssc => Block ssc -> JLEvent
jlAdoptedBlock = JLAdoptedBlock . showHeaderHash . headerHash
