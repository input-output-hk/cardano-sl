{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Monadic represantion of something that has @json@ journaled log
-- of operations.

module Pos.Util.JsonLog
       ( JLEvent(..)
       , JLBlock (..)
       , jlCreatedBlock
       , jlAdoptedBlock
       , fromJLSlotId
       ) where

import           Universum               hiding (catchAll)

import           Data.Aeson.TH           (deriveJSON)
import           Formatting              (sformat)
import           Serokell.Aeson.Options  (defaultOptions)

import           Pos.Binary.Block        ()
import           Pos.Binary.Core         ()
import           Pos.Block.Core          (BiSsc, Block, blockHeader, mainBlockTxPayload)
import           Pos.Core                (SlotId (..), epochIndexL, gbHeader,
                                          gbhPrevBlock, getSlotIndex, headerHash,
                                          headerSlotL, mkLocalSlotIndex)
import           Pos.Crypto              (Hash, hash, hashHexF)
import           Pos.Ssc.Class.Helpers   (SscHelpersClass)
import           Pos.Txp.Core            (txpTxs)
import           Pos.Util.Util           (leftToPanic)

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
fromJLSlotId (ep, sl) =
    SlotId
        (fromIntegral ep)
        (leftToPanic "fromJLSlotId: " $ mkLocalSlotIndex sl)

-- | Json log event.
data JLEvent = JLCreatedBlock JLBlock
             | JLAdoptedBlock BlockId
             | JLTpsStat Int
  deriving Show

$(deriveJSON defaultOptions ''JLBlock)
$(deriveJSON defaultOptions ''JLEvent)

-- | Return event of created block.
jlCreatedBlock :: BiSsc ssc => Block ssc -> JLEvent
jlCreatedBlock block = JLCreatedBlock $ JLBlock {..}
  where
    jlHash = showHash $ headerHash block
    jlPrevBlock = showHash $ either (view gbhPrevBlock) (view gbhPrevBlock) (block ^. blockHeader)
    jlSlot = (fromIntegral $ siEpoch slot, fromIntegral $ getSlotIndex $ siSlot slot)
    jlTxs = case block of
              Left _   -> []
              Right mB -> map fromTx . toList $ mB ^. mainBlockTxPayload . txpTxs
    slot :: SlotId
    slot = either (\h -> SlotId (h ^. epochIndexL) minBound) (view $ gbHeader . headerSlotL) $ block
    fromTx = showHash . hash

showHash :: Hash a -> Text
showHash = sformat hashHexF

-- | Returns event of created 'Block'.
jlAdoptedBlock :: SscHelpersClass ssc => Block ssc -> JLEvent
jlAdoptedBlock = JLAdoptedBlock . showHash . headerHash
