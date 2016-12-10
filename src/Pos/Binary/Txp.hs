-- | Binary serialization for Pos.Txp.*

module Pos.Binary.Txp where

import           Control.Monad.Fail          (fail)
import           Data.Binary.Get             (getWord32be, getWord8)
import           Data.Binary.Put             (putWord32be, putWord8)
import           Data.Digest.CRC32           (crc32)
import           Universum

import           Pos.Binary.Class            (Bi (..))
import           Pos.Txp.Types.Communication (TxDataMsg, TxInvMsg, TxReqMsg)
import           Pos.Types.Address           (Address (..))

instance Bi TxInvMsg
instance Bi TxReqMsg
instance Bi TxDataMsg
