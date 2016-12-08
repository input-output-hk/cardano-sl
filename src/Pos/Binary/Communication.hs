-- | Communication-related serialization

module Pos.Binary.Communication where

import           Universum

import           Pos.Binary.Class                 (Bi)
import           Pos.Communication.Types.Block    (RequestBlock (..), SendBlock (..),
                                                   SendBlockHeader (..))
import           Pos.Communication.Types.SysStart (SysStartRequest (..),
                                                   SysStartResponse (..))
import           Pos.Communication.Types.Tx       (SendTx, SendTxs)
import           Pos.Ssc.Class.Types              (Ssc (..))

instance Bi SendTx
instance Bi SendTxs

instance Ssc ssc => Bi (SendBlock ssc)
instance Ssc ssc => Bi (SendBlockHeader ssc)
instance Bi (RequestBlock ssc)

instance Bi SysStartRequest
instance Bi SysStartResponse
