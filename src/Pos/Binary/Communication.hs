-- | Communication-related serialization

module Pos.Binary.Communication () where

import           Pos.Binary.Class                 (Bi)
import           Pos.Communication.Types.Block    (RequestBlock (..),
                                                   RequestBlockchainPart (..),
                                                   SendBlock (..), SendBlockHeader (..),
                                                   SendBlockchainPart (..))
import           Pos.Communication.Types.SysStart (SysStartRequest (..),
                                                   SysStartResponse (..))
import           Pos.Ssc.Class.Types              (Ssc (..))

instance Bi SysStartRequest
instance Bi SysStartResponse

instance Ssc ssc => Bi (SendBlock ssc)
instance Ssc ssc => Bi (SendBlockHeader ssc)
instance Ssc ssc => Bi (SendBlockchainPart ssc)
instance Bi (RequestBlock ssc)
instance Bi (RequestBlockchainPart ssc)
