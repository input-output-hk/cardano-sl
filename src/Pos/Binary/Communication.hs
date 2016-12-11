-- | Communication-related serialization

module Pos.Binary.Communication () where

import           Universum

import           Pos.Binary.Class                 (Bi (..))
import           Pos.Communication.Types.Block    (RequestBlock (..),
                                                   RequestBlockchainPart (..),
                                                   SendBlock (..), SendBlockHeader (..),
                                                   SendBlockchainPart (..))
import           Pos.Communication.Types.SysStart (SysStartRequest (..),
                                                   SysStartResponse (..))
import           Pos.Ssc.Class.Types              (Ssc (..))

instance Bi SysStartRequest where
    put = mempty
    get = pure SysStartRequest

instance Bi SysStartResponse where
    put (SysStartResponse t msid) = put t >> put msid
    get = SysStartResponse <$> get <*> get

instance Ssc ssc => Bi (SendBlock ssc)
instance Ssc ssc => Bi (SendBlockHeader ssc)
instance Ssc ssc => Bi (SendBlockchainPart ssc)
instance Ssc ssc => Bi (RequestBlock ssc)
instance Ssc ssc => Bi (RequestBlockchainPart ssc)
