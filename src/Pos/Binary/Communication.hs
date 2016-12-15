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
import           Pos.Types                        ()

instance Bi SysStartRequest where
    put = mempty
    get = pure SysStartRequest

instance Bi SysStartResponse where
    put (SysStartResponse t msid) = put t >> put msid
    get = SysStartResponse <$> get <*> get

instance Ssc ssc => Bi (SendBlock ssc) where
    put (SendBlock b) = put b
    get = SendBlock <$> get

instance Ssc ssc => Bi (SendBlockHeader ssc) where
    put (SendBlockHeader b) = put b
    get = SendBlockHeader <$> get

instance Ssc ssc => Bi (SendBlockchainPart ssc) where
    put (SendBlockchainPart b) = put b
    get = SendBlockchainPart <$> get

instance Bi (RequestBlock ssc) where
    put (RequestBlock b) = put b
    get = RequestBlock <$> get

instance Bi (RequestBlockchainPart ssc) where
    put RequestBlockchainPart{..} = do
        put rbFromBlock
        put rbUntilBlock
        put rbCount
    get = RequestBlockchainPart <$> get <*> get <*> get
