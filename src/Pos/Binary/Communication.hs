-- | Communication-related serialization

module Pos.Binary.Communication () where

import           Universum

import           Pos.Binary.Class        (Bi (..))
import           Pos.Communication.Types (MsgBlock (..), MsgGetBlocks (..),
                                          MsgGetHeaders (..), MsgHeaders (..),
                                          RequestBlock (..), RequestBlockchainPart (..),
                                          SendBlockHeader (..), SendBlockchainPart (..),
                                          SendProxySecretKey (..), SysStartRequest (..),
                                          SysStartResponse (..))
import           Pos.Ssc.Class.Types     (Ssc (..))
import           Pos.Types               ()

instance Bi SysStartRequest where
    put = mempty
    get = pure SysStartRequest

instance Bi SysStartResponse where
    put (SysStartResponse t msid) = put t >> put msid
    get = SysStartResponse <$> get <*> get

instance Bi (MsgGetHeaders ssc) where
    put (MsgGetHeaders f t) = put f >> put t
    get = MsgGetHeaders <$> get <*> get

instance Bi (MsgGetBlocks ssc) where
    put (MsgGetBlocks f t) = put f >> put t
    get = MsgGetBlocks <$> get <*> get

instance Ssc ssc => Bi (MsgHeaders ssc) where
    put (MsgHeaders b) = put b
    get = MsgHeaders <$> get

instance Ssc ssc => Bi (MsgBlock ssc) where
    put (MsgBlock b) = put b
    get = MsgBlock <$> get

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

instance Bi SendProxySecretKey where
    put (SendProxySecretKey pSk) = put pSk
    get = SendProxySecretKey <$> get
