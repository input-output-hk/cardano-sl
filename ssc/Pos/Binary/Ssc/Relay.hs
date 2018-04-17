module Pos.Binary.Ssc.Relay
       (
       ) where

import           Universum

import           Pos.Binary.Class (Bi (..), encodeListLen, enforceSize)
import           Pos.Communication.Types.Relay (DataMsg (..))
import qualified Pos.Ssc.Message as T

instance Bi (DataMsg T.MCCommitment) where
    encode (DataMsg (T.MCCommitment signedComm)) = encode signedComm
    encodedSize (DataMsg (T.MCCommitment signedComm)) = encodedSize signedComm
    decode = DataMsg . T.MCCommitment <$> decode

instance Bi (DataMsg T.MCOpening) where
    encode (DataMsg (T.MCOpening sId opening)) = encodeListLen 2 <> encode sId <> encode opening
    encodedSize (DataMsg (T.MCOpening sId opening)) = 1 + encodedSize sId + encodedSize opening
    decode = do
        enforceSize "DataMsg T.MCOpening" 2
        DataMsg <$> (T.MCOpening <$> decode <*> decode)

instance Bi (DataMsg T.MCShares) where
    encode (DataMsg (T.MCShares sId innerMap)) = encodeListLen 2 <> encode sId <> encode innerMap
    encodedSize (DataMsg (T.MCShares sId innerMap)) = 1 + encodedSize sId + encodedSize innerMap
    decode = do
        enforceSize "DataMsg T.MCShares" 2
        DataMsg <$> (T.MCShares <$> decode <*> decode)

instance Bi (DataMsg T.MCVssCertificate) where
    encode (DataMsg (T.MCVssCertificate vss)) = encode vss
    encodedSize (DataMsg (T.MCVssCertificate vss)) = encodedSize vss
    decode = DataMsg . T.MCVssCertificate <$> decode
