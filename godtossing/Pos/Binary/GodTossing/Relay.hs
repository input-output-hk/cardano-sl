module Pos.Binary.GodTossing.Relay
       (
       ) where

import           Universum

import           Pos.Binary.Class              (Bi (..), enforceSize, encodeListLen)
import           Pos.Core.Configuration        (HasConfiguration)
import           Pos.Communication.Types.Relay (DataMsg (..))
import qualified Pos.Ssc.GodTossing.Types      as T

instance Bi (DataMsg T.MCCommitment) where
  encode (DataMsg (T.MCCommitment signedComm)) = encode signedComm
  decode = DataMsg . T.MCCommitment <$> decode

instance Bi (DataMsg T.MCOpening) where
  encode (DataMsg (T.MCOpening sId opening)) = encodeListLen 2 <> encode sId <> encode opening
  decode = do
    enforceSize "DataMsg T.MCOpening" 2
    DataMsg <$> (T.MCOpening <$> decode <*> decode)

instance Bi (DataMsg T.MCShares) where
  encode (DataMsg (T.MCShares sId innerMap)) = encodeListLen 2 <> encode sId <> encode innerMap
  decode = do
    enforceSize "DataMsg T.MCShares" 2
    DataMsg <$> (T.MCShares <$> decode <*> decode)

instance HasConfiguration => Bi (DataMsg T.MCVssCertificate) where
  encode (DataMsg (T.MCVssCertificate vss)) = encode vss
  decode = DataMsg . T.MCVssCertificate <$> decode
