module Pos.Binary.GodTossing.Relay
       (
       ) where

import           Universum

import           Pos.Binary.Class              (Bi (..), label, labelS, putField)
import           Pos.Communication.Types.Relay (DataMsg (..))
import qualified Pos.Ssc.GodTossing.Types      as T
import qualified Pos.Binary.Cbor               as Cbor

instance Bi (DataMsg T.MCCommitment) where
    sizeNPut = labelS "DataMsg MCCommitment" $
        putField (\(DataMsg (T.MCCommitment signedComm)) -> signedComm)
    get = label "DataMsg MCCommitment" $ fmap DataMsg $ T.MCCommitment <$> get

instance Cbor.Bi (DataMsg T.MCCommitment) where
  encode (DataMsg (T.MCCommitment signedComm)) = Cbor.encode signedComm
  decode = DataMsg . T.MCCommitment <$> Cbor.decode

instance Bi (DataMsg T.MCOpening) where
    sizeNPut = labelS "DataMsg MCOpening" $
        putField (\(DataMsg (T.MCOpening st _)) -> st) <>
        putField (\(DataMsg (T.MCOpening _ op)) -> op)
    get =  label "DataMsg MCOpening" $ DataMsg <$> liftM2 T.MCOpening get get

instance Cbor.Bi (DataMsg T.MCOpening) where
  encode (DataMsg (T.MCOpening sId opening)) = Cbor.encodeListLen 2 <> Cbor.encode sId <> Cbor.encode opening
  decode = do
    Cbor.enforceSize "DataMsg T.MCOpening" 2
    DataMsg <$> (T.MCOpening <$> Cbor.decode <*> Cbor.decode)

instance Bi (DataMsg T.MCShares) where
    sizeNPut = labelS "DataMsg MCShares" $
        putField (\(DataMsg (T.MCShares st _)) -> st) <>
        putField (\(DataMsg (T.MCShares _ im)) -> im)
    get = label "DataMsg MCShares" $ DataMsg <$> liftM2 T.MCShares get get

instance Cbor.Bi (DataMsg T.MCShares) where
  encode (DataMsg (T.MCShares sId innerMap)) = Cbor.encodeListLen 2 <> Cbor.encode sId <> Cbor.encode innerMap
  decode = do
    Cbor.enforceSize "DataMsg T.MCShares" 2
    DataMsg <$> (T.MCShares <$> Cbor.decode <*> Cbor.decode)

instance Bi (DataMsg T.MCVssCertificate) where
    sizeNPut = labelS "DataMsg MCVssCertificate" $
        putField $ \(DataMsg (T.MCVssCertificate vssCert)) -> vssCert
    get = label "DataMsg MCVssCertificate" $ fmap DataMsg $ T.MCVssCertificate <$> get

instance Cbor.Bi (DataMsg T.MCVssCertificate) where
  encode (DataMsg (T.MCVssCertificate vss)) = Cbor.encode vss
  decode = DataMsg . T.MCVssCertificate <$> Cbor.decode
