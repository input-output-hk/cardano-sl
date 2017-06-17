module Pos.Binary.GodTossing.Relay
       (
       ) where

import           Universum

import           Pos.Binary.Class              (Bi (..), get, label, putField)
import           Pos.Communication.Types.Relay (DataMsg (..))
import qualified Pos.Ssc.GodTossing.Types      as T

instance Bi (DataMsg T.MCCommitment) where
    sizeNPut = putField (\(DataMsg (T.MCCommitment signedComm)) -> signedComm)
    get = fmap DataMsg $ label "DataMsg MCCommitment" $ T.MCCommitment <$> get

instance Bi (DataMsg T.MCOpening) where
    sizeNPut = putField (\(DataMsg (T.MCOpening st _)) -> st) <>
               putField (\(DataMsg (T.MCOpening _ op)) -> op)
    get = fmap DataMsg $ label "DataMsg MCOpening" $ liftM2 T.MCOpening get get

instance Bi (DataMsg T.MCShares) where
    sizeNPut = putField (\(DataMsg (T.MCShares st _)) -> st) <>
               putField (\(DataMsg (T.MCShares _ im)) -> im)
    get = fmap DataMsg $ label "DataMsg MCShares" $ liftM2 T.MCShares get get

instance Bi (DataMsg T.MCVssCertificate) where
    sizeNPut = putField $ \(DataMsg (T.MCVssCertificate vssCert)) -> vssCert
    get = fmap DataMsg $ label "DataMsg MCVssCertificate" $ T.MCVssCertificate <$> get
