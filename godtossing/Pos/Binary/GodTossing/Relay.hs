module Pos.Binary.GodTossing.Relay
       (
       ) where

import           Universum

import           Pos.Binary.Class              (Bi, get, label, put)
import           Pos.Communication.Types.Relay (DataMsg (..))
import qualified Pos.Ssc.GodTossing.Types      as T

instance Bi (DataMsg T.MCCommitment) where
    put (DataMsg (T.MCCommitment signedComm)) = put signedComm
    get = fmap DataMsg $ label "DataMsg MCCommitment" $
              T.MCCommitment <$> get

instance Bi (DataMsg T.MCOpening) where
    put (DataMsg (T.MCOpening st op)) = put st >> put op
    get = fmap DataMsg $ label "DataMsg MCOpening" $
              T.MCOpening <$> get <*> get

instance Bi (DataMsg T.MCShares) where
    put (DataMsg (T.MCShares st im)) = put st >> put im
    get = fmap DataMsg $ label "DataMsg MCShares" $
              T.MCShares <$> get <*> get

instance Bi (DataMsg T.MCVssCertificate) where
    put (DataMsg (T.MCVssCertificate vssCert)) = put vssCert
    get = fmap DataMsg $ label "DataMsg MCVssCertificate" $
              T.MCVssCertificate <$> get
