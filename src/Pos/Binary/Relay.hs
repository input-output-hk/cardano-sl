-- | Pos.Communication.Relay serialization instances

module Pos.Binary.Relay () where

import           Universum

import           Pos.Binary.Class                 (Bi (..), label, putField, labelS)
import qualified Pos.Binary.Cbor                  as Cbor
import           Pos.Binary.Crypto                ()
import           Pos.Binary.Ssc                   ()
import           Pos.Binary.Update                ()
import           Pos.Communication.Types.Relay    (DataMsg (..))
import           Pos.Crypto                       (hash)
import           Pos.Delegation.Types             (ProxySKLightConfirmation)
import           Pos.Types                        (ProxySKHeavy, ProxySKLight)
import           Pos.Update.Core                  (UpdateProposal, UpdateVote (..))

instance Bi (DataMsg (UpdateProposal, [UpdateVote])) where
    sizeNPut = labelS "DataMsg (UpdateProposal, [UpdateVote])" $ putField dmContents
    get = label "DataMsg (UpdateProposal, [UpdateVote])" $ do
        c@(up, votes) <- get
        let !id = hash up
        unless (all ((id ==) . uvProposalId) votes) $
            fail "get@DataMsg@Update: vote's uvProposalId must be equal UpId"
        pure $ DataMsg c

instance Cbor.Bi (DataMsg (UpdateProposal, [UpdateVote])) where
  encode = Cbor.encode . dmContents
  decode = do
    c@(up, votes) <- Cbor.decode
    let !id = hash up
    unless (all ((id ==) . uvProposalId) votes) $ fail "decode@DataMsg@Update: vote's uvProposalId must be equal UpId"
    pure $ DataMsg c

instance Bi (DataMsg UpdateVote) where
    sizeNPut = labelS "DataMsg UpdateVote" $ putField dmContents
    get = label "DataMsg UpdateVote" $ DataMsg <$> get

instance Cbor.Bi (DataMsg UpdateVote) where
  encode = Cbor.encode . dmContents
  decode = DataMsg <$> Cbor.decode

instance Bi (DataMsg ProxySKLight) where
    sizeNPut = labelS "DataMsg ProxySKLight" $ putField dmContents
    get = label "DataMsg ProxySKLight" $ DataMsg <$> get

instance Cbor.Bi (DataMsg ProxySKLight) where
  encode = Cbor.encode . dmContents
  decode = DataMsg <$> Cbor.decode

instance Bi (DataMsg ProxySKHeavy) where
    sizeNPut = labelS "DataMsg ProxySKHeavy" $ putField dmContents
    get = label "DataMsg ProxySKHeavy" $ DataMsg <$> get

instance Cbor.Bi (DataMsg ProxySKHeavy) where
  encode = Cbor.encode . dmContents
  decode = DataMsg <$> Cbor.decode

instance Bi (DataMsg ProxySKLightConfirmation) where
    sizeNPut = labelS "DataMsg ProxySKLightConfirmation" $ putField dmContents
    get = label "DataMsg ProxySKLightConfirmation" $ DataMsg <$> get

instance Cbor.Bi (DataMsg ProxySKLightConfirmation) where
  encode = Cbor.encode . dmContents
  decode = DataMsg <$> Cbor.decode
