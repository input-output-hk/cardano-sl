-- | Pos.Communication.Relay serialization instances

module Pos.Binary.Relay () where

import           Universum

import           Pos.Binary.Class                 (Bi (..))
import           Pos.Binary.Crypto                ()
import           Pos.Binary.Ssc                   ()
import           Pos.Binary.Update                ()
import           Pos.Communication.Types.Relay    (DataMsg (..))
import           Pos.Crypto                       (hash)
import           Pos.Delegation.Types             (ProxySKLightConfirmation)
import           Pos.Types                        (ProxySKHeavy, ProxySKLight)
import           Pos.Update.Core                  (UpdateProposal, UpdateVote (..))

instance Bi (DataMsg (UpdateProposal, [UpdateVote])) where
  encode = encode . dmContents
  decode = do
    c@(up, votes) <- decode
    let !id = hash up
    unless (all ((id ==) . uvProposalId) votes) $ fail "decode@DataMsg@Update: vote's uvProposalId must be equal UpId"
    pure $ DataMsg c

instance Bi (DataMsg UpdateVote) where
  encode = encode . dmContents
  decode = DataMsg <$> decode

instance Bi (DataMsg ProxySKLight) where
  encode = encode . dmContents
  decode = DataMsg <$> decode

instance Bi (DataMsg ProxySKHeavy) where
  encode = encode . dmContents
  decode = DataMsg <$> decode

instance Bi (DataMsg ProxySKLightConfirmation) where
  encode = encode . dmContents
  decode = DataMsg <$> decode
