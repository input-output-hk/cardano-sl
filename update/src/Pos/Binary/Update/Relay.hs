{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Binary.Update.Relay
       (
       ) where

import           Universum hiding (id)

import           Pos.Binary.Class (Bi (..))
import           Pos.Binary.Core ()
import qualified Pos.Core.Update as U
import           Pos.Crypto (hash)
import           Pos.Infra.Binary ()
import           Pos.Infra.Communication.Types.Relay (DataMsg (..))
import           Pos.Util.Util (cborError)

----------------------------------------------------------------------------
-- Relay
----------------------------------------------------------------------------

instance Bi (DataMsg (U.UpdateProposal, [U.UpdateVote])) where
    encode = encode . dmContents
    decode = do
        c@(up, votes) <- decode
        let !id = hash up
        -- FIXME don't do this in the decoder.
        unless (all ((id ==) . U.uvProposalId) votes) $ cborError $
            "decode@DataMsg@Update: vote's uvProposalId must be equal UpId"
        pure $ DataMsg c

instance Bi (DataMsg U.UpdateVote) where
    encode = encode . dmContents
    decode = DataMsg <$> decode
