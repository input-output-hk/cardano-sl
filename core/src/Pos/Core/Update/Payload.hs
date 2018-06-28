module Pos.Core.Update.Payload
       ( UpdatePayload (..)
       , checkUpdatePayload
       ) where

import           Universum

import           Control.Monad.Except (MonadError)
import           Data.Default (Default (..))
import           Data.SafeCopy (base, deriveSafeCopySimple)
import qualified Data.Text.Buildable as Buildable
import           Formatting (bprint, (%))
import           Serokell.Util.Text (listJson)

import           Pos.Binary.Class (Cons (..), Field (..), deriveSimpleBi)
import           Pos.Crypto (ProtocolMagic)

import           Pos.Core.Update.Proposal
import           Pos.Core.Update.Vote

-- | Update System payload. 'BodyProof MainBlockchain' contains
-- 'UpdateProof' = @Hash UpdatePayload@.
data UpdatePayload = UpdatePayload
    { upProposal :: !(Maybe UpdateProposal)
    , upVotes    :: ![UpdateVote]
    } deriving (Eq, Show, Generic, Typeable)

instance NFData UpdatePayload

instance Buildable UpdatePayload where
    build UpdatePayload {..}
        | null upVotes = formatMaybeProposal upProposal <> ", no votes"
        | otherwise =
            formatMaybeProposal upProposal <>
            bprint
                ("\n    votes: "%listJson)
                (map formatVoteShort upVotes)

instance Default UpdatePayload where
    def = UpdatePayload Nothing []

checkUpdatePayload
    :: MonadError Text m
    => ProtocolMagic
    -> UpdatePayload
    -> m ()
checkUpdatePayload pm it = do
    -- Linter denies using foldables on Maybe.
    -- Suggests whenJust rather than forM_.
    --
    --   ¯\_(ツ)_/¯
    --
    whenJust (upProposal it) (checkUpdateProposal pm)
    forM_ (upVotes it) (checkUpdateVote pm)


deriveSimpleBi ''UpdatePayload [
    Cons 'UpdatePayload [
        Field [| upProposal :: Maybe UpdateProposal |],
        Field [| upVotes    :: [UpdateVote]         |]
    ]]

deriveSafeCopySimple 0 'base ''UpdatePayload
