-- | Functions working with PollModifier.

module Pos.Update.Poll.Modifier
       ( modifyPollModifier
       ) where

import           Data.Default          (Default (def))
import           Data.Semigroup        (Semigroup)
import           Universum

import           Pos.Update.Poll.Types (PollModifier (..))

instance Default PollModifier where
    def =
        PollModifier
        { pmBVs = mempty
        , pmAdoptedBVFull = Nothing
        , pmConfirmed = mempty
        , pmConfirmedProps = mempty
        , pmActiveProps = mempty
        , pmSlottingData = Nothing
        , pmEpochProposers = mempty
        }

-- | Unite two PollModifiers. Second argument dominates, i. e. if
-- there are two confliciting modifications, the second one wins.
modifyPollModifier :: PollModifier -> PollModifier -> PollModifier
modifyPollModifier pmOld pmNew = PollModifier
    (pmBVs pmOld <> pmBVs pmNew)
    (pmAdoptedBVFull pmNew <|> pmAdoptedBVFull pmOld)
    (pmConfirmed pmOld <> pmConfirmed pmNew)
    (pmConfirmedProps pmOld <> pmConfirmedProps pmNew)
    (pmActiveProps pmOld <> pmActiveProps pmNew)
    (pmSlottingData pmNew <|> pmSlottingData pmOld)
    (pmEpochProposers pmNew <|> pmEpochProposers pmOld)

instance Semigroup PollModifier where

instance Monoid PollModifier where
    mempty = def
    mappend = modifyPollModifier
