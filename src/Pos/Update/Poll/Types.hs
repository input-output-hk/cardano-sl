-- | Types related to Poll monad.

module Pos.Update.Poll.Types
       ( PollModifier (..)
       ) where

import           Data.Default    (Default (def))
import           Universum

import           Pos.DB.Types    (ProposalState)
import           Pos.Script.Type (ScriptVersion)
import           Pos.Types       (ApplicationName, NumSoftwareVersion, ProtocolVersion)
import           Pos.Update.Core (UpId)

-- | PollModifier is used in verification. It represents operation which
-- one should apply to global state to obtain result of application of
-- MemPool or blocks which are verified.
data PollModifier = PollModifier
    { pmNewScriptVersions :: !(HashMap ProtocolVersion ScriptVersion)
    , pmLastAdoptedPV     :: !(Maybe ProtocolVersion)
    , pmNewConfirmed      :: !(HashMap ApplicationName NumSoftwareVersion)
    , pmNewActiveProps    :: !(HashMap UpId ProposalState)
    , pmDelActiveProps    :: !(HashSet UpId)
    , pmNewActivePropsIdx :: !(HashMap ApplicationName UpId)
    , pmDelActivePropsIdx :: !(HashSet ApplicationName)
    }

instance Default PollModifier where
    def =
        PollModifier
        { pmNewScriptVersions = mempty
        , pmLastAdoptedPV = Nothing
        , pmNewConfirmed = mempty
        , pmNewActiveProps = mempty
        , pmDelActiveProps = mempty
        , pmNewActivePropsIdx = mempty
        , pmDelActivePropsIdx = mempty
        }
