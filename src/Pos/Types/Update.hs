{-# LANGUAGE DeriveLift      #-}
{-# LANGUAGE TemplateHaskell #-}

module Pos.Types.Update
       ( UpdateProposal (..)
       , UpdateVote (..)
       , UpdateData (..)
       , SystemTag (getSystemTag)
       , mkSystemTag
       , systemTagMaxLength
       ) where

import           Data.Char                  (isAscii)
import qualified Data.HashMap.Strict        as HM
import           Data.SafeCopy              (base, deriveSafeCopySimple)
import qualified Data.Text                  as T
import           Data.Text.Buildable        (Buildable)
import qualified Data.Text.Buildable        as Buildable
import           Formatting                 (bprint, build, (%))
import           Language.Haskell.TH.Syntax (Lift)
import           Serokell.Util.Text         (listJson)
import           Universum                  hiding (show)

import           Pos.Crypto                 (Hash, PublicKey, Signature)
import           Pos.Script.Type            (ScriptVersion)
import           Pos.Types.Version          (ProtocolVersion, SoftwareVersion)
-- Import instance Safecopy HM.HashMap
import           Pos.Util                   ()

-- | Tag of system for which update data is purposed, e.g. win64, mac32
newtype SystemTag = SystemTag { getSystemTag :: Text }
  deriving (Eq, Ord, Show, Generic, Buildable, Hashable, Lift, Typeable)

systemTagMaxLength :: Integral i => i
systemTagMaxLength = 10

mkSystemTag :: MonadFail m => Text -> m SystemTag
mkSystemTag tag | T.length tag > systemTagMaxLength
                    = fail "SystemTag: too long string passed"
                | T.any (not . isAscii) tag
                    = fail "SystemTag: not ascii string passed"
                | otherwise
                    = pure $ SystemTag tag

-- | Proposal for software update
data UpdateProposal = UpdateProposal
    { upProtocolVersion :: !ProtocolVersion
    , upScriptVersion   :: !ScriptVersion
    , upSoftwareVersion :: !SoftwareVersion
    , upData            :: !(HM.HashMap SystemTag UpdateData)
    }
  deriving (Eq, Show, Generic, Typeable)

instance Buildable UpdateProposal where
    build UpdateProposal {..} =
      bprint (build%" { protocol v"%build%", scripts v"%build%", tags: "%listJson%" }")
        upSoftwareVersion upProtocolVersion upScriptVersion (HM.keys upData)

data UpdateData = UpdateData
    { udAppDiffHash :: !(Hash LByteString)
    , udPkgHash     :: !(Hash LByteString)
    , udUpdaterHash :: !(Hash LByteString)
    }
  deriving (Eq, Show, Generic, Typeable)

-- | Vote for update proposal
data UpdateVote = UpdateVote
    { -- | Public key of stakeholder, who votes
      uvKey       :: !PublicKey
    , -- | Software version to which this vote applies
      uvSoftware  :: !SoftwareVersion
    , -- | Approval/rejection bit
      uvDecision  :: !Bool
    , -- | Signature of (Update proposal, Approval/rejection bit)
      --   by stakeholder
      uvSignature :: !(Signature (UpdateProposal, Bool))
    }
  deriving (Eq, Show, Generic, Typeable)

deriveSafeCopySimple 0 'base ''SystemTag
deriveSafeCopySimple 0 'base ''UpdateData
deriveSafeCopySimple 0 'base ''UpdateProposal
deriveSafeCopySimple 0 'base ''UpdateVote
