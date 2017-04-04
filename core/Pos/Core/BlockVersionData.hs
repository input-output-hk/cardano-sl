-- | BlockVersionData-related stuff.

module Pos.Core.BlockVersionData
       (
       ) where

import           Universum

import qualified Data.Text.Buildable        as Buildable
import           Formatting                 (bprint, build, int, (%))
import           Serokell.Data.Memory.Units (memory)

import           Pos.Core.Coin              ()
import           Pos.Core.Types             (BlockVersionData (..))
import           Pos.Util.Util              ()

instance Buildable BlockVersionData where
    build BlockVersionData {..} =
      bprint ("{ scripts v"%build%
              ", slot duration: "%int%" mcs"%
              ", block size limit: "%memory%
              ", header size limit: "%memory%
              ", tx size limit: "%memory%
              ", proposal size limit: "%memory%
              ", mpc threshold: "%build%
              ", heavyweight delegation threshold: "%build%
              ", update vote threshold: "%build%
              ", update proposal threshold: "%build%
              ", update implicit period: "%int%" slots"%
              ", update softfork threshold: "%build%
              " }")
        bvdScriptVersion
        bvdSlotDuration
        bvdMaxBlockSize
        bvdMaxHeaderSize
        bvdMaxTxSize
        bvdMaxProposalSize
        bvdMpcThd
        bvdHeavyDelThd
        bvdUpdateVoteThd
        bvdUpdateProposalThd
        bvdUpdateImplicit
        bvdUpdateSoftforkThd

instance NFData BlockVersionData where
