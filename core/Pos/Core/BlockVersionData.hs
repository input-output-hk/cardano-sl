-- | BlockVersionData-related stuff.

module Pos.Core.BlockVersionData
       ( softforkRuleF
       ) where

import           Universum

import qualified Data.Text.Buildable        as Buildable
import           Formatting                 (Format, bprint, build, int, (%))
import           Serokell.Data.Memory.Units (memory)

import           Pos.Core.Coin              ()
import           Pos.Core.Types             (BlockVersionData (..), SoftforkRule (..))
import           Pos.Util.Util              ()

instance NFData SoftforkRule

instance Buildable SoftforkRule where
    build SoftforkRule {..} =
        bprint ("(init = "%build%", min = "%build%", decrement = "%build%")")
        srInitThd srMinThd srThdDecrement

-- | 'SoftforkRule' formatter which restricts type.
softforkRuleF :: Format r (SoftforkRule -> r)
softforkRuleF = build

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
              ", softfork rule: "%softforkRuleF%
              ", tx fee policy: "%build%
              ", unlock stake epoch: "%build%
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
        bvdSoftforkRule
        bvdTxFeePolicy
        bvdUnlockStakeEpoch

instance NFData BlockVersionData where
