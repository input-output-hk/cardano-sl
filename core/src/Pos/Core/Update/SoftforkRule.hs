module Pos.Core.Update.SoftforkRule
       ( SoftforkRule (..)
       , softforkRuleF
       , checkSoftforkRule
       ) where

import           Universum

import           Control.Monad.Except (MonadError)
import qualified Data.Text.Buildable as Buildable
import           Formatting (Format, bprint, build, (%))

import           Pos.Core.Common (CoinPortion, checkCoinPortion)

-- | Values defining softfork resolution rule.
-- If a proposal is confirmed at the 's'-th epoch, softfork resolution threshold
-- at the 't'-th epoch will be
-- 'max spMinThd (spInitThd - (t - s) * spThdDecrement)'.
--
-- Softfork resolution threshold is the portion of total stake such
-- that if total stake of issuers of blocks with some block version is
-- greater than this portion, this block version becomes adopted.
data SoftforkRule = SoftforkRule
    { srInitThd      :: !CoinPortion
    -- ^ Initial threshold (right after proposal is confirmed).
    , srMinThd       :: !CoinPortion
    -- ^ Minimal threshold (i. e. threshold can't become less than
    -- this one).
    , srThdDecrement :: !CoinPortion
    -- ^ Theshold will be decreased by this value after each epoch.
    } deriving (Show, Eq, Ord, Generic)

instance Hashable SoftforkRule
instance NFData SoftforkRule

instance Buildable SoftforkRule where
    build SoftforkRule {..} =
        bprint ("(init = "%build%", min = "%build%", decrement = "%build%")")
        srInitThd srMinThd srThdDecrement

-- | 'SoftforkRule' formatter which restricts type.
softforkRuleF :: Format r (SoftforkRule -> r)
softforkRuleF = build

checkSoftforkRule
    :: (MonadError Text m)
    => SoftforkRule
    -> m ()
checkSoftforkRule SoftforkRule {..} = do
    checkCoinPortion srInitThd
    checkCoinPortion srMinThd
    checkCoinPortion srThdDecrement
