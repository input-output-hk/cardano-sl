module Pos.Core.Update.SoftforkRule
       ( SoftforkRule (..)
       , softforkRuleF
       , checkSoftforkRule
       ) where

import           Universum

import qualified Data.Aeson.Options as S (defaultOptions)
import           Data.Aeson.TH (deriveJSON)
import           Control.Monad.Except (MonadError)
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Formatting (Format, bprint, build, (%))
import qualified Formatting.Buildable as Buildable
import           Text.JSON.Canonical (FromJSON (..), ReportSchemaErrors,
                     ToJSON (..), fromJSField, mkObject)

import           Pos.Binary.Class (Cons (..), Field (..), deriveSimpleBi)
import           Pos.Core.Common (CoinPortion, checkCoinPortion)
import           Pos.Core.Genesis.Canonical ()

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

instance Monad m => ToJSON m SoftforkRule where
    toJSON SoftforkRule {..} =
        mkObject
            [ ("initThd", toJSON srInitThd)
            , ("minThd", toJSON srMinThd)
            , ("thdDecrement", toJSON srThdDecrement)
            ]

instance ReportSchemaErrors m => FromJSON m SoftforkRule where
    fromJSON obj = do
        srInitThd <- fromJSField obj "initThd"
        srMinThd <- fromJSField obj "minThd"
        srThdDecrement <- fromJSField obj "thdDecrement"
        return SoftforkRule {..}

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

deriveJSON S.defaultOptions ''SoftforkRule

deriveSimpleBi ''SoftforkRule [
    Cons 'SoftforkRule [
        Field [| srInitThd      :: CoinPortion |],
        Field [| srMinThd       :: CoinPortion |],
        Field [| srThdDecrement :: CoinPortion |]
    ]]

deriveSafeCopySimple 0 'base ''SoftforkRule -- 💋
