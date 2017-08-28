-- | A way to customize node's behavior (e.g. change GodTossing algorithm
-- parameters).
module Pos.Behavior
       ( BehaviorConfig(..)
       ) where

import           Universum

import qualified Data.Aeson                  as A
import           Data.Default                (Default (..))

import           Pos.Security.Params         (SecurityParams)
import           Pos.Ssc.GodTossing.Behavior (GtBehavior)

data BehaviorConfig = BehaviorConfig
    { bcSecurityParams :: !SecurityParams    -- ^ network
    , bcGtBehavior     :: !GtBehavior        -- ^ godtossing
    }
    deriving (Eq, Show)

instance Default BehaviorConfig where
    def = BehaviorConfig def def

instance A.FromJSON BehaviorConfig where
    parseJSON = A.withObject "BehaviorConfig" $ \o -> do
        bcSecurityParams <- o A..: "networkAttacks"
        bcGtBehavior     <- o A..: "godTossing"
        pure BehaviorConfig{..}
