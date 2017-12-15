-- | A way to customize node's behavior (e.g. change SSC algorithm
-- parameters).
module Pos.Behavior
       ( BehaviorConfig(..)
       ) where

import           Universum

import qualified Data.Aeson as A
import           Data.Default (Default (..))

import           Pos.Security.Params (SecurityParams)
import           Pos.Ssc.Behavior (SscBehavior)

data BehaviorConfig = BehaviorConfig
    { bcSecurityParams :: !SecurityParams    -- ^ network
    , bcSscBehavior    :: !SscBehavior       -- ^ SSC
    }
    deriving (Eq, Show)

instance Default BehaviorConfig where
    def = BehaviorConfig def def

instance A.FromJSON BehaviorConfig where
    parseJSON = A.withObject "BehaviorConfig" $ \o -> do
        bcSecurityParams <- o A..: "networkAttacks"
        bcSscBehavior    <- o A..: "ssc"
        pure BehaviorConfig{..}
