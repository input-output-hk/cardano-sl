-- | Customization of update behavior.

module Pos.Update.Behavior
       ( UpdateBehavior (..)
       ) where

import           Universum

import qualified Data.Aeson as A
import           Data.Default (Default (..))

data UpdateBehavior = UpdateBehavior
    { ubEmulateUpdate  :: !Bool
    -- ^ Pretend there is a new update shortly after start. Can be
    -- used for testing.
    } deriving (Show, Eq)

instance Default UpdateBehavior where
    def = UpdateBehavior { ubEmulateUpdate = False }

instance A.FromJSON UpdateBehavior where
    parseJSON = A.withObject "UpdateBehavior" $ \o -> do
        ubEmulateUpdate  <- fromMaybe False <$> o A..:! "emulateUpdate"
        pure UpdateBehavior {..}
