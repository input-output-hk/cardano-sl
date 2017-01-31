module Pos.Communication.Types.Relay
       ( InvMsg (..)
       , ReqMsg (..)
       , DataMsg (..)
       , InvOrData
       ) where

import qualified Data.Text.Buildable as B
import           Formatting          (bprint, build, (%))
import           Test.QuickCheck     (Arbitrary (..))
import           Universum

-- | Inventory message. Can be used to announce the fact that you have
-- some data.
data InvMsg key tag = InvMsg
    { imTag  :: !tag
    , imKeys :: !(NonEmpty key)
    }

deriving instance (Show key, Show tag) => Show (InvMsg key tag)
deriving instance (Eq key, Eq tag) => Eq (InvMsg key tag)
instance (Arbitrary key, Arbitrary tag) => Arbitrary (InvMsg key tag) where
    arbitrary = InvMsg <$> arbitrary <*> arbitrary

-- | Request message. Can be used to request data (ideally data which
-- was previously announced by inventory message).
data ReqMsg key tag = ReqMsg
    { rmTag  :: !tag
    , rmKeys :: !(NonEmpty key)
    }

deriving instance (Show key, Show tag) => Show (ReqMsg key tag)
deriving instance (Eq key, Eq tag) => Eq (ReqMsg key tag)
instance (Arbitrary key, Arbitrary tag) => Arbitrary (ReqMsg key tag) where
    arbitrary = ReqMsg <$> arbitrary <*> arbitrary

-- | Data message. Can be used to send actual data.
data DataMsg key contents = DataMsg
    { dmContents :: !contents
    , dmKey      :: !key
    }

type InvOrData tag key contents = Either (InvMsg key tag) (DataMsg key contents)

deriving instance (Show key, Show contents) => Show (DataMsg key contents)
deriving instance (Eq key, Eq contents) => Eq (DataMsg key contents)

instance (Buildable key, Buildable contents) => Buildable (DataMsg key contents) where
    build (DataMsg key contents) =
        bprint ("key = "%build%", contents = "%build) key contents
