module Cardano.Wallet.API.V1.Swagger.Example where

import           Universum

import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.V1.Types
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import           Data.Default (Default (def))
import           Node (NodeId (..))

import           Pos.Client.Txp.Util (InputSelectionPolicy (..))
import           Test.QuickCheck (Arbitrary (..), Gen, listOf1)

import           Cardano.Wallet.Orphans.Arbitrary ()
import           Pos.Arbitrary.Wallet.Web.ClientTypes ()
import           Pos.Wallet.Web.ClientTypes (CUpdateInfo)
import           Pos.Wallet.Web.Methods.Misc (WalletStateSnapshot (..))
import           Test.Pos.Wallet.Arbitrary.Web.ClientTypes ()

import qualified Data.Map.Strict as Map


class Arbitrary a => Example a where
    example :: Gen a
    example = arbitrary

instance Example ()
instance Example a => Example (NonEmpty a)

-- NOTE: we don't want to see empty list examples in our swagger doc :)
instance Example a => Example [a] where
    example = listOf1 example

instance (IxSet.Indexable a, Example a) => Example (IxSet.IxSet a) where
    example = IxSet.fromList <$> listOf1 example

-- NOTE: we don't want to see "null" examples in our swagger doc :)
instance Example a => Example (Maybe a) where
    example = Just <$> example

-- NOTE: we don't want to see empty maps in our swagger doc :)
instance (Ord k, Example k, Example v) => Example (Map k v) where
    example = Map.fromList <$> listOf1 ((,) <$> example <*> example)

instance Example CUpdateInfo
instance Example WalletStateSnapshot


-- IMPORTANT: if executing `grep "[]\|null" wallet-new/spec/swagger.json` returns any element - then we have to add Example instances for those objects because we don't want to see [] or null examples in our docs.
--
-- TODO: We should probably add this as a part of our swagger CI script and fail swagger if we find some of them - with instruction to the developer above what is said above.
--
