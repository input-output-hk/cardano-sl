module Cardano.Wallet.API.V1.Swagger.Example where

import           Universum

import           Test.QuickCheck (Arbitrary (..), Gen, listOf1, oneof)

import           Cardano.Wallet.Orphans.Arbitrary ()
import           Data.Default (Default (def))
import           Node (NodeId (..))
import           Pos.Arbitrary.Wallet.Web.ClientTypes ()
import           Pos.Client.Txp.Util (InputSelectionPolicy (..))
import           Pos.Util.Mnemonic (Mnemonic)
import           Pos.Wallet.Web.ClientTypes (CUpdateInfo)
import           Pos.Wallet.Web.Methods.Misc (WalletStateSnapshot (..))

import qualified Data.Map.Strict as Map
import qualified Pos.Core.Common as Core
import qualified Pos.Crypto.Signing as Core


class Arbitrary a => Example a where
    example :: Gen a
    example = arbitrary

instance Example ()
instance Example a => Example (NonEmpty a)

-- NOTE: we don't want to see empty list examples in our swagger doc :)
instance Example a => Example [a] where
    example = listOf1 example

-- NOTE: we don't want to see "null" examples in our swagger doc :)
instance Example a => Example (Maybe a) where
    example = Just <$> example

-- NOTE: we don't want to see empty maps in our swagger doc :)
instance (Ord k, Example k, Example v) => Example (Map k v) where
    example = Map.fromList <$> listOf1 ((,) <$> example <*> example)

instance Example CUpdateInfo

instance Example (Mnemonic 12)  where
    example = pure def


instance Example WalletStateSnapshot


-- IMPORTANT: if executing `grep "[]\|null" wallet-new/spec/swagger.json` returns any element - then we have to add Example instances for those objects because we don't want to see [] or null examples in our docs.
--
-- TODO: We should probably add this as a part of our swagger CI script and fail swagger if we find some of them - with instruction to the developer above what is said above.
--
-- Most of it comes to removing Nothing from `Arbitrary (Maybe a)` instance and removing empty list from `Arbitrary [a]` instance. It could be done automatically with some quickcheck hacks but I think it would be an overkill.
