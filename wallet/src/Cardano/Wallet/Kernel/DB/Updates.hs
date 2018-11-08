module Cardano.Wallet.Kernel.DB.Updates (
    Updates(..)
  , noUpdates
  , addUpdate
  , removeNextUpdate
  , getNextUpdate
  , clearUpdates
  ) where

import           Universum

import           Control.Lens (iso, makeLenses, (%=), _Wrapped)
import           Data.Coerce (coerce)
import           Data.SafeCopy (base, deriveSafeCopy)

import           Pos.Chain.Update (SoftwareVersion (..))
import           Pos.Core.Chrono

import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Util.AcidState
import           Cardano.Wallet.Kernel.Util.StrictList (StrictList)
import qualified Cardano.Wallet.Kernel.Util.StrictList as SL
import           UTxO.Util (modifyAndGetNew)

-- | Available updates
--
-- Whenever the launcher successfully downloads a new version of the software,
-- that version should be added to this list ('waitForUpdate' in
-- "Cardano.Wallet.Kernel.NodeStateAdaptor" can be used to wait for these
-- notifications).
newtype Updates = Updates {
      _getUpdates :: OldestFirst StrictList (InDb SoftwareVersion)
    }

makeLenses ''Updates
deriveSafeCopy 1 'base ''Updates

updatesList :: Lens' Updates (StrictList SoftwareVersion)
updatesList = getUpdates . _Wrapped . iso coerce coerce

noUpdates :: Updates
noUpdates = Updates $ OldestFirst SL.Nil

addUpdate :: InDb SoftwareVersion -> Update' e Updates ()
addUpdate (InDb upd) = updatesList %= (<> SL.singleton upd)

removeNextUpdate :: Update' e Updates ()
removeNextUpdate = updatesList %= SL.drop 1

clearUpdates :: Update' e Updates ()
clearUpdates = updatesList %= \_ -> mempty

-- | Next the next available update, if any
--
-- We remove any queued updates equal to or older than the current version.
--
-- NOTE: This is adopted from the behaviour of 'nextUpdate' in
-- "Pos.Wallet.Web.Methods.Misc".
getNextUpdate :: InDb SoftwareVersion -- ^ Current
              -> Update' e Updates (Maybe (InDb SoftwareVersion))
getNextUpdate (InDb current) = do
    fmap InDb . SL.toMaybe . view updatesList <$> modifyAndGetNew dropOld
  where
    dropOld :: Updates -> Updates
    dropOld = updatesList %~ SL.dropWhile (not . isUpdateActual)

    isUpdateActual :: SoftwareVersion -> Bool
    isUpdateActual ver = svAppName ver == svAppName current
                      && svNumber  ver >  svNumber  current
