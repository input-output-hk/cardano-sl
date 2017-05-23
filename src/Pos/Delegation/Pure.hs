-- | Pure functions on different delegation datatypes.

module Pos.Delegation.Pure
       ( dlgMemPoolApplyBlock
       , dlgMemPoolDetectLoop
       ) where


import           Universum

import           Control.Lens         (uses, (%=))
import qualified Data.HashMap.Strict  as HM
import qualified Data.HashSet         as HS
import           Data.List            (partition)

import           Pos.Block.Core       (MainBlock, mainBlockDlgPayload)
import           Pos.Core             (ProxySKHeavy)
import           Pos.Crypto           (ProxySecretKey (..), PublicKey)
import           Pos.Delegation.Types (DlgMemPool)


-- ^ Applies block certificates to 'ProxySKHeavyMap'.
dlgMemPoolApplyBlock :: MainBlock ssc -> DlgMemPool -> DlgMemPool
dlgMemPoolApplyBlock block m = flip execState m $ do
    let (toDelete,toReplace) =
            partition (\ProxySecretKey{..} -> pskIssuerPk == pskDelegatePk)
                      (view mainBlockDlgPayload block)
    for_ toDelete $ \psk -> identity %= HM.delete (pskIssuerPk psk)
    for_ toReplace $ \psk -> identity %= HM.insert (pskIssuerPk psk) psk

-- ^ Checks if addition of the psk to the map will lead to
-- loops. Returns nothing if it's good, first-already-visited public
-- key otherwise.
dlgMemPoolDetectLoop :: ProxySKHeavy -> DlgMemPool -> Maybe PublicKey
dlgMemPoolDetectLoop toAdd pskm =
    flip evalState (pskm, HS.singleton $ pskIssuerPk toAdd) $ trav (pskDelegatePk toAdd)
  where
    trav cur = ifM (uses _2 $ HS.member cur) (pure $ Just cur) $ do
        next <- uses _1 $ HM.lookup cur
        _2 %= HS.insert cur
        maybe (pure Nothing) (trav . pskDelegatePk) next
