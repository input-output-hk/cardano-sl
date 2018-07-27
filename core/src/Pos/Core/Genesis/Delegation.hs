module Pos.Core.Genesis.Delegation
       ( GenesisDelegation (..)
       , noGenesisDelegation
       , mkGenesisDelegation
       , recreateGenesisDelegation
       ) where

import           Universum hiding (elems)

import qualified Data.Aeson as Aeson (FromJSON (..), ToJSON (..))
import           Control.Lens (at)
import           Control.Monad.Except (MonadError (throwError))
import qualified Data.HashMap.Strict as HM
import           Formatting (build, sformat, (%))
import           Serokell.Util (allDistinct)
import           Text.JSON.Canonical (FromJSON (..), ReportSchemaErrors,
                     ToJSON (..))

import           Pos.Core.Common (StakeholderId, addressHash)
import           Pos.Core.Delegation (ProxySKHeavy)
import           Pos.Crypto.Signing (ProxySecretKey (..), isSelfSignedPsk)
import           Pos.Util.Json.Parse (wrapConstructor)
import           Pos.Util.Util (toAesonError)

-- | This type contains genesis state of heavyweight delegation. It
-- wraps a map where keys are issuers (i. e. stakeholders who
-- delegated) and values are proxy signing keys. There are some invariants:
-- 1. In each pair delegate must differ from issuer, i. e. no revocations.
-- 2. PSKs must be consistent with keys in the map, i. e. issuer's ID must be
--    equal to the key in the map.
-- 3. Delegates can't be issuers, i. e. transitive delegation is not supported.
--    It's not needed in genesis, it can always be reduced.
newtype GenesisDelegation = UnsafeGenesisDelegation
    { unGenesisDelegation :: HashMap StakeholderId ProxySKHeavy
    } deriving (Show, Eq, Container)

instance Monad m => ToJSON m GenesisDelegation where
    toJSON = toJSON . unGenesisDelegation

instance ReportSchemaErrors m => FromJSON m GenesisDelegation where
    fromJSON val = do
        psks <- fromJSON val
        wrapConstructor $ recreateGenesisDelegation psks

instance Aeson.ToJSON GenesisDelegation where
    toJSON = Aeson.toJSON . unGenesisDelegation

instance Aeson.FromJSON GenesisDelegation where
    parseJSON = Aeson.parseJSON >=> \v -> do
        (elems :: HashMap StakeholderId ProxySKHeavy) <- mapM Aeson.parseJSON v
        toAesonError $ recreateGenesisDelegation elems

-- | Empty 'GenesisDelegation'.
noGenesisDelegation :: GenesisDelegation
noGenesisDelegation = UnsafeGenesisDelegation mempty

-- | Safe constructor of 'GenesisDelegation' from a list of PSKs.
mkGenesisDelegation ::
       MonadError Text m
    => [ProxySKHeavy]
    -> m GenesisDelegation
mkGenesisDelegation psks = do
    unless (allDistinct $ pskIssuerPk <$> psks) $
        throwError "all issuers must be distinct"
    let res = HM.fromList [(addressHash (pskIssuerPk psk), psk) | psk <- psks]
    recreateGenesisDelegation res

-- | Safe constructor of 'GenesisDelegation' from existing map.
recreateGenesisDelegation ::
       MonadError Text m
    => HashMap StakeholderId ProxySKHeavy
    -> m GenesisDelegation
recreateGenesisDelegation pskMap = do
    forM_ (HM.toList pskMap) $ \(k, psk) ->
        when (addressHash (pskIssuerPk psk) /= k) $
            throwError $ sformat
                ("wrong issuerPk set as key for delegation map: "%
                 "issuer id = "%build%", cert id = "%build)
                k (addressHash (pskIssuerPk psk))
    when (any isSelfSignedPsk pskMap) $
        throwError "there is a self-signed (revocation) psk"
    let isIssuer psk =
            isJust $ pskMap ^. at (addressHash (pskDelegatePk psk))
    when (any isIssuer pskMap) $
        throwError "one of the delegates is also an issuer, don't do it"
    return $ UnsafeGenesisDelegation pskMap
