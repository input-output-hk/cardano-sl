-- | Helper functions related to genesis types.

module Pos.Core.Genesis.Helpers
       ( mkGenesisDelegation
       , recreateGenesisDelegation
       , convertNonAvvmDataToBalances
       ) where

import           Universum

import           Control.Lens (at)
import           Control.Monad.Except (MonadError (throwError))
import qualified Data.HashMap.Strict as HM
import           Formatting (build, sformat, (%))
import           Serokell.Util (allDistinct)

import           Pos.Binary.Class (Bi)
import           Pos.Core.Common (Address, Coin, StakeholderId, addressHash, decodeTextAddress,
                                  unsafeAddCoin, unsafeIntegerToCoin)
import           Pos.Core.Delegation (ProxySKHeavy)
import           Pos.Core.Genesis.Types (GenesisDelegation (..), GenesisNonAvvmBalances (..))
import           Pos.Crypto.Signing (ProxySecretKey (..), isSelfSignedPsk)

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

-- | Generate genesis address distribution out of avvm
-- parameters. Txdistr of the utxo is all empty. Redelegate it in
-- calling funciton.
convertNonAvvmDataToBalances
    :: forall m .
       ( MonadError Text m, Bi Address )
    => HashMap Text Integer
    -> m GenesisNonAvvmBalances
convertNonAvvmDataToBalances balances = GenesisNonAvvmBalances <$> balances'
  where
    balances' :: m (HashMap Address Coin)
    balances' = HM.fromListWith unsafeAddCoin <$> traverse convert (HM.toList balances)
    convert :: (Text, Integer) -> m (Address, Coin)
    convert (txt, i) = do
        addr <- either throwError pure $ decodeTextAddress txt
        return (addr, unsafeIntegerToCoin i)
