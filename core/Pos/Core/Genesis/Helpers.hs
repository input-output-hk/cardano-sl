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
import           Pos.Core.Address (addressHash, decodeTextAddress)
import           Pos.Core.Coin (unsafeAddCoin, unsafeIntegerToCoin)
import           Pos.Core.Delegation.Types (ProxySKHeavy)
import           Pos.Core.Genesis.Types (GenesisDelegation (..), GenesisNonAvvmBalances (..))
import           Pos.Core.Types (Address, Coin, StakeholderId)
import           Pos.Crypto.Signing (ProxySecretKey (..), isSelfSignedPsk)

-- | Safe constructor of 'GenesisDelegation' from a list of PSKs.
mkGenesisDelegation ::
       MonadError Text m
    => [ProxySKHeavy]
    -> m GenesisDelegation
mkGenesisDelegation psks = do
    unless (allDistinct $ pskIssuerPk <$> psks) $
        throwError "all issuers must be distinct"
    let resPairs =
            psks <&> \psk@ProxySecretKey {..} -> (addressHash pskIssuerPk, psk)
    let resMap = HM.fromList resPairs
    recreateGenesisDelegation resMap

-- | Safe constructor of 'GenesisDelegation' from existing map.
recreateGenesisDelegation ::
       MonadError Text m
    => HashMap StakeholderId ProxySKHeavy
    -> m GenesisDelegation
recreateGenesisDelegation pskMap = do
    forM_ (HM.toList pskMap) $ \(k, ProxySecretKey{..}) ->
        when (addressHash pskIssuerPk /= k) $
            throwError $ sformat
                ("wrong issuerPk set as key for delegation map: "%
                 "issuer id = "%build%", cert id = "%build)
                k (addressHash pskIssuerPk)
    when (any isSelfSignedPsk pskMap) $
        throwError "there is a self-signed (revocation) psk"
    let isIssuer ProxySecretKey {..} =
            isJust $ pskMap ^. at (addressHash pskDelegatePk)
    when (any isIssuer pskMap) $
        throwError "one of the delegates is also an issuer, don't do it"
    return $ UnsafeGenesisDelegation pskMap

-- | Generate genesis address distribution out of avvm
-- parameters. Txdistr of the utxo is all empty. Redelegate it in
-- calling funciton.
convertNonAvvmDataToBalances
    :: forall m .
       ( MonadFail m, Bi Address )
    => HashMap Text Integer
    -> m GenesisNonAvvmBalances
convertNonAvvmDataToBalances balances = GenesisNonAvvmBalances <$> balances'
  where
    balances' :: m (HashMap Address Coin)
    balances' = HM.fromListWith unsafeAddCoin <$> traverse convert (HM.toList balances)
    convert :: (Text, Integer) -> m (Address, Coin)
    convert (txt, i) = case decodeTextAddress txt of
        Left err   -> fail (toString err)
        Right addr -> return (addr, unsafeIntegerToCoin i)
