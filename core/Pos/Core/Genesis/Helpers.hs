-- | Helper functions related to genesis types.

module Pos.Core.Genesis.Helpers
       ( mkGenesisDelegation
       , convertNonAvvmDataToBalances
       ) where

import           Universum

import           Control.Lens             (at)
import           Control.Monad.Except     (MonadError (throwError))
import qualified Data.HashMap.Strict      as HM
import           Formatting               (build, sformat, (%))
import           Serokell.Util            (allDistinct)

import           Pos.Binary.Class         (Bi)
import           Pos.Core.Address         (addressHash, decodeTextAddress)
import           Pos.Core.Coin            (unsafeAddCoin, unsafeIntegerToCoin)
import           Pos.Core.Genesis.Types   (GenesisDelegation (..),
                                           GenesisNonAvvmBalances (..))
import           Pos.Core.Types           (Address, Coin, ProxySKHeavy, StakeholderId)
import           Pos.Crypto.Signing.Types (ProxySecretKey (..), isSelfSignedPsk)

-- | Safe constructor of 'GenesisDelegation'.
mkGenesisDelegation ::
       MonadError Text m
    => HashMap StakeholderId ProxySKHeavy
    -> m GenesisDelegation
mkGenesisDelegation pskM = do
    forM_ (HM.toList pskM) $ \(k, ProxySecretKey{..}) ->
        when (addressHash pskIssuerPk /= k) $
            throwError $ sformat
                ("wrong issuerPk set as key for delegation map: "%
                 "issuer id = "%build%", cert id = "%build)
                k (addressHash pskIssuerPk)
    unless (allDistinct $ pskIssuerPk <$> psks) $
        throwError "all issuers must be distinct"
    when (any isSelfSignedPsk psks) $
        throwError "there is a self-signed (revocation) psk"
    let resPairs =
            psks <&> \psk@ProxySecretKey {..} -> (addressHash pskIssuerPk, psk)
    let resMap = HM.fromList resPairs
    let isIssuer ProxySecretKey {..} =
            isJust $ resMap ^. at (addressHash pskDelegatePk)
    when (any isIssuer psks) $
        throwError "one of the delegates is also an issuer, don't do it"
    return $ UnsafeGenesisDelegation resMap
  where
    psks = toList pskM

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
