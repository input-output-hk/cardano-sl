{-# LANGUAGE NumDecimals #-}

module Test.Pos.Chain.Genesis.Dummy
       ( dummyConfig
       , dummyConfigStartTime
       , dummyProtocolConstants
       , dummyK
       , dummyEpochSlots
       , dummySlotSecurityParam
       , dummyGenesisInitializer
       , dummyGenesisAvvmBalances
       , dummyGeneratedGenesisData
       , dummyGeneratedSecrets
       , dummyGenesisSecretKeys
       , dummyGenesisSecretKeysRich
       , dummyGenesisSecretKeysPoor
       , dummyGenesisSecretsRich
       , dummyGenesisSecretsPoor
       , dummyGenesisSpec
       , dummyBlockVersionData
       , dummyGenesisData
       , dummyGenesisDataStartTime
       , dummyGenesisHash
       , dummyTxValRulesConfig
       , dummyTxValRules
       ) where

import           Universum

import           Pos.Chain.Genesis (Config (..), FakeAvvmOptions (..),
                     GeneratedGenesisData (..), GeneratedSecrets (..),
                     GenesisAvvmBalances (..), GenesisData (..),
                     GenesisHash (..), GenesisInitializer (..),
                     GenesisSpec (..), PoorSecret, RichSecrets (..),
                     TestnetBalanceOptions (..), generateGenesisData,
                     genesisProtocolConstantsFromProtocolConstants,
                     gsSecretKeys, gsSecretKeysPoor, gsSecretKeysRich,
                     mkConfig, noGenesisDelegation)
import           Pos.Chain.Txp (TxValidationRules (..),
                     TxValidationRulesConfig (..), mkLiveTxValidationRules)
import           Pos.Chain.Update (BlockVersionData (..), SoftforkRule (..))
import           Pos.Core (BlockCount, Coeff (..), EpochIndex (..),
                     ProtocolConstants (..), SharedSeed (..), SlotCount,
                     Timestamp, TxFeePolicy (..), TxSizeLinear (..),
                     VssMaxTTL (..), VssMinTTL (..), kEpochSlots,
                     kSlotSecurityParam, pcBlkSecurityParam,
                     unsafeCoinPortionFromDouble)
import           Pos.Crypto (SecretKey)

import           Test.Pos.Crypto.Dummy (dummyProtocolMagic)

dummyConfig :: Config
dummyConfig = dummyConfigStartTime 0

dummyConfigStartTime :: Timestamp -> Config
dummyConfigStartTime ts = mkConfig ts dummyGenesisSpec dummyTxValRulesConfig

dummyProtocolConstants :: ProtocolConstants
dummyProtocolConstants = ProtocolConstants
    { pcK         = 10
    , pcVssMinTTL = VssMinTTL 2
    , pcVssMaxTTL = VssMaxTTL 6
    }

dummyK :: BlockCount
dummyK = pcBlkSecurityParam dummyProtocolConstants

dummyEpochSlots :: SlotCount
dummyEpochSlots = kEpochSlots dummyK

dummySlotSecurityParam :: SlotCount
dummySlotSecurityParam = kSlotSecurityParam dummyK

dummyGeneratedGenesisData :: GeneratedGenesisData
dummyGeneratedGenesisData = generateGenesisData dummyProtocolMagic
                                                dummyProtocolConstants
                                                dummyGenesisInitializer
                                                dummyGenesisAvvmBalances

dummyGeneratedSecrets :: GeneratedSecrets
dummyGeneratedSecrets = ggdSecrets dummyGeneratedGenesisData

dummyGenesisSecretsRich :: [RichSecrets]
dummyGenesisSecretsRich = gsRichSecrets dummyGeneratedSecrets

dummyGenesisSecretsPoor :: [PoorSecret]
dummyGenesisSecretsPoor = gsPoorSecrets dummyGeneratedSecrets

dummyGenesisSecretKeys :: [SecretKey]
dummyGenesisSecretKeys = gsSecretKeys dummyGeneratedSecrets

dummyGenesisSecretKeysRich :: [SecretKey]
dummyGenesisSecretKeysRich = gsSecretKeysRich dummyGeneratedSecrets

dummyGenesisSecretKeysPoor :: [SecretKey]
dummyGenesisSecretKeysPoor = gsSecretKeysPoor dummyGeneratedSecrets

dummyGenesisSpec :: GenesisSpec
dummyGenesisSpec = UnsafeGenesisSpec
    dummyGenesisAvvmBalances
    dummyFtsSeed
    noGenesisDelegation
    dummyBlockVersionData
    (genesisProtocolConstantsFromProtocolConstants dummyProtocolConstants
                                                   dummyProtocolMagic
    )
    dummyGenesisInitializer

dummyGenesisAvvmBalances :: GenesisAvvmBalances
dummyGenesisAvvmBalances = GenesisAvvmBalances mempty

dummyFtsSeed :: SharedSeed
dummyFtsSeed = SharedSeed "c2tvdm9yb2RhIEdndXJkYSBib3JvZGEgcHJvdm9kYSA="

dummyBlockVersionData :: BlockVersionData
dummyBlockVersionData = BlockVersionData
    0
    7000
    2000000
    2000000
    4096
    700
    (unsafeCoinPortionFromDouble 0.01)
    (unsafeCoinPortionFromDouble 0.005)
    (unsafeCoinPortionFromDouble 0.001)
    (unsafeCoinPortionFromDouble 0.1)
    10
    (SoftforkRule (unsafeCoinPortionFromDouble 0.9)
                  (unsafeCoinPortionFromDouble 0.6)
                  (unsafeCoinPortionFromDouble 0.05)
    )
    (TxFeePolicyTxSizeLinear $ TxSizeLinear (Coeff 155381) (Coeff 43.946))
    (EpochIndex maxBound)

dummyGenesisInitializer :: GenesisInitializer
dummyGenesisInitializer = GenesisInitializer
    (TestnetBalanceOptions 12 4 6e17 0.99 True)
    (FakeAvvmOptions 10 100000)
    (unsafeCoinPortionFromDouble 1)
    True
    0

dummyGenesisData :: GenesisData
dummyGenesisData = configGenesisData dummyConfig

dummyGenesisDataStartTime :: Timestamp -> GenesisData
dummyGenesisDataStartTime = configGenesisData . dummyConfigStartTime

dummyGenesisHash :: GenesisHash
dummyGenesisHash = configGenesisHash dummyConfig

-- | A TxValidationRules which has not gone into effect yet
dummyTxValRules :: TxValidationRules
dummyTxValRules = mkLiveTxValidationRules currentEpoch dummyTxValRulesConfig
  where
    currentEpoch = EpochIndex 0


-- In order to limit the `Attributes` size in a Tx, `TxValidationRules` was
-- created in https://github.com/input-output-hk/cardano-sl/pull/3878. The
-- value is checked in `checkTx`.
dummyTxValRulesConfig :: TxValidationRulesConfig
dummyTxValRulesConfig = TxValidationRulesConfig
                            cutOffEpoch
                            addAttribSizeRes
                            txAttribSizeRes
  where
    -- The epoch from which the validation rules in `checkTx` are enforced.
    cutOffEpoch = (EpochIndex 1)
    -- The size limit of `Addr Attributes`.
    addAttribSizeRes = 128
    -- The size limit of `TxAttributes`.
    txAttribSizeRes = 128
