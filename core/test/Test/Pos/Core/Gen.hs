module Test.Pos.Core.Gen
       (
        -- Pos.Core.Common Generators
          genAddrAttributes
        , genAddress
        , genAddrType
        , genAddrSpendingData
        , genAddrStakeDistribution
        , genCoin
        , genCoinPortion
        , genScript
        , genScriptVersion
        , genStakeholderId
       ) where

import           Universum

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Pos.Binary.Core ()
import           Pos.Core.Common (Address (..), AddrAttributes (..),
                                  AddrSpendingData (..),
                                  AddrStakeDistribution (..), AddrType (..),
                                  Coin (..), CoinPortion (..), Script (..),
                                  ScriptVersion, StakeholderId, makeAddress)
import           Test.Pos.Crypto.Gen

----------------------------------------------------------------------------
-- Pos.Core.Common Generators
----------------------------------------------------------------------------

genAddrAttributes :: Gen AddrAttributes
genAddrAttributes = AddrAttributes <$> hap <*> genAddrStakeDistribution
  where
    hap = Just <$> genHDAddressPayload

genAddress :: Gen Address
genAddress = makeAddress <$> genAddrSpendingData <*> genAddrAttributes

genAddrType :: Gen AddrType
genAddrType = Gen.choice [ return ATPubKey
                         , return ATScript
                         , return ATRedeem
                         , ATUnknown <$> Gen.word8 Range.constantBounded
                         ]

genAddrSpendingData :: Gen AddrSpendingData
genAddrSpendingData = Gen.choice gens
  where
    gens = [ PubKeyASD <$> genPublicKey
           , ScriptASD <$> genScript
           , RedeemASD <$> genRedeemPublicKey
           , UnknownASD <$> (Gen.word8 Range.constantBounded) <*> gen32Bytes
           ]

genAddrStakeDistribution :: Gen AddrStakeDistribution
genAddrStakeDistribution = Gen.choice gens
  where
    gens = [ return BootstrapEraDistr
           , SingleKeyDistr <$> genStakeholderId
           , UnsafeMultiKeyDistr <$> genMap
           ]
    genMap = Gen.map Range.constantBounded genPair
    genPair = do
      si <- genStakeholderId
      cp <- genCoinPortion
      return (si, cp)

genCoin :: Gen Coin
genCoin = Coin <$> Gen.word64 Range.constantBounded

genCoinPortion :: Gen CoinPortion
genCoinPortion = CoinPortion <$> Gen.word64 Range.constantBounded

genScript :: Gen Script
genScript = Script <$> genScriptVersion <*> gen32Bytes

genScriptVersion :: Gen ScriptVersion
genScriptVersion = Gen.word16 Range.constantBounded

genStakeholderId :: Gen StakeholderId
genStakeholderId = genAbstractHash genPublicKey

----------------------------------------------------------------------------
-- Helper Generators
----------------------------------------------------------------------------

genBytes :: Int -> Gen ByteString
genBytes n = Gen.bytes (Range.singleton n)

gen32Bytes :: Gen ByteString
gen32Bytes = genBytes 32
