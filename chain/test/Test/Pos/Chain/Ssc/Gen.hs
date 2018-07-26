module Test.Pos.Chain.Ssc.Gen
       ( genAttackTarget
       ) where

import           Universum

import qualified Data.ByteString.Char8 as BC
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Pos.Chain.Security (AttackTarget (..))
import           Test.Pos.Core.Gen (genStakeholderId)


----------------------------------------------------------------------------
-- Pos.Chain.Ssc.Security Generators
----------------------------------------------------------------------------

genAttackTarget :: Gen AttackTarget
genAttackTarget = do
    netHostAddr <- genHost
    port <- Gen.word16 (Range.constant 1 65535)
    Gen.choice [ pure $ NetworkAddressTarget (netHostAddr, port)
               , PubKeyAddressTarget <$> genStakeholderId
               ]

genHost :: Gen ByteString
genHost = BC.pack <$> Gen.string (Range.constant 1 10) Gen.alphaNum
