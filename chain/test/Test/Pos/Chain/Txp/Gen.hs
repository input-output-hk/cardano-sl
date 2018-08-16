module Test.Pos.Chain.Txp.Gen
       ( genTxpConfiguration
       ) where

import           Universum

import qualified Data.Set as S
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Pos.Chain.Txp (RequiresNetworkMagic (..),
                     TxpConfiguration (..))

import           Test.Pos.Core.Gen (genAddress)

genRequiresNetworkMagic :: Gen RequiresNetworkMagic
genRequiresNetworkMagic = Gen.element [NMMustBeNothing, NMMustBeJust]

genTxpConfiguration :: Gen TxpConfiguration
genTxpConfiguration = do
    limit <- Gen.int (Range.constant 0 200)
    addrs <- Gen.list (Range.linear 0 50) genAddress
    rnm   <- genRequiresNetworkMagic
    return (TxpConfiguration limit (S.fromList addrs) rnm)
