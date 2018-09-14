{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module DelegationSpec where

import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Test.Hspec
import           Test.QuickCheck

import           Delegation
import           Delegation.LedgerState (applyTransaction)
import           Utils

singleCertTx :: Cert -> KeyPair -> Tx
singleCertTx cert keys =
  let txBody = TxBody Set.empty [] (Set.singleton cert)
      sig    = sign (sKey keys) txBody
      txWits = Wits (Set.empty) (Set.singleton $ WitCert (vKey keys) sig)
  in Tx txBody txWits

regCertTx :: KeyPair -> Tx
regCertTx keys = singleCertTx (RegKey (vKey keys)) keys

deRegCertTx :: KeyPair -> Tx
deRegCertTx keys = singleCertTx (DeRegKey (vKey keys)) keys

registerKeyProp :: LedgerState -> KeyPair -> Bool
registerKeyProp ls keys =
  let lsReg = applyTransaction ls (regCertTx keys)
      lsDeReg = applyTransaction lsReg (deRegCertTx keys)
      compStKeys ls1 ls2 = (getStKeys ls1) == (getStKeys ls2)
      alreadyRegistered = Set.member (hashKey (vKey keys)) (getStKeys ls)
  in compStKeys ls lsDeReg || ( alreadyRegistered && compStKeys ls lsReg )

spec :: Spec
spec = do
  it "deregistration undoes registration" $ property registerKeyProp
