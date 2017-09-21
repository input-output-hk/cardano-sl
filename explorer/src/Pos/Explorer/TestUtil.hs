{-# LANGUAGE UndecidableInstances #-}

module Pos.Explorer.TestUtil where

import qualified Prelude
import           Universum

import           Data.Default                      (def)
import           Data.Text.Buildable               (build)
import           Serokell.Data.Memory.Units        (Byte, Gigabyte, convertUnit)
import           Test.QuickCheck                   (Arbitrary (..), Property, Testable,
                                                    counterexample, forAll, property)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary)

import           Pos.Arbitrary.Block               ()
import           Pos.Block.Core                    (Block, BlockHeader, MainBlock)
import           Pos.Block.Logic                   (RawPayload (..), createMainBlockPure)
import           Pos.Block.Types                   (SlogUndo, Undo)
import qualified Pos.Communication                 ()
import           Pos.Core                          (HasCoreConstants,
                                                    SlotId (..),
                                                    giveStaticConsts)
import           Pos.Crypto                        (SecretKey)
import           Pos.Delegation                    (DlgPayload, DlgUndo, ProxySKBlockInfo)
import           Pos.Ssc.Class                     (Ssc (..), sscDefaultPayload)
import           Pos.Ssc.GodTossing                (GtPayload (..), SscGodTossing)
import           Pos.Txp.Core                      (TxAux)
import           Pos.Update.Core                   (UpdatePayload (..))


----------------------------------------------------------------
-- Arbitrary and Show instances
----------------------------------------------------------------

-- I used the build function since I suspect that it's safe (even in tests).
instance Prelude.Show SlogUndo where
    show = giveStaticConsts $ show . build

instance Prelude.Show DlgUndo where
    show = show . build

instance Prelude.Show Undo where
    show = giveStaticConsts $ show . build

instance Arbitrary SlogUndo where
    arbitrary = genericArbitrary

instance Arbitrary DlgUndo where
    arbitrary = genericArbitrary

instance Arbitrary Undo where
    arbitrary = giveStaticConsts $ genericArbitrary

----------------------------------------------------------------
-- Utility
-- TODO(ks): Extract this in some common PureBlockTest module in src/?
----------------------------------------------------------------

basicBlockGenericUnsafe
    :: (HasCoreConstants)
    => BlockHeader SscGodTossing
    -> SecretKey
    -> SlotId
    -> Block SscGodTossing
basicBlockGenericUnsafe prevHeader sk slotId = case (basicBlock prevHeader sk slotId) of
    Left e      -> error e
    Right block -> Right block

basicBlock
    :: (HasCoreConstants)
    => BlockHeader SscGodTossing
    -> SecretKey
    -> SlotId
    -> Either Text (MainBlock SscGodTossing)
basicBlock prevHeader sk slotId =
    producePureBlock infLimit prevHeader [] Nothing slotId def (defGTP slotId) def sk
  where
    defGTP :: HasCoreConstants => SlotId -> GtPayload
    defGTP sId = sscDefaultPayload @SscGodTossing $ siSlot sId

    infLimit = convertUnit @Gigabyte @Byte 1

emptyBlk :: (HasCoreConstants, Testable p) => (Either Text (MainBlock SscGodTossing) -> p) -> Property
emptyBlk testableBlock =
    forAll arbitrary $ \(sk, prevHeader, slotId) ->
    testableBlock
        $ producePureBlock infLimit prevHeader [] Nothing slotId def (defGTP slotId) def sk
  where
    defGTP :: HasCoreConstants => SlotId -> GtPayload
    defGTP sId = sscDefaultPayload @SscGodTossing $ siSlot sId

    infLimit = convertUnit @Gigabyte @Byte 1

producePureBlock
    :: HasCoreConstants
    => Byte
    -> BlockHeader SscGodTossing
    -> [TxAux]
    -> ProxySKBlockInfo
    -> SlotId
    -> DlgPayload
    -> SscPayload SscGodTossing
    -> UpdatePayload
    -> SecretKey
    -> Either Text (MainBlock SscGodTossing)
producePureBlock limit prev txs psk slot dlgPay sscPay usPay sk =
    createMainBlockPure limit prev psk slot sk $
    RawPayload txs sscPay dlgPay usPay

leftToCounter :: (ToString s, Testable p) => Either s a -> (a -> p) -> Property
leftToCounter x c = either (\t -> counterexample (toString t) False) (property . c) x


