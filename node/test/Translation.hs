-- | Test that we can translate from UTxO to the abstract chain.
module Translation
    ( spec
    ) where

import           Chain.Abstract
import           Chain.Abstract.Translate.FromUTxO
import           Data.Set (Set)
import qualified Data.Set as Set
import           Infrastructure.Generator
import           Pos.Core.Chrono (OldestFirst (..))
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.Pos.Configuration (withDefConfiguration)
import           Universum
import qualified UTxO.DSL as DSL
import qualified Chain.Abstract as Abs
import qualified Data.Map.Strict as Map
import           Data.List.NonEmpty
-- import           UTxO.Context


spec :: Spec
spec = do
    describe "Chain translation verification" $ do
      it "A bootstrap transaction must contain at least one output and no input" $ do
        -- let addrs = (Addr (IxRich 0) 0) :| [] -- from UTxO.Context
        let addrs = (Addr 1024) :| [Addr 2048] -- from Chain.Abstract
        let oDsl0 = DSL.Output
                { outAddr = head addrs
                , outVal  = 10 :: DSL.Value
                } :: DSL.Output DSL.GivenHash Addr
        let trDsl0 = DSL.Transaction
                { trFresh = 2 :: DSL.Value
                , trIns   = Set.empty
                , trOuts  = [oDsl0]
                , trFee   = 3 :: DSL.Value
                , trHash  = 123120 :: Int
                , trExtra = [] }
        let inDsl1 = DSL.Input
                { inpTrans = DSL.givenHash trDsl0
                , inpIndex = 0 }
        let oDsl1 = DSL.Output
                { outAddr = addrs !! 1
                , outVal  = 5 :: DSL.Value
                } :: DSL.Output DSL.GivenHash Addr
        let trDsl1 = DSL.Transaction
                { trFresh = 2 :: DSL.Value
                , trIns   = Set.fromList [inDsl1]
                , trOuts  = [oDsl1]
                , trFee   = 3 :: DSL.Value
                , trHash  = 54209842 :: Int
                , trExtra = [] }
        let blDsl = OldestFirst { getOldestFirst = [trDsl0, trDsl1] }
        let chDsl = OldestFirst { getOldestFirst = [blDsl] }

        let oAbs = Abs.Output
                { outAddr        = head addrs
                , outVal         = 10 :: DSL.Value
                , outRepartition = Repartition (Map.empty :: Map.Map Addr (Sum Int))
                }
        let trAbs1 = Abs.Transaction
                { trFresh   = 2 :: DSL.Value
                , trIns     = inDsl1 :| []
                , trOuts    = oAbs :| []
                , trFee     = 3 :: DSL.Value
                , trHash    = DSL.trHash trDsl1
                , trExtra   = []
                , trWitness = addrs !! 0 :| []
                }
        let blAbs = Abs.Block
                { blockPred         = BlockHash 0 -- this is the genesis block so not sure what to put here
                , blockSlot         = SlotId 0
                , blockIssuer       = addrs !! 0
                , blockTransactions = OldestFirst { getOldestFirst = [trAbs1] }
                , blockDlg          = []
                }
        let chAbs = OldestFirst { getOldestFirst = [blAbs] }

        -- TODO(md): see about Show instances for types when using `shouldBe`
        -- outDSL oAbs `shouldBe` oDsl0

        -- TODO(md): Finalise this property
        -- translate @DSL.GivenHash @IO addrs chDsl [] `shouldBe` (return $ Right chAbs)
        pending
