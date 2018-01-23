-- | Wallet unit tests
--
-- TODO: Take advantage of https://github.com/input-output-hk/cardano-sl/pull/2296 ?
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main (main) where

import Universum
import Data.List (last)
import Formatting (sformat, bprint, build, (%), shown)
import Test.Hspec
import Prelude (Show(..))
import qualified Data.Text.Buildable

import qualified Pos.Block.Error as Cardano
import qualified Pos.Txp.Toil    as Cardano
import Serokell.Util (mapJson)

import UTxO.Bootstrap
import UTxO.Context
import UTxO.DSL hiding (example1)
import UTxO.Fees
import UTxO.Interpreter
import UTxO.Translate

{-------------------------------------------------------------------------------
  Main test driver
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    -- _showContext
    hspec tests

-- | Debugging: show the translation context
_showContext :: IO ()
_showContext = do
    putStrLn $ runTranslateNoErrors $ withConfig $
      sformat build <$> ask
    putStrLn $ runTranslateNoErrors $
      dumpStr . bootstrapTransactions <$> ask

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

tests :: Spec
tests = describe "Wallet unit tests" $ do
    testSanityChecks

testSanityChecks :: Spec
testSanityChecks = describe "Test sanity checks" $ do
    it "can construct and verify empty block" $
      intAndVerify emptyBlock `shouldSatisfy` expectValid

    it "can construct and verify block with one transaction" $
      intAndVerify oneTrans `shouldSatisfy` expectValid

    it "can construct and verify example 1 from the UTxO paper" $
      intAndVerify example1 `shouldSatisfy` expectValid

    it "can reject overspending" $
      intAndVerify overspend `shouldSatisfy` expectInvalid

    it "can reject double spending" $
      intAndVerify doublespend `shouldSatisfy` expectInvalid
  where
    emptyBlock :: PreChain
    emptyBlock = PreChain $ \_boot _fees -> [[]]

    oneTrans :: PreChain
    oneTrans = PreChain $ \boot ((fee : _) : _) ->
      let t1 = Transaction {
            trIns  = [ Input boot 0 ] -- rich 0
          , trOuts = [ Output (AddrOrdinary r1) 1000
                     , Output (AddrOrdinary r0) (initR0 - 1000 - fee)
                     , Output AddrTreasury      fee
                     ]
          }
      in [[t1]]

    -- Try to transfer from R0 to R1, but leaving R0's balance the same
    overspend :: PreChain
    overspend = PreChain $ \boot ((fee : _) : _) ->
      let t1 = Transaction {
                   trIns  = [ Input boot 0 ] -- rich 0
                 , trOuts = [ Output (AddrOrdinary r1) 1000
                            , Output (AddrOrdinary r0) initR0
                            , Output AddrTreasury      fee
                            ]
                 }
      in [[t1]]

    -- Try to transfer from R0 and R1 using the same output
    -- TODO: in principle this example /ought/ to work without any kind of
    -- outputs at all; but in practice this breaks stuff because now we have
    -- two identical transactions which would therefore get identical IDs?
    doublespend :: PreChain
    doublespend = PreChain $ \boot _fees ->
      let t1 = Transaction {
                   trIns  = [ Input boot 0 ] -- rich 0
                 , trOuts = [ Output (AddrOrdinary r1) 1000 ]
                 }
          t2 = Transaction {
                       trIns  = [ Input boot 0 ] -- rich 0
                     , trOuts = [ Output (AddrOrdinary r2) 1000 ]
                     }
      in [[t1, t2]]

    -- Translation of example 1 of the paper, adjusted to allow for fees
    --
    -- Transaction t1 in the example creates new coins, and transaction t2
    -- tranfers this to an ordinary address. In other words, t1 and t2
    -- corresponds to the bootstrap transactions.
    --
    -- Transaction t3 then transfers part of R0's balance to R1, returning the
    -- rest to back to R0; and t4 transfers the remainder of R0's balance to
    -- R2.
    --
    -- Transaction 5 in example 1 is a transaction /from/ the treasury /to/ an
    -- ordinary address. This currently has no equivalent in Cardano, so we omit
    -- it.
    example1 :: PreChain
    example1 = PreChain $ \boot ((fee3 : fee4 : _) : _) ->
      let t3 = Transaction {
                   trIns  = [ Input boot 0 ] -- rich 0
                 , trOuts = [ Output (AddrOrdinary r1) 1000
                            , Output (AddrOrdinary r0) (initR0 - 1000 - fee3)
                            , Output AddrTreasury      fee3
                            ]
                 }
          t4 = Transaction {
                   trIns  = [ Input t3 1 ]
                 , trOuts = [ Output (AddrOrdinary r2) (initR0 - 1000 - fee3 - fee4)
                            , Output AddrTreasury      fee4
                            ]
                 }
      in [[t3, t4]]

    initR0 = 11137499999752500

    r0 = Addr (IxRich 0) 0
    r1 = Addr (IxRich 1) 0
    r2 = Addr (IxRich 2) 0

{-------------------------------------------------------------------------------
  Chain with some information still missing
-------------------------------------------------------------------------------}

newtype PreChain = PreChain (Transaction Addr -> [[Fee]] -> [[Transaction Addr]])

fromPreChain :: PreChain -> Translate IntException (Chain Addr, Ledger Addr)
fromPreChain (PreChain f) = do
    boot <- asks bootstrapTransactions
    txs  <- calculateFees (f (last boot))
    let chain  = Chain txs -- doesn't include the boot transactions
        ledger = chainToLedger chain ++ reverse boot
    return (chain, ledger)

{-------------------------------------------------------------------------------
  Verify chain
-------------------------------------------------------------------------------}

-- | Interpret and verify a chain, given the bootstrap transactions
intAndVerify :: PreChain -> ValidationResult
intAndVerify pc = runInterpret $ do
    (chain, ledger) <- fromPreChain pc
    let dslIsValid = isValidLedger ledger
        dslUtxo    = utxoAfter     ledger
    mChain' <- catchTranslateErrors $ int chain
    case mChain' of
      Left e ->
        if dslIsValid
          then return $ Disagreement (UnexpectedError e)
          else return $ ExpectedInvalid (Right e)
      Right chain' -> do
        isCardanoValid <- verifyBlocksPrefix chain'
        case (dslIsValid, isCardanoValid) of
          (False, Left e)  -> return $ ExpectedInvalid (Left e)
          (False, Right _) -> return $ Disagreement UnexpectedValid
          (True,  Left e)  -> return $ Disagreement (UnexpectedInvalid e)
          (True,  Right (_undo, utxo)) -> do
            utxo' <- int dslUtxo
            if utxo == utxo'
              then return $ ExpectedValid
              else return $ Disagreement (UnexpectedUtxo dslUtxo utxo utxo')

{-------------------------------------------------------------------------------
  Chain verification test result
-------------------------------------------------------------------------------}

data ValidationResult =
    -- | We expected the chain to be valid; DSL and Cardano both agree
    ExpectedValid

    -- | We expected the chain to be invalid; DSL and Cardano both agree
    --
    -- We record the error message we get from Cardano (the DSL just reports
    -- true or false). Note that some invalid chains cannot even be constructed
    -- (for example, when we try to overspend).
  | ExpectedInvalid (Either Cardano.VerifyBlocksException IntException)

    -- | Disagreement between the DSL and Cardano
    --
    -- This indicates a bug. Of course, the bug could be in any number of
    -- places:
    --
    -- * Our translatiom from the DSL to Cardano is wrong
    -- * There is a bug in the DSL definitions
    -- * There is a bug in the Cardano implementation
    --
    -- We record the error message from Cardano, if Cardano thought the chain
    -- was invalid
  | Disagreement Disagreement

-- | Disagreement between Cardano and the DSL
data Disagreement =
    -- | Cardano reported the chain as invalid, but the DSL reported it as
    -- valid. We record the error message from Cardano.
    UnexpectedInvalid Cardano.VerifyBlocksException

    -- | Cardano reported an error during chain translation, but the DSL
    -- reported it as valid.
  | UnexpectedError IntException

    -- | Cardano reported the chain as valid, but the DSL reported it as
    -- invalid.
  | UnexpectedValid

    -- | Both Cardano and the DSL reported the chain as valid, but they computed
    -- a different UTxO
  | UnexpectedUtxo {
        utxoDsl     :: Utxo Addr
      , utxoCardano :: Cardano.Utxo
      , utxoInt     :: Cardano.Utxo
      }

expectValid :: ValidationResult -> Bool
expectValid ExpectedValid = True
expectValid _otherwise    = False

expectInvalid :: ValidationResult -> Bool
expectInvalid (ExpectedInvalid _) = True
expectInvalid _otherwise          = False

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Show ValidationResult where
  show = toString . pretty

instance Show Disagreement where
  show = toString . pretty

instance Buildable ValidationResult where
  build ExpectedValid               = "ExpectedValid"
  build (ExpectedInvalid (Left e))  = bprint ("ExpectedInvalid " % build) e
  build (ExpectedInvalid (Right e)) = bprint ("ExpectedInvalid " % shown) e
  build (Disagreement d)            = bprint ("Disagreement " % build) d

instance Buildable Disagreement where
  build (UnexpectedInvalid e) = bprint ("UnexpectedInvalid " % build) e
  build (UnexpectedError e)   = bprint ("UnexpectedError " % shown) e
  build UnexpectedValid       = "UnexpectedValid"
  build UnexpectedUtxo{..}    = bprint
      ( "UnexpectedUtxo"
      % "{ dsl:     " % build
      % ", cardano: " % mapJson
      % ", int:     " % mapJson
      % "}"
      )
      utxoDsl
      utxoCardano
      utxoInt
