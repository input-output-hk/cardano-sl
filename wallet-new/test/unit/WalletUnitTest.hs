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
import qualified Data.Text as Text
import qualified Data.Text.Buildable

import qualified Pos.Block.Error as Cardano
import qualified Pos.Txp.Toil    as Cardano
import Serokell.Util (mapJson)

import UTxO.Bootstrap
import UTxO.Context
import UTxO.DSL
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
    putStrLn $ runTranslateNoErrors $ do
      tc <- ask
      withConfig $ return (sformat build tc)
    putStrLn $ runTranslateNoErrors $ do
      tc <- ask
      return $ dumpStr (bootstrapTransactions tc)

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

tests :: Spec
tests = describe "Wallet unit tests" $ do
    testSanityChecks

testSanityChecks :: Spec
testSanityChecks = describe "Test sanity checks" $ do
    it "can construct and verify empty block" $
      intAndVerify (PreChain $ \_boot -> [[]])
        `shouldSatisfy` expectValid

    it "can construct and verify block with one transaction" $
      intAndVerify (PreChain $ \boot -> [[t1 boot]])
        `shouldSatisfy` expectValid

    it "can reject double spending" $
      intAndVerify (PreChain $ \boot -> [[t1 boot, t1 boot]])
        `shouldSatisfy` expectInvalid

    it "can reject overspending" $
      intAndVerify (PreChain $ \boot -> [[t2 boot]])
        `shouldSatisfy` expectInvalid
  where
    -- transfer 10 from rich 0 to rich 1
    t1 :: Transaction Addr -> Fee -> Transaction Addr
    t1 boot fee = Transaction {
          trIns  = [ Input boot 0 ] -- rich 0
        , trOuts = [ Output (AddrOrdinary (Addr (IxRich 1) 0)) 10
                   , Output (AddrOrdinary (Addr (IxRich 0) 0)) (initRich - 10 - fee)
                   , Output AddrTreasury                       fee
                   ]
        }

    -- transfer 10 from rich 0 to rich 1, try to eat the cake and have it too
    t2 :: Transaction Addr -> Fee -> Transaction Addr
    t2 boot fee = Transaction {
          trIns  = [ Input boot 0 ] -- rich 0
        , trOuts = [ Output (AddrOrdinary (Addr (IxRich 1) 0)) 10
                   , Output (AddrOrdinary (Addr (IxRich 0) 0)) (initRich - fee)
                   , Output AddrTreasury                       fee
                   ]
        }

    initRich = 11137499999752500

{-------------------------------------------------------------------------------
  Chain with some information still missing
-------------------------------------------------------------------------------}

newtype PreChain = PreChain (Transaction Addr -> [[Fee -> Transaction Addr]])

fromPreChain :: PreChain -> Translate IntException (Chain Addr, Ledger Addr)
fromPreChain (PreChain f) = do
    boot <- asks bootstrapTransactions
    txs' <- mapM (mapM calculateFee) $ f (last boot)
    let chain  = Chain txs' -- doesn't include the boot transactions
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
  show = Text.unpack . pretty

instance Show Disagreement where
  show = Text.unpack . pretty

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
