module Test.Spec.Translation (
    spec
  ) where

import           Universum

import qualified Data.Set as Set
import           Formatting (bprint, build, shown, (%))
import qualified Formatting.Buildable
import           Pos.Core.Chrono
import           Serokell.Util (mapJson)
import           Test.Hspec.QuickCheck

import qualified Pos.Chain.Block as Cardano
import qualified Pos.Chain.Txp as Cardano
import           Pos.Core (Coeff (..), TxSizeLinear (..))

import           Test.Infrastructure.Generator
import           Test.Infrastructure.Genesis
import           Util.Buildable.Hspec
import           Util.Buildable.QuickCheck
import           Util.Validated
import           UTxO.Bootstrap
import           UTxO.Context
import           UTxO.DSL
import           UTxO.Interpreter
import           UTxO.Translate

{-------------------------------------------------------------------------------
  UTxO->Cardano translation tests
-------------------------------------------------------------------------------}

spec :: Spec
spec = do
    describe "Translation sanity checks" $ do
      it "can construct and verify empty block" $
        intAndVerifyPure linearFeePolicy emptyBlock `shouldSatisfy` expectValid

      it "can construct and verify block with one transaction" $
        intAndVerifyPure linearFeePolicy oneTrans `shouldSatisfy` expectValid

      it "can construct and verify example 1 from the UTxO paper" $
        intAndVerifyPure linearFeePolicy example1 `shouldSatisfy` expectValid

      it "can reject overspending" $
        intAndVerifyPure linearFeePolicy overspend `shouldSatisfy` expectInvalid

      it "can reject double spending" $
        intAndVerifyPure linearFeePolicy doublespend `shouldSatisfy` expectInvalid

      -- There are subtle points near the epoch boundary, so we test from a
      -- few blocks less to a few blocks more than the length of an epoch
      prop "can construct and verify chain that spans epochs" $
        let epochSlots = runTranslateNoErrors $ asks (ccEpochSlots . tcCardano)
        in forAll (choose (  1,  3) :: Gen Int) $ \numEpochs ->
           forAll (choose (-10, 10) :: Gen Int) $ \extraSlots ->
             let numSlots = numEpochs * fromIntegral epochSlots + extraSlots in
             shouldSatisfy
               (intAndVerifyPure linearFeePolicy (spanEpochs numSlots))
               expectValid

    describe "Translation QuickCheck tests" $ do
      prop "can translate randomly generated chains" $
        forAll
          (intAndVerifyGen (genChainUsingModel . cardanoModel linearFeePolicy))
          expectValid

  where
    linearFeePolicy = TxSizeLinear (Coeff 155381) (Coeff 43.946)

{-------------------------------------------------------------------------------
  Example hand-constructed chains
-------------------------------------------------------------------------------}

emptyBlock :: GenesisValues h a -> Chain h a
emptyBlock _ = OldestFirst [OldestFirst []]

oneTrans :: Hash h Addr => GenesisValues h Addr -> Chain h Addr
oneTrans GenesisValues{..} = OldestFirst [OldestFirst [t1]]
  where
    fee1 = overestimate txFee 1 2
    t1   = Transaction {
               trFresh = 0
             , trFee   = fee1
             , trHash  = 1
             , trIns   = Set.fromList [ fst initUtxoR0 ]
             , trOuts  = [ Output r1 1000
                         , Output r0 (initBalR0 - 1000 - fee1)
                         ]
             , trExtra = ["t1"]
             }

-- | Try to transfer from R0 to R1, but leaving R0's balance the same
overspend :: Hash h Addr => GenesisValues h Addr -> Chain h Addr
overspend GenesisValues{..} = OldestFirst [OldestFirst [t1]]
  where
    fee1 = overestimate txFee 1 2
    t1   = Transaction {
               trFresh = 0
             , trFee   = fee1
             , trHash  = 1
             , trIns   = Set.fromList [ fst initUtxoR0 ]
             , trOuts  = [ Output r1 1000
                         , Output r0 initBalR0
                         ]
             , trExtra = ["t1"]
             }

-- | Try to transfer to R1 and R2 using the same output
doublespend :: Hash h Addr => GenesisValues h Addr -> Chain h Addr
doublespend GenesisValues{..} = OldestFirst [OldestFirst [t1, t2]]
  where
    fee1 = overestimate txFee 1 2
    t1   = Transaction {
               trFresh = 0
             , trFee   = fee1
             , trHash  = 1
             , trIns   = Set.fromList [ fst initUtxoR0 ]
             , trOuts  = [ Output r1 1000
                         , Output r0 (initBalR0 - 1000 - fee1)
                         ]
             , trExtra = ["t1"]
             }

    fee2 = overestimate txFee 1 2
    t2   = Transaction {
               trFresh = 0
             , trFee   = fee2
             , trHash  = 2
             , trIns   = Set.fromList [ fst initUtxoR0 ]
             , trOuts  = [ Output r2 1000
                         , Output r0 (initBalR0 - 1000 - fee2)
                         ]
             , trExtra = ["t2"]
             }

-- | Translation of example 1 of the paper, adjusted to allow for fees
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
example1 :: Hash h Addr => GenesisValues h Addr -> Chain h Addr
example1 GenesisValues{..} = OldestFirst [OldestFirst [t3, t4]]
  where
    fee3 = overestimate txFee 1 2
    t3   = Transaction {
               trFresh = 0
             , trFee   = fee3
             , trHash  = 3
             , trIns   = Set.fromList [ fst initUtxoR0 ]
             , trOuts  = [ Output r1 1000
                         , Output r0 (initBalR0 - 1000 - fee3)
                         ]
             , trExtra = ["t3"]
             }

    fee4 = overestimate txFee 1 1
    t4   = Transaction {
               trFresh = 0
             , trFee   = fee4
             , trHash  = 4
             , trIns   = Set.fromList [ Input (hash t3) 1 ]
             , trOuts  = [ Output r2 (initBalR0 - 1000 - fee3 - fee4) ]
             , trExtra = ["t4"]
             }


-- | Chain that spans epochs
spanEpochs :: forall h. Hash h Addr
           => Int -> GenesisValues h Addr -> Chain h Addr
spanEpochs numSlots GenesisValues{..} = OldestFirst $
    go 1
       (fst initUtxoR0)
       (fst initUtxoR1)
       initBalR0
       initBalR1
       numSlots
  where
    go :: Int           -- Next available hash
       -> Input h Addr  -- UTxO entry with r0's balance
       -> Input h Addr  -- UTxO entry with r1's balance
       -> Value         -- r0's current total balance
       -> Value         -- r1's current total balance
       -> Int           -- Number of cycles to go
       -> [Block h Addr]
    go _ _ _ _ _ 1 = []
    go freshHash r0utxo r1utxo r0balance r1balance n =
        let tPing = ping freshHash       r0utxo r0balance
            tPong = pong (freshHash + 1) r1utxo r1balance
        in OldestFirst [tPing, tPong]
         : go (freshHash + 2)
              (Input (hash tPing) 1)
              (Input (hash tPong) 1)
              (r0balance - 10 - fee)
              (r1balance - 10 - fee)
              (n - 1)

    -- Rich 0 transferring a small amount to rich 1
    ping :: Int -> Input h Addr -> Value -> Transaction h Addr
    ping freshHash r0utxo r0balance = Transaction {
          trFresh = 0
        , trFee   = fee
        , trHash  = freshHash
        , trIns   = Set.fromList [ r0utxo ]
        , trOuts  = [ Output r1 10
                    , Output r0 (r0balance - 10 - fee)
                    ]
        , trExtra = ["ping"]
        }

    -- Rich 1 transferring a small amount to rich 0
    pong :: Int -> Input h Addr -> Value -> Transaction h Addr
    pong freshHash r1utxo r1balance = Transaction {
          trFresh = 0
        , trFee   = fee
        , trHash  = freshHash
        , trIns   = Set.fromList [ r1utxo ]
        , trOuts  = [ Output r0 10
                    , Output r1 (r1balance - 10 - fee)
                    ]
        , trExtra = ["pong"]
        }

    fee :: Value
    fee = overestimate txFee 1 2


{-------------------------------------------------------------------------------
  Verify chain
-------------------------------------------------------------------------------}

intAndVerifyPure :: TxSizeLinear
                 -> (GenesisValues GivenHash Addr -> Chain GivenHash Addr)
                 -> ValidationResult GivenHash Addr
intAndVerifyPure txSizeLinear pc = runIdentity $ intAndVerify (Identity . pc . genesisValues txSizeLinear)

-- | Specialization of 'intAndVerify' to 'Gen'
intAndVerifyGen :: (Transaction GivenHash Addr -> Gen (Chain GivenHash Addr))
                -> Gen (ValidationResult GivenHash Addr)
intAndVerifyGen = intAndVerify

-- | Specialization of 'intAndVerifyChain' to 'GivenHash'
intAndVerify :: Monad m
             => (Transaction GivenHash Addr -> m (Chain GivenHash Addr))
             -> m (ValidationResult GivenHash Addr)
intAndVerify = intAndVerifyChain

-- | Interpret and verify a chain.
intAndVerifyChain :: (Hash h Addr, Monad m)
                  => (Transaction h Addr -> m (Chain h Addr))
                  -> m (ValidationResult h Addr)
intAndVerifyChain pc = runTranslateT $ do
    boot  <- asks bootstrapTransaction
    chain <- lift $ pc boot
    let ledger      = chainToLedger boot chain
        dslIsValid  = ledgerIsValid ledger
        dslUtxo     = ledgerUtxo    ledger
    intResult <- catchTranslateErrors $ runIntBoot' boot $ int chain
    case intResult of
      Left e ->
        case dslIsValid of
          Valid     () -> return $ Disagreement ledger (UnexpectedError e)
          Invalid _ e' -> return $ ExpectedInvalid' e' e
      Right (chain', ctxt) -> do
        let chain'' = fromMaybe (error "intAndVerify: Nothing")
                    $ nonEmptyOldestFirst
                    $ chain'
        isCardanoValid <- verifyBlocksPrefix chain''
        case (dslIsValid, isCardanoValid) of
          (Invalid _ e' , Invalid _ e) -> return $ ExpectedInvalid e' e
          (Invalid _ e' , Valid     _) -> return $ Disagreement ledger (UnexpectedValid e')
          (Valid     () , Invalid _ e) -> return $ Disagreement ledger (UnexpectedInvalid e)
          (Valid     () , Valid (_undo, finalUtxo)) -> do
            (finalUtxo', _) <- runIntT' ctxt $ int dslUtxo
            if finalUtxo == finalUtxo'
              then return $ ExpectedValid
              else return . Disagreement ledger
                  $ UnexpectedUtxo dslUtxo finalUtxo finalUtxo'

{-------------------------------------------------------------------------------
  Chain verification test result
-------------------------------------------------------------------------------}

data ValidationResult h a =
    -- | We expected the chain to be valid; DSL and Cardano both agree
    ExpectedValid

    -- | We expected the chain to be invalid; DSL and Cardano both agree
    -- ExpectedInvalid
    --     validationErrorDsl
    --     validationErrorCardano
  | ExpectedInvalid !Text !Cardano.VerifyBlocksException

    -- | Variation on 'ExpectedInvalid', where we cannot even /construct/
    -- the Cardano chain, much less validate it.
    -- ExpectedInvalid
    --     validationErrorDsl
    --     validationErrorInt
  | ExpectedInvalid'  !Text !IntException

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
    -- was invalid, as well as the ledger that causes the problem.
    -- Disagreement
    --     validationLedger
    --     validationDisagreement
  | Disagreement !(Ledger h a) !(Disagreement h a)

-- | Disagreement between Cardano and the DSL
--
-- We consider something to be "unexpectedly foo" when Cardano says it's
-- " foo " but the DSL says it's " not foo "; the DSL is the spec, after all
-- (of course that doesn't mean that it cannot contain bugs :).
data Disagreement h a =
    -- | Cardano reported the chain as invalid, but the DSL reported it as
    -- valid. We record the error message from Cardano.
    UnexpectedInvalid Cardano.VerifyBlocksException

    -- | Cardano reported an error during chain translation, but the DSL
    -- reported it as valid.
  | UnexpectedError IntException

    -- | Cardano reported the chain as valid, but the DSL reported it as
    -- invalid.
  | UnexpectedValid Text

    -- | Both Cardano and the DSL reported the chain as valid, but they computed
    -- a different UTxO
    -- UnexpectedUtxo utxoDsl utxoCardano utxoInt
  | UnexpectedUtxo !(Utxo h a) !Cardano.Utxo !Cardano.Utxo

expectValid :: ValidationResult h a -> Bool
expectValid ExpectedValid = True
expectValid _otherwise    = False

expectInvalid :: ValidationResult h a -> Bool
expectInvalid (ExpectedInvalid _ _) = True
expectInvalid _otherwise            = False

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance (Hash h a, Buildable a) => Buildable (ValidationResult h a) where
  build ExpectedValid = "ExpectedValid"
  build (ExpectedInvalid
             validationErrorDsl
             validationErrorCardano) = bprint
      ( "ExpectedInvalid"
      % ", errorDsl:     " % build
      % ", errorCardano: " % build
      % "}"
      )
      validationErrorDsl
      validationErrorCardano
  build (ExpectedInvalid'
             validationErrorDsl
             validationErrorInt) = bprint
      ( "ExpectedInvalid'"
      % ", errorDsl: " % build
      % ", errorInt: " % build
      % "}"
      )
      validationErrorDsl
      validationErrorInt
  build (Disagreement
             validationLedger
             validationDisagreement) = bprint
      ( "Disagreement "
      % "{ ledger: "       % build
      % ", disagreement: " % build
      % "}"
      )
      validationLedger
      validationDisagreement

instance (Hash h a, Buildable a) => Buildable (Disagreement h a) where
  build (UnexpectedInvalid e) = bprint ("UnexpectedInvalid " % build) e
  build (UnexpectedError e)   = bprint ("UnexpectedError " % shown) e
  build (UnexpectedValid e)   = bprint ("UnexpectedValid " % shown) e
  build (UnexpectedUtxo utxoDsl utxoCardano utxoInt) = bprint
      ( "UnexpectedUtxo"
      % "{ dsl:     " % build
      % ", cardano: " % mapJson
      % ", int:     " % mapJson
      % "}"
      )
      utxoDsl
      utxoCardano
      utxoInt
