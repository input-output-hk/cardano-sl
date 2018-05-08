{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

-- | Wallet unit tests
--
-- TODO: Take advantage of https://github.com/input-output-hk/cardano-sl/pull/2296 ?
module Main (main) where

import           Universum

import qualified Data.Set as Set
import qualified Data.Text.Buildable
import           Formatting (bprint, build, sformat, shown, (%))
import           Serokell.Util (mapJson)
import           Test.Hspec.QuickCheck
import           Test.QuickCheck (Property, arbitrary, frequency, listOf, shuffle, suchThat)

import qualified Pos.Block.Error as Cardano
import           Pos.Core (HasConfiguration)
import qualified Pos.Txp.Toil as Cardano
import           Pos.Util.Chrono

import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.Actions as Actions
import qualified Cardano.Wallet.Kernel.Diffusion as Kernel

import           UTxO.Bootstrap
import           UTxO.Context
import           UTxO.Crypto
import           UTxO.DSL
import           UTxO.Generator
import           UTxO.Interpreter
import           UTxO.Translate

import           Util.Buildable.Hspec
import           Util.Buildable.QuickCheck
import           Util.Validated
import           Wallet.Abstract
import           Wallet.Inductive
import           Wallet.Inductive.Cardano
import           Wallet.Inductive.Generator
import           Wallet.Inductive.Invariants
import           Wallet.Inductive.Validation

import qualified Wallet.Basic as Base
import qualified Wallet.Incremental as Incr
import qualified Wallet.Prefiltered as Pref
import qualified Wallet.Rollback.Basic as Roll
import qualified Wallet.Rollback.Full as Full

{-------------------------------------------------------------------------------
  Main test driver
-------------------------------------------------------------------------------}

main :: IO ()
main = do
--    _showContext
    runTranslateNoErrors $ withConfig $
            return $ hspec tests

-- | Debugging: show the translation context
_showContext :: IO ()
_showContext = do
    putStrLn $ runTranslateNoErrors $ withConfig $
      sformat build <$> ask
    putStrLn $ runTranslateNoErrors $
      let bootstrapTransaction' :: TransCtxt -> Transaction GivenHash Addr
          bootstrapTransaction' = bootstrapTransaction
      in sformat build . bootstrapTransaction' <$> ask

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

tests :: HasConfiguration =>Spec
tests = describe "Wallet unit tests" $ do
    testTranslation
    testPureWallet
    testWalletKernel
    testWalletWorker

{-------------------------------------------------------------------------------
  Generator model

  The generators are polymorphic in the types of addresses we have, and need
  various parameters. Here we introduce a simple model from which we can
  derive all of these arguments. We can then instantiate this model to

  * Cardano itself (given the bootstrap transaction).
    This is a model that results in something that we can translate to Cardano,
    but since it deals with the " real world " it has all kinds of different
    actors, large values, etc., and so is a bit difficult to debug when
    looking at values manually.

  * Simplified model, with small values, simple addresses, and no fees
-------------------------------------------------------------------------------}

data GeneratorModel h a = GeneratorModel {
      -- | Bootstrap transaction
      gmBoot          :: Transaction h a

      -- | Addresses to work with
      --
      -- These will be the addresses we can transfers funds from and to
    , gmAllAddresses  :: [a]

     -- | Which subset of 'gmAllAddresses' can we choose from for @ours@?
    , gmPotentialOurs :: a -> Bool

      -- | Maximum number of addresses to use for @ours@
    , gmMaxNumOurs    :: Int

      -- | Estimate fees
    , gmEstimateFee   :: Int -> Int -> Value
    }

genChainUsingModel :: (Hash h a, Ord a) => GeneratorModel h a -> Gen (Chain h a)
genChainUsingModel GeneratorModel{..} =
    evalStateT (genChain params) initState
  where
    params    = defChainParams gmEstimateFee gmAllAddresses
    initUtxo  = utxoRestrictToAddr (`elem` gmAllAddresses) $ trUtxo gmBoot
    initState = initTrState initUtxo 1

genInductiveUsingModel :: (Hash h a, Ord a)
                       => GeneratorModel h a -> Gen (Inductive h a)
genInductiveUsingModel GeneratorModel{..} = do
    numOurs <- choose (1, min (length potentialOurs) gmMaxNumOurs)
    addrs'  <- shuffle potentialOurs
    let ours = Set.fromList (take numOurs addrs')
    events  <- evalStateT (genWalletEvents (params ours)) initState
    return Inductive {
        inductiveBoot   = gmBoot
      , inductiveOurs   = ours
      , inductiveEvents = events
      }
  where
    potentialOurs = filter gmPotentialOurs gmAllAddresses
    params ours   = defEventsParams gmEstimateFee gmAllAddresses ours initUtxo
    initUtxo      = utxoRestrictToAddr (`elem` gmAllAddresses) $ trUtxo gmBoot
    initState     = initEventsGlobalState 1

cardanoModel :: Transaction GivenHash Addr -> GeneratorModel GivenHash Addr
cardanoModel boot = GeneratorModel {
      gmBoot          = boot
    , gmAllAddresses  = filter (not . isAvvmAddr) $ addrsInBoot boot
    , gmPotentialOurs = \_ -> True
    , gmEstimateFee   = estimateCardanoFee
    , gmMaxNumOurs    = 5
    }

addrsInBoot :: Transaction GivenHash a -> [a]
addrsInBoot = map outAddr . trOuts

estimateCardanoFee :: Int -> Int -> Value
estimateCardanoFee ins outs = fromIntegral $ (ins + outs) * 150000

simpleModel :: GeneratorModel GivenHash Char
simpleModel = GeneratorModel {
      gmAllAddresses  = addrs
    , gmPotentialOurs = \_ -> True
    , gmEstimateFee   = \_ _ -> 0
    , gmMaxNumOurs    = 3
    , gmBoot          = Transaction {
                            trFresh = fromIntegral (length addrs) * initBal
                          , trIns   = Set.empty
                          , trOuts  = [Output a initBal | a <- addrs]
                          , trFee   = 0
                          , trHash  = 0
                          , trExtra = ["Simple bootstrap"]
                          }
    }
  where
    addrs :: [Char]
    addrs = ['a' .. 'g']

    initBal :: Value
    initBal = 10000

{-------------------------------------------------------------------------------
  UTxO->Cardano translation tests
-------------------------------------------------------------------------------}

testTranslation :: Spec
testTranslation = do
    describe "Translation sanity checks" $ do
      it "can construct and verify empty block" $
        intAndVerifyPure emptyBlock `shouldSatisfy` expectValid

      it "can construct and verify block with one transaction" $
        intAndVerifyPure oneTrans `shouldSatisfy` expectValid

      it "can construct and verify example 1 from the UTxO paper" $
        intAndVerifyPure example1 `shouldSatisfy` expectValid

      it "can reject overspending" $
        intAndVerifyPure overspend `shouldSatisfy` expectInvalid

      it "can reject double spending" $
        intAndVerifyPure doublespend `shouldSatisfy` expectInvalid

    describe "Translation QuickCheck tests" $ do
      prop "can translate randomly generated chains" $
        forAll
          (intAndVerifyGen (genChainUsingModel . cardanoModel))
          expectValid

{-------------------------------------------------------------------------------
  Pure wallet tests
-------------------------------------------------------------------------------}

testPureWalletWith :: forall h a. (Hash h a, Ord a, Buildable a)
                   => Inductive h a -> Property
testPureWalletWith indWithRoll = conjoin [
      -- sanity check on the test
      uptoFirstRollback indDontRoll `shouldBe` indDontRoll
    , shouldBeValidated (void (inductiveIsValid indWithRoll))

      -- check that the invariants hold in each model
    , checkInvariants NoRollback    "base" indDontRoll baseEmpty
    , checkInvariants NoRollback    "incr" indDontRoll incrEmpty
    , checkInvariants NoRollback    "pref" indDontRoll prefEmpty
    , checkInvariants BasicRollback "roll" indWithRoll rollEmpty
    , checkInvariants FullRollback  "full" indWithRoll fullEmpty

      -- check equivalence between the models (no rollbacks)
    , checkEquivalent "base/incr" indDontRoll baseEmpty incrEmpty
    , checkEquivalent "base/pref" indDontRoll baseEmpty prefEmpty
    , checkEquivalent "base/roll" indDontRoll baseEmpty rollEmpty
    , checkEquivalent "base/full" indDontRoll baseEmpty fullEmpty

      -- check equivalence between models (with rollbacks)
    , checkEquivalent "roll/full" indWithRoll rollEmpty fullEmpty
    ]
  where
    -- Prefix of the 'Inductive' without any rollbacks
    indDontRoll :: Inductive h a
    indDontRoll = uptoFirstRollback indWithRoll

    checkInvariants :: ApplicableInvariants
                    -> Text
                    -> Inductive h a
                    -> (Set a -> Transaction h a -> Wallet h a)
                    -> Expectation
    checkInvariants applicableInvariants label ind@Inductive{..} w =
        shouldBeValidated $
          walletInvariants applicableInvariants label (w inductiveOurs) ind

    checkEquivalent :: Text
                    -> Inductive h a
                    -> (Set a -> Transaction h a -> Wallet h a)
                    -> (Set a -> Transaction h a -> Wallet h a)
                    -> Expectation
    checkEquivalent label ind@Inductive{..} w w' =
        shouldBeValidated $
          walletEquivalent label (w inductiveOurs) (w' inductiveOurs) ind

    oursFromSet :: Set a -> Ours a
    oursFromSet = flip Set.member

    baseEmpty :: Set a -> Transaction h a -> Wallet h a
    incrEmpty :: Set a -> Transaction h a -> Wallet h a
    prefEmpty :: Set a -> Transaction h a -> Wallet h a
    rollEmpty :: Set a -> Transaction h a -> Wallet h a
    fullEmpty :: Set a -> Transaction h a -> Wallet h a

    baseEmpty = walletBoot Base.walletEmpty . oursFromSet
    incrEmpty = walletBoot Incr.walletEmpty . oursFromSet
    prefEmpty = walletBoot Pref.walletEmpty . oursFromSet
    rollEmpty = walletBoot Roll.walletEmpty . oursFromSet
    fullEmpty = walletBoot Full.walletEmpty . oursFromSet

testPureWallet :: Spec
testPureWallet = do
    describe "Test pure wallets" $ do
      it "Using simple model" $
        forAll (genInductiveUsingModel simpleModel) $ testPureWalletWith
      it "Using Cardano model" $
        forAll (genInductiveUsingModel (cardanoModel boot)) $ testPureWalletWith
  where
    transCtxt = runTranslateNoErrors ask
    boot      = bootstrapTransaction transCtxt

{-------------------------------------------------------------------------------
  Compare the wallet kernel with the pure model
-------------------------------------------------------------------------------}

testWalletKernel :: HasConfiguration => Spec
testWalletKernel =
    it "Compare wallet kernel to pure model" $
      forAll (genInductiveUsingModel model) $ \ind -> do
        -- TODO: remove once we have support for rollback in the kernel
        let indDontRoll = uptoFirstRollback ind
        bracketActiveWallet $ \activeWallet -> do
          checkEquivalent activeWallet indDontRoll
  where
    transCtxt = runTranslateNoErrors ask
    boot      = bootstrapTransaction transCtxt
    model     = (cardanoModel boot) {
                    gmMaxNumOurs    = 1
                  , gmPotentialOurs = isPoorAddr
                  }

    checkEquivalent :: forall h. Hash h Addr
                    => Kernel.ActiveWallet
                    -> Inductive h Addr
                    -> Expectation
    checkEquivalent activeWallet ind = do
       shouldReturnValidated $ runTranslateT $ do
         equivalentT activeWallet (encKpEnc ekp) (mkWallet (== addr)) ind
      where
        [addr]       = Set.toList $ inductiveOurs ind
        AddrInfo{..} = resolveAddr addr transCtxt
        Just ekp     = addrInfoMasterKey

    -- TODO: We should move to the full model instead of the base model
    mkWallet :: Hash h Addr => Ours Addr -> Transaction h Addr -> Wallet h Addr
    mkWallet = walletBoot Base.walletEmpty

{-------------------------------------------------------------------------------
  Wallet worker state machine tests
-------------------------------------------------------------------------------}

testWalletWorker :: Spec
testWalletWorker = do
    describe "Test wallet worker state machine" $ do

      it "Starts in a valid initial state with no effect on the wallet" $ do
          let StackResult{..} = runStackWorker [] $ Stack [1..10]
          srState `shouldSatisfy` Actions.isValidState
          srState `shouldSatisfy` Actions.isInitialState
          srStack `shouldBe` Stack [1..10]

      it "State invariants are not violated" $ forAll (listOf someAction) $
          \actions -> Actions.isValidState (srState $ runStackWorker actions $ Stack [1..5])

      it "Applies blocks immediately from its initial state" $ do
          let actions = [ Actions.ApplyBlocks (OldestFirst $ 1:|[2,3]) ]
              StackResult{..} = runStackWorker actions $ Stack []
          srStack `shouldBe` Stack [3,2,1]

      it "Applies blocks in the correct order" $ do
          let actions = [ Actions.ApplyBlocks $ OldestFirst $ 1:|[2,3]
                        , Actions.ApplyBlocks $ OldestFirst $ 4:|[5,6] ]
              StackResult{..} = runStackWorker actions $ Stack []
          srStack `shouldBe` Stack [6,5,4,3,2,1]

      it "Can switch to a new fork" $ do
          let actions = [ Actions.ApplyBlocks    $ OldestFirst $ 1:|[2,3]
                        , Actions.RollbackBlocks $ NewestFirst $ 3:|[2]
                        , Actions.ApplyBlocks    $ OldestFirst $ 4:|[5,6] ]
              StackResult{..} = runStackWorker actions $ Stack []
          srState `shouldSatisfy` (not . Actions.hasPendingFork)
          srStack `shouldBe` Stack [6,5,4,1]

      it "Can switch to a new fork by combining actions" $ do
          let actions = [ Actions.ApplyBlocks    $ OldestFirst $ 1:|[2,3]
                        , Actions.RollbackBlocks $ NewestFirst $ 3:|[2]
                        , Actions.ApplyBlocks    $ OldestFirst $ 4:|[]
                      , Actions.ApplyBlocks    $ OldestFirst $ 5:|[6] ]
              StackResult{..} = runStackWorker actions $ Stack []
          srState `shouldSatisfy` (not . Actions.hasPendingFork)
          srStack `shouldBe` Stack [6,5,4,1]

      it "Behaves like the simple stack model, when there is no pending fork" $ do
          let stk0 = Stack [1..100]
              run = (`runStackWorker` stk0)
              doesNotResultInFork = not . Actions.hasPendingFork . srState . run
          forAll (listOf someAction `suchThat` doesNotResultInFork) $
              \actions -> do
                  let StackResult{..} = run actions
                      expectedStack  = execState (mapM actionToStackOp actions) stk0
                  srStack `shouldBe` expectedStack

  where
    runStackWorker :: [Actions.WalletAction Int] -> Stack -> StackResult
    runStackWorker actions stk0 =
        let (s, stk) = runState (Actions.interpList stackOps actions) stk0
        in StackResult { srState = s, srStack = stk }

    -- Bias the actions slightly towards increasing the blockchain size
    someAction :: Gen (Actions.WalletAction Int)
    someAction = frequency [ (10, (Actions.ApplyBlocks . OldestFirst)    <$> arbitrary)
                           , (7,  (Actions.RollbackBlocks . NewestFirst) <$> arbitrary)
                           , (1,   Actions.LogMessage                    <$> arbitrary)
                           ]

data StackResult = StackResult
    { srState :: Actions.WalletWorkerState Int
    , srStack :: Stack
    }

stackOps :: Actions.WalletActionInterp (State Stack) Int
stackOps = Actions.WalletActionInterp
    { Actions.applyBlocks  = mapM_ push
    , Actions.switchToFork = \n bs -> do
          replicateM_ n pop
          mapM_ push bs
    , Actions.emit         = const (return ())
    }
  where
    push = interpStackOp . Push
    pop  = interpStackOp Pop

data StackOp = Push Int | Pop
newtype Stack = Stack [Int]
    deriving (Eq, Show)

instance Buildable Stack where
    build (Stack stk) = bprint ("Stack " % shown) stk

interpStackOp :: StackOp -> State Stack ()
interpStackOp op = modify $ \stk ->
    case (op, stk) of
        (Push x, Stack xs)     -> Stack (x:xs)
        (Pop,    Stack (_:xs)) -> Stack xs
        (Pop,    Stack [])     -> Stack []

actionToStackOp :: Actions.WalletAction Int -> State Stack ()
actionToStackOp = \case
    Actions.ApplyBlocks    bs -> mapM_ push bs
    Actions.RollbackBlocks bs -> mapM_ (const pop) bs
    Actions.LogMessage _      -> return ()
  where
    push = interpStackOp . Push
    pop  = interpStackOp Pop

{-------------------------------------------------------------------------------
  Wallet resource management
-------------------------------------------------------------------------------}

-- | Initialize passive wallet in a manner suitable for the unit tests
bracketPassiveWallet :: (Kernel.PassiveWallet -> IO a) -> IO a
bracketPassiveWallet = Kernel.bracketPassiveWallet logMessage
  where
   -- TODO: Decide what to do with logging
    logMessage _sev txt = print txt

-- | Initialize active wallet in a manner suitable for generator-based testing
bracketActiveWallet :: (Kernel.ActiveWallet -> IO a) -> IO a
bracketActiveWallet test =
    bracketPassiveWallet $ \passive ->
      Kernel.bracketActiveWallet passive diffusion $ \active ->
        test active

-- TODO: Decide what we want to do with submitted transactions
diffusion :: Kernel.WalletDiffusion
diffusion =  Kernel.WalletDiffusion {
    walletSendTx = \_tx -> return False
  }

{-------------------------------------------------------------------------------
  Example hand-constructed chains
-------------------------------------------------------------------------------}

emptyBlock :: Hash h Addr => Transaction h Addr -> Chain h a
emptyBlock _boot = OldestFirst [OldestFirst []]

oneTrans :: Hash h Addr => Transaction h Addr -> Chain h Addr
oneTrans boot = OldestFirst [OldestFirst [t1]]
  where
    fee1 = estimateCardanoFee 1 2
    t1   = Transaction {
               trFresh = 0
             , trFee   = fee1
             , trHash  = 1
             , trIns   = Set.fromList [ Input (hash boot) 0 ] -- rich 0
             , trOuts  = [ Output r1 1000
                         , Output r0 (initR0 - 1000 - fee1)
                         ]
             , trExtra = ["t1"]
             }

-- | Try to transfer from R0 to R1, but leaving R0's balance the same
overspend :: Hash h Addr => Transaction h Addr -> Chain h Addr
overspend boot = OldestFirst [OldestFirst [t1]]
  where
    fee1 = estimateCardanoFee 1 2
    t1   = Transaction {
               trFresh = 0
             , trFee   = fee1
             , trHash  = 1
             , trIns   = Set.fromList [ Input (hash boot) 0 ] -- rich 0
             , trOuts  = [ Output r1 1000
                         , Output r0 initR0
                         ]
             , trExtra = ["t1"]
             }

-- | Try to transfer to R1 and R2 using the same output
doublespend :: Hash h Addr => Transaction h Addr -> Chain h Addr
doublespend boot = OldestFirst [OldestFirst [t1, t2]]
  where
    fee1 = estimateCardanoFee 1 2
    t1   = Transaction {
               trFresh = 0
             , trFee   = fee1
             , trHash  = 1
             , trIns   = Set.fromList [ Input (hash boot) 0 ] -- rich 0
             , trOuts  = [ Output r1 1000
                         , Output r0 (initR0 - 1000 - fee1)
                         ]
             , trExtra = ["t1"]
             }

    fee2 = estimateCardanoFee 1 2
    t2   = Transaction {
               trFresh = 0
             , trFee   = fee2
             , trHash  = 2
             , trIns   = Set.fromList [ Input (hash boot) 0 ] -- rich 0
             , trOuts  = [ Output r2 1000
                         , Output r0 (initR0 - 1000 - fee2)
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
example1 :: Hash h Addr => Transaction h Addr -> Chain h Addr
example1 boot = OldestFirst [OldestFirst [t3, t4]]
  where
    fee3 = estimateCardanoFee 1 2
    t3   = Transaction {
               trFresh = 0
             , trFee   = fee3
             , trHash  = 3
             , trIns   = Set.fromList [ Input (hash boot) 0 ] -- rich 0
             , trOuts  = [ Output r1 1000
                         , Output r0 (initR0 - 1000 - fee3)
                         ]
             , trExtra = ["t3"]
             }

    fee4 = estimateCardanoFee 1 1
    t4   = Transaction {
               trFresh = 0
             , trFee   = fee4
             , trHash  = 4
             , trIns   = Set.fromList [ Input (hash t3) 1 ]
             , trOuts  = [ Output r2 (initR0 - 1000 - fee3 - fee4) ]
             , trExtra = ["t4"]
             }

{-------------------------------------------------------------------------------
  Some initial values

  TODO: These come from the genesis block. We shouldn't hardcode them
  in the tests but rather derive them from the bootstrap transaction.
-------------------------------------------------------------------------------}

initR0 :: Value
initR0 = 11137499999752500

r0, r1, r2 :: Addr
r0 = Addr (IxRich 0) 0
r1 = Addr (IxRich 1) 0
r2 = Addr (IxRich 2) 0

{-------------------------------------------------------------------------------
  Verify chain
-------------------------------------------------------------------------------}

intAndVerifyPure :: (Transaction GivenHash Addr -> Chain GivenHash Addr)
                 -> ValidationResult GivenHash Addr
intAndVerifyPure pc = runIdentity $ intAndVerify (Identity . pc)

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
                    $ map Right chain'
        isCardanoValid <- verifyBlocksPrefix chain''
        case (dslIsValid, isCardanoValid) of
          (Invalid _ e' , Invalid _ e) -> return $ ExpectedInvalid e' e
          (Invalid _ e' , Valid     _) -> return $ Disagreement ledger (UnexpectedValid e')
          (Valid     () , Invalid _ e) -> return $ Disagreement ledger (UnexpectedInvalid e)
          (Valid     () , Valid (_undo, finalUtxo)) -> do
            (finalUtxo', _) <- runIntT' ctxt $ int dslUtxo
            if finalUtxo == finalUtxo'
              then return $ ExpectedValid
              else return $ Disagreement ledger UnexpectedUtxo {
                         utxoDsl     = dslUtxo
                       , utxoCardano = finalUtxo
                       , utxoInt     = finalUtxo'
                       }

{-------------------------------------------------------------------------------
  Chain verification test result
-------------------------------------------------------------------------------}

data ValidationResult h a =
    -- | We expected the chain to be valid; DSL and Cardano both agree
    ExpectedValid

    -- | We expected the chain to be invalid; DSL and Cardano both agree
  | ExpectedInvalid {
        validationErrorDsl     :: Text
      , validationErrorCardano :: Cardano.VerifyBlocksException
      }

    -- | Variation on 'ExpectedInvalid', where we cannot even /construct/
    -- the Cardano chain, much less validate it.
  | ExpectedInvalid' {
        validationErrorDsl :: Text
      , validationErrorInt :: IntException
      }

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
  | Disagreement {
        validationLedger       :: Ledger h a
      , validationDisagreement :: Disagreement h a
      }

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
  | UnexpectedUtxo {
        utxoDsl     :: Utxo h a
      , utxoCardano :: Cardano.Utxo
      , utxoInt     :: Cardano.Utxo
      }

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
  build ExpectedInvalid{..} = bprint
      ( "ExpectedInvalid"
      % ", errorDsl:     " % build
      % ", errorCardano: " % build
      % "}"
      )
      validationErrorDsl
      validationErrorCardano
  build ExpectedInvalid'{..} = bprint
      ( "ExpectedInvalid'"
      % ", errorDsl: " % build
      % ", errorInt: " % build
      % "}"
      )
      validationErrorDsl
      validationErrorInt
  build Disagreement{..} = bprint
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
