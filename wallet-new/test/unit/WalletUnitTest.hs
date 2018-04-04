{-# LANGUAGE TupleSections #-}

-- | Wallet unit tests
--
-- TODO: Take advantage of https://github.com/input-output-hk/cardano-sl/pull/2296 ?
module Main (main) where

import qualified Data.Set as Set
import qualified Data.Text.Buildable
import           Formatting (bprint, build, sformat, shown, (%))
import           Serokell.Util (mapJson)
import           Test.Hspec.QuickCheck
import           Universum

import qualified Pos.Block.Error as Cardano
import qualified Pos.Txp.Toil as Cardano
import           Pos.Util.Chrono

import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.Diffusion as Kernel

import           UTxO.BlockGen
import           UTxO.Bootstrap
import           UTxO.Context
import           UTxO.DSL
import           UTxO.Interpreter
import           UTxO.PreChain
import           UTxO.Translate

import           Util.Buildable.Hspec
import           Util.Buildable.QuickCheck
import           Util.Validated
import           Wallet.Abstract
import qualified Wallet.Basic          as Base
import qualified Wallet.Incremental    as Incr
import qualified Wallet.Prefiltered    as Pref
import qualified Wallet.Rollback.Basic as Roll
import qualified Wallet.Rollback.Full  as Full

{-------------------------------------------------------------------------------
  Main test driver
-------------------------------------------------------------------------------}

main :: IO ()
main = do
--    _showContext
    hspec tests

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

tests :: Spec
tests = describe "Wallet unit tests" $ do
    testTranslation
    testPureWallet
    testPassiveWallet
    testActiveWallet

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
          (intAndVerifyGen genValidBlockchain)
          expectValid

{-------------------------------------------------------------------------------
  Pure wallet tests
-------------------------------------------------------------------------------}

testPureWallet :: Spec
testPureWallet = do
    it "Test pure wallets" $
      forAll genInductive $ \ind -> conjoin [
          checkInvariants "base"      ind baseEmpty
        , checkInvariants "incr"      ind incrEmpty
        , checkInvariants "pref"      ind prefEmpty
        , checkInvariants "roll"      ind rollEmpty
        , checkInvariants "full"      ind fullEmpty
        , checkEquivalent "base/incr" ind baseEmpty incrEmpty
        , checkEquivalent "base/pref" ind baseEmpty prefEmpty
        , checkEquivalent "base/roll" ind baseEmpty rollEmpty
        , checkEquivalent "base/full" ind baseEmpty fullEmpty
        ]
  where
    transCtxt = runTranslateNoErrors ask

    genInductive :: Hash h Addr => Gen (InductiveWithOurs h Addr)
    genInductive = do
      fpc <- runTranslateT $ fromPreChain genValidBlockchain
      n <- choose
        ( 1
        , length . filter (not . isAvvmAddr) . toList
        . ledgerAddresses $ fpcLedger fpc
        )
      genFromBlockchainPickingAccounts n fpc

    checkInvariants :: (Hash h a, Eq a, Buildable a)
                    => Text
                    -> InductiveWithOurs h a
                    -> (Set a -> Transaction h a -> Wallet h a)
                    -> Expectation
    checkInvariants label (InductiveWithOurs addrs ind) w =
        shouldBeValidated $
          walletInvariants label (w addrs) ind

    checkEquivalent :: (Hash h a, Eq a, Buildable a)
                    => Text
                    -> InductiveWithOurs h a
                    -> (Set a -> Transaction h a -> Wallet h a)
                    -> (Set a -> Transaction h a -> Wallet h a)
                    -> Expectation
    checkEquivalent label (InductiveWithOurs addrs ind) w w' =
        shouldBeValidated $
          walletEquivalent label (w addrs) (w' addrs) ind

    oursFromSet :: Set Addr -> Ours Addr
    oursFromSet addrs addr = do
        guard (Set.member addr addrs)
        return $ fst (resolveAddr addr transCtxt)

    baseEmpty :: Set Addr -> Transaction GivenHash Addr -> Wallet GivenHash Addr
    incrEmpty :: Set Addr -> Transaction GivenHash Addr -> Wallet GivenHash Addr
    prefEmpty :: Set Addr -> Transaction GivenHash Addr -> Wallet GivenHash Addr
    rollEmpty :: Set Addr -> Transaction GivenHash Addr -> Wallet GivenHash Addr
    fullEmpty :: Set Addr -> Transaction GivenHash Addr -> Wallet GivenHash Addr

    baseEmpty = walletBoot Base.walletEmpty . oursFromSet
    incrEmpty = walletBoot Incr.walletEmpty . oursFromSet
    prefEmpty = walletBoot Pref.walletEmpty . oursFromSet
    rollEmpty = walletBoot Roll.walletEmpty . oursFromSet
    fullEmpty = walletBoot Full.walletEmpty . oursFromSet

{-------------------------------------------------------------------------------
  Passive wallet tests
-------------------------------------------------------------------------------}

testPassiveWallet  :: Spec
testPassiveWallet = around bracketPassiveWallet $
    describe "Passive wallet sanity checks" $ do
      it "can be initialized" $ \w ->
        Kernel.init w

-- | Initialize passive wallet in a manner suitable for the unit tests
bracketPassiveWallet :: (Kernel.PassiveWallet -> IO a) -> IO a
bracketPassiveWallet = Kernel.bracketPassiveWallet logMessage
  where
   -- TODO: Decide what to do with logging
    logMessage _sev _txt = return ()

{-------------------------------------------------------------------------------
  Active wallet tests
-------------------------------------------------------------------------------}

testActiveWallet :: Spec
testActiveWallet = around bracketWallet $
    describe "Active wallet sanity checks" $ do
      it "initially has no pending transactions" $ \w ->
        Kernel.hasPending w `shouldReturn` False

-- | Initialize active wallet in a manner suitable for unit testing
bracketWallet :: (Kernel.ActiveWallet -> IO a) -> IO a
bracketWallet test =
    bracketPassiveWallet $ \passive ->
      Kernel.bracketActiveWallet passive diffusion $ \active ->
        test active
  where
    -- TODO: Decide what we want to do with submitted transactions
    diffusion :: Kernel.WalletDiffusion
    diffusion =  Kernel.WalletDiffusion {
          walletSendTx = \_tx -> return False
        }

{-------------------------------------------------------------------------------
  Example hand-constructed chains
-------------------------------------------------------------------------------}

emptyBlock :: Hash h Addr => PreChain h Identity ()
emptyBlock = preChain $ \_boot -> return $ \_fees ->
    OldestFirst [OldestFirst []]

oneTrans :: Hash h Addr => PreChain h Identity ()
oneTrans = preChain $ \boot -> return $ \((fee : _) : _) ->
    let t1 = Transaction {
                 trFresh = 0
               , trFee   = fee
               , trHash  = 1
               , trIns   = Set.fromList [ Input (hash boot) 0 ] -- rich 0
               , trOuts  = [ Output r1 1000
                           , Output r0 (initR0 - 1000 - fee)
                           ]
               , trExtra = ["t1"]
               }
    in OldestFirst [OldestFirst [t1]]

-- Try to transfer from R0 to R1, but leaving R0's balance the same
overspend :: Hash h Addr => PreChain h Identity ()
overspend = preChain $ \boot -> return $ \((fee : _) : _) ->
    let t1 = Transaction {
                 trFresh = 0
               , trFee   = fee
               , trHash  = 1
               , trIns   = Set.fromList [ Input (hash boot) 0 ] -- rich 0
               , trOuts  = [ Output r1 1000
                           , Output r0 initR0
                           ]
               , trExtra = ["t1"]
               }
    in OldestFirst [OldestFirst [t1]]

-- Try to transfer to R1 and R2 using the same output
-- TODO: in principle this example /ought/ to work without any kind of
-- outputs at all; but in practice this breaks stuff because now we have
-- two identical transactions which would therefore get identical IDs?
doublespend :: Hash h Addr => PreChain h Identity ()
doublespend = preChain $ \boot -> return $ \((fee1 : fee2 : _) : _) ->
    let t1 = Transaction {
                 trFresh = 0
               , trFee   = fee1
               , trHash  = 1
               , trIns   = Set.fromList [ Input (hash boot) 0 ] -- rich 0
               , trOuts  = [ Output r1 1000
                           , Output r0 (initR0 - 1000 - fee1)
                           ]
               , trExtra = ["t1"]
               }
        t2 = Transaction {
                 trFresh = 0
               , trFee   = fee2
               , trHash  = 2
               , trIns   = Set.fromList [ Input (hash boot) 0 ] -- rich 0
               , trOuts  = [ Output r2 1000
                           , Output r0 (initR0 - 1000 - fee2)
                           ]
               , trExtra = ["t2"]
               }
    in OldestFirst [OldestFirst [t1, t2]]

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
example1 :: Hash h Addr => PreChain h Identity ()
example1 = preChain $ \boot -> return $ \((fee3 : fee4 : _) : _) ->
    let t3 = Transaction {
                 trFresh = 0
               , trFee   = fee3
               , trHash  = 3
               , trIns   = Set.fromList [ Input (hash boot) 0 ] -- rich 0
               , trOuts  = [ Output r1 1000
                           , Output r0 (initR0 - 1000 - fee3)
                           ]
               , trExtra = ["t3"]
               }
        t4 = Transaction {
                 trFresh = 0
               , trFee   = fee4
               , trHash  = 4
               , trIns   = Set.fromList [ Input (hash t3) 1 ]
               , trOuts  = [ Output r2 (initR0 - 1000 - fee3 - fee4) ]
               , trExtra = ["t4"]
               }
    in OldestFirst [OldestFirst [t3, t4]]

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

intAndVerifyPure :: PreChain GivenHash Identity a
                 -> ValidationResult GivenHash Addr
intAndVerifyPure = runIdentity . intAndVerify

intAndVerifyGen :: PreChain GivenHash Gen a
                -> Gen (ValidationResult GivenHash Addr)
intAndVerifyGen = intAndVerify

-- | Interpret and verify a chain, given the bootstrap transactions
intAndVerify :: (Hash h Addr, Monad m)
             => PreChain h m a -> m (ValidationResult h Addr)
intAndVerify = intAndVerifyChain

-- | Interpret and verify a chain, given the bootstrap transactions. Also
-- returns the 'FromPreChain' value, which contains the blockchain, ledger,
-- boot transaction, etc.
intAndVerifyChain :: (Hash h Addr, Monad m)
                  => PreChain h m a
                  -> m (ValidationResult h Addr)
intAndVerifyChain pc = runTranslateT $ do
    FromPreChain{..} <- fromPreChain pc
    let dslIsValid = ledgerIsValid fpcLedger
        dslUtxo    = ledgerUtxo    fpcLedger
    intResult <- catchTranslateErrors $ runIntBoot fpcBoot fpcChain
    case intResult of
      Left e ->
        case dslIsValid of
          Valid     () -> return $ Disagreement fpcLedger (UnexpectedError e)
          Invalid _ e' -> return $ ExpectedInvalid' e' e
      Right (chain', ctxt) -> do
        let chain'' = fromMaybe (error "intAndVerify: Nothing")
                    $ nonEmptyOldestFirst chain'
        isCardanoValid <- verifyBlocksPrefix chain''
        case (dslIsValid, isCardanoValid) of
          (Invalid _ e' , Invalid _ e) -> return $ ExpectedInvalid e' e
          (Invalid _ e' , Valid     _) -> return $ Disagreement fpcLedger (UnexpectedValid e')
          (Valid     () , Invalid _ e) -> return $ Disagreement fpcLedger (UnexpectedInvalid e)
          (Valid     () , Valid (_undo, finalUtxo)) -> do
            (finalUtxo', _) <- runIntT ctxt dslUtxo
            if finalUtxo == finalUtxo'
              then return $ ExpectedValid
              else return $ Disagreement fpcLedger UnexpectedUtxo {
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
