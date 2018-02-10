-- | Wallet unit tests
--
-- TODO: Take advantage of https://github.com/input-output-hk/cardano-sl/pull/2296 ?
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main (main) where

import Universum
import Formatting (sformat, bprint, build, (%), shown)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Prelude (Show(..))
import Serokell.Util (mapJson)
import qualified Data.Text.Buildable
import qualified Data.Set as Set

import Pos.Util.Chrono
import qualified Pos.Block.Error as Cardano
import qualified Pos.Txp.Toil    as Cardano

import qualified Cardano.Wallet.Kernel           as Kernel
import qualified Cardano.Wallet.Kernel.Diffusion as Kernel

import UTxO.Bootstrap
import UTxO.Context
import UTxO.DSL
import UTxO.Interpreter
import UTxO.PreChain
import UTxO.Translate

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
    sanityCheckTranslation
    quickCheckTranslation
    sanityCheckPassiveWallet
    sanityCheckActiveWallet

{-------------------------------------------------------------------------------
  UTxO->Cardano translation tests
-------------------------------------------------------------------------------}

sanityCheckTranslation :: Spec
sanityCheckTranslation =
    describe "Test sanity checks" $ do
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

quickCheckTranslation :: Spec
quickCheckTranslation =
    describe "QuickCheck sanity checks" $ do
      prop "can construct and verify block with one arbitrary transaction" $
        expectValid <$> intAndVerifyGen genOneTrans

{-------------------------------------------------------------------------------
  Passive wallet tests
-------------------------------------------------------------------------------}

sanityCheckPassiveWallet  :: Spec
sanityCheckPassiveWallet = around bracketPassiveWallet $
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

sanityCheckActiveWallet :: Spec
sanityCheckActiveWallet = around bracketWallet $
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
  Example QuickCheck generated chains
-------------------------------------------------------------------------------}

-- | Chain with a single transaction that transfers an arbitrary amount from
-- the first rich actor to the second.
genOneTrans :: Hash h Addr => PreChain h Gen
genOneTrans = PreChain $ \boot -> do
    -- TODO: The actual range we can use here is @(0, initR0 - fee)@ where
    -- @fee@ is the fee of the transaction. Sadly, however, we don't know
    -- this fee in advantage. Hence, any QuickCheck generators for transactions
    -- will need to be a little bit conversative (possibly using some kind of
    -- @maxFee@ upper bound).
    value <- choose (0, 1000)
    return $ \((fee : _) : _) ->
      let t1 = Transaction {
                   trFresh = 0
                 , trFee   = fee
                 , trHash  = 1
                 , trIns   = Set.fromList [ Input (hash boot) 0 ] -- rich 0
                 , trOuts  = [ Output r1 value
                             , Output r0 (initR0 - value - fee)
                             ]
                 }
      in OldestFirst [OldestFirst [t1]]

{-------------------------------------------------------------------------------
  Example hand-constructed chains
-------------------------------------------------------------------------------}

emptyBlock :: Hash h Addr => PreChain h Identity
emptyBlock = PreChain $ \_boot -> return $ \_fees ->
    OldestFirst [OldestFirst []]

oneTrans :: Hash h Addr => PreChain h Identity
oneTrans = PreChain $ \boot -> return $ \((fee : _) : _) ->
    let t1 = Transaction {
                 trFresh = 0
               , trFee   = fee
               , trHash  = 1
               , trIns   = Set.fromList [ Input (hash boot) 0 ] -- rich 0
               , trOuts  = [ Output r1 1000
                           , Output r0 (initR0 - 1000 - fee)
                           ]
               }
    in OldestFirst [OldestFirst [t1]]

-- Try to transfer from R0 to R1, but leaving R0's balance the same
overspend :: Hash h Addr => PreChain h Identity
overspend = PreChain $ \boot -> return $ \((fee : _) : _) ->
    let t1 = Transaction {
                 trFresh = 0
               , trFee   = fee
               , trHash  = 1
               , trIns   = Set.fromList [ Input (hash boot) 0 ] -- rich 0
               , trOuts  = [ Output r1 1000
                           , Output r0 initR0
                           ]
               }
    in OldestFirst [OldestFirst [t1]]

-- Try to transfer to R1 and R2 using the same output
-- TODO: in principle this example /ought/ to work without any kind of
-- outputs at all; but in practice this breaks stuff because now we have
-- two identical transactions which would therefore get identical IDs?
doublespend :: Hash h Addr => PreChain h Identity
doublespend = PreChain $ \boot -> return $ \((fee1 : fee2 : _) : _) ->
    let t1 = Transaction {
                 trFresh = 0
               , trFee   = fee1
               , trHash  = 1
               , trIns   = Set.fromList [ Input (hash boot) 0 ] -- rich 0
               , trOuts  = [ Output r1 1000
                           , Output r0 (initR0 - 1000 - fee1)
                           ]
               }
        t2 = Transaction {
                 trFresh = 0
               , trFee   = fee2
               , trHash  = 2
               , trIns   = Set.fromList [ Input (hash boot) 0 ] -- rich 0
               , trOuts  = [ Output r2 1000
                           , Output r0 (initR0 - 1000 - fee2)
                           ]
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
example1 :: Hash h Addr => PreChain h Identity
example1 = PreChain $ \boot -> return $ \((fee3 : fee4 : _) : _) ->
    let t3 = Transaction {
                 trFresh = 0
               , trFee   = fee3
               , trHash  = 3
               , trIns   = Set.fromList [ Input (hash boot) 0 ] -- rich 0
               , trOuts  = [ Output r1 1000
                           , Output r0 (initR0 - 1000 - fee3)
                           ]
               }
        t4 = Transaction {
                 trFresh = 0
               , trFee   = fee4
               , trHash  = 4
               , trIns   = Set.fromList [ Input (hash t3) 1 ]
               , trOuts  = [ Output r2 (initR0 - 1000 - fee3 - fee4) ]
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

intAndVerifyPure :: PreChain GivenHash Identity -> ValidationResult
intAndVerifyPure = runIdentity . intAndVerify

intAndVerifyGen :: PreChain GivenHash Gen -> Gen ValidationResult
intAndVerifyGen = intAndVerify

-- | Interpret and verify a chain, given the bootstrap transactions
intAndVerify :: (Hash h Addr, Monad m)
             => PreChain h m -> m ValidationResult
intAndVerify pc = runTranslateT $ do
    FromPreChain{..} <- fromPreChain pc
    let dslIsValid = ledgerIsValid fpcLedger
        dslUtxo    = ledgerUtxo    fpcLedger
    intResult <- catchTranslateErrors $ runIntBoot fpcBoot fpcChain
    case intResult of
      Left e ->
        if dslIsValid
          then return $ Disagreement (UnexpectedError e)
          else return $ ExpectedInvalid (Right e)
      Right (chain', ctxt) -> do
        let chain'' = fromMaybe (error "intAndVerify: Nothing")
                    $ nonEmptyOldestFirst chain'
        isCardanoValid <- verifyBlocksPrefix chain''
        case (dslIsValid, isCardanoValid) of
          (False, Left e)  -> return $ ExpectedInvalid (Left e)
          (False, Right _) -> return $ Disagreement UnexpectedValid
          (True,  Left e)  -> return $ Disagreement (UnexpectedInvalid e)
          (True,  Right (_undo, utxo)) -> do
            (utxo', _) <- runIntT ctxt dslUtxo
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
--
-- We consider something to be "unexpectedly foo" when Cardano says it's
-- " foo " but the DSL says it's " not foo "; the DSL is the spec, after all
-- (of course that doesn't mean that it cannot contain bugs :).
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
  | forall h. Hash h Addr => UnexpectedUtxo {
        utxoDsl     :: Utxo h Addr
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
