{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Idealized specification of UTxO-style accounting
module UTxO.DSL (
    -- * Parameters
    Value
  , Index
    -- * Addresses
  , Address(..)
    -- * Transaction
  , Transaction(..)
  , trIns'
  , trIsAcceptable
  , trBalance
  , trSpentOutputs
  , trUnspentOutputs
  , trUtxo
    -- * Outputs
  , Output(..)
    -- * Inputs
  , Input(..)
  , inpTransaction
  , inpSpentOutput
  , inpSpentOutput'
  , inpVal
  , inpVal'
    -- * Ledger
  , Ledger(..)
  , ledgerToNewestFirst
  , ledgerEmpty
  , ledgerSingleton
  , ledgerAdd
  , ledgerAdds
  , ledgerTails
  , ledgerBalance
  , ledgerUnspentOutputs
  , ledgerUtxo
  , ledgerIsValid
  , ledgerAddresses
    -- * Hash
  , Hash(..)
  , GivenHash(..)
  , IdentityAsHash
  , givenHash
  , findHash
  , findHash'
    -- * Additional
    -- ** UTxO
  , Utxo(..)
  , utxoEmpty
  , utxoNull
  , utxoApply
  , utxoFromMap
  , utxoFromList
  , utxoSingleton
  , utxoToList
  , utxoDomain
  , utxoRange
  , utxoSize
  , utxoBalance
  , utxoUnion
  , utxoUnions
  , utxoInsert
  , utxoDelete
  , utxoRestrictToAddr
  , utxoRestrictToInputs
  , utxoRemoveInputs
  , utxoLookup
  , utxoAmountForInput
  , utxoAddressForInput
    -- ** Chain
  , Block
  , Chain
  , chainToLedger
  , utxoApplyBlock
  ) where

import           Control.Exception (throw)
import           Control.Monad.Except (MonadError (..))
import           Data.Foldable (Foldable (..), foldr, sum)
import           Data.List (tail)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Validated
import           Formatting (bprint, build, sformat, (%))
import qualified Formatting.Buildable
import           Pos.Core.Chrono (NewestFirst (NewestFirst),
                     OldestFirst (getOldestFirst))
import           Prelude (Show (..))
import           Serokell.Util (listJson, mapJson)
import           Universum hiding (Foldable, foldr, sum, tail, toList)

import           Cardano.Wallet.Kernel.Util (at, restrictKeys, withoutKeys)

{-------------------------------------------------------------------------------
  Parameters
-------------------------------------------------------------------------------}

type Value = Word64
type Index = Word32

{-------------------------------------------------------------------------------
  Addresses
-------------------------------------------------------------------------------}

-- | Address
--
-- We identity some special addresses to deal with fresh coin generation and
-- fees. This is only used in balance computations.
data Address a =
    AddrGenesis
  | AddrTreasury
  | AddrRegular a
  deriving (Eq, Ord)

{-------------------------------------------------------------------------------
  Transactions

  We define UTxO-style transactions only; for our purposes account-style
  transactions are not needed.
-------------------------------------------------------------------------------}

data Transaction h a = Transaction {
      trFresh :: Value
    -- ^ The money that is created by this transaction. This money
    -- implicitly comes from the treasury.
    , trIns   :: Set (Input h a)
    -- ^ The set of input transactions that feed this transaction.
    , trOuts  :: [Output h a]
    -- ^ The list of outputs for this transaction.
    , trFee   :: Value
    -- ^ The fee charged to this transaction.
    , trHash  :: Int
    -- ^ The hash of this transaction. Must be unique in the entire chain.
    , trExtra :: [Text]
    -- ^ Free-form comments, used for debugging
    }

-- | The inputs as a list
--
-- Useful in various calculations
trIns' :: Transaction h a -> [Input h a]
trIns' = Set.toList . trIns

-- | Whether this transaction is acceptable for the given ledger
--
-- NOTE: The notion of 'valid' is not relevant for UTxO transactions,
-- so we omit it.
trIsAcceptable :: (Hash h a, Buildable a)
               => Transaction h a -> Ledger h a -> Validated Text ()
trIsAcceptable t l = sequence_ [
      allInputsHaveOutputs
    , valueIsPreserved
    , inputsHaveNotBeenSpent
    ]
  where
    allInputsHaveOutputs :: Validated Text ()
    allInputsHaveOutputs = forM_ (trIns t) $ \inp ->
        whenNothing_ (inpSpentOutput inp l) $
          throwError (sformat
            ( "In transaction "
            % build
            % ": cannot resolve input "
            % build
            )
            t
            inp)

    -- TODO: Ideally, we would require here that @sumIn == sumOut@. However,
    -- as long as we have to be conservative about fees, we will not be able
    -- to achieve that in the unit tests.
    valueIsPreserved :: Validated Text ()
    valueIsPreserved =
        unless (sumIn >= sumOut) $
          throwError $ sformat
            ( "In transaction "
            % build
            % ": value not preserved (in: "
            % build
            % ", out: "
            % build
            % "; difference "
            % build
            % ")"
            )
            t
            sumIn
            sumOut
            -- avoid overflow
            (if sumOut > sumIn then sumOut - sumIn else sumIn - sumOut)
      where
        sumIn  = sum (map (`inpVal'` l) (trIns' t)) + trFresh t
        sumOut = sum (map outVal        (trOuts t)) + trFee   t

    inputsHaveNotBeenSpent :: Validated Text ()
    inputsHaveNotBeenSpent = forM_ (trIns t) $ \inp ->
        unless (inp `Set.member` ledgerUnspentOutputs l) $
          throwError $ sformat
            ( "In transaction "
            % build
            % ": input "
            % build
            % " already spent"
            )
            t
            inp

-- | The effect this transaction has on the balance of an address
trBalance :: forall h a. (Hash h a, Eq a)
          => Address a -> Transaction h a -> Ledger h a -> Value
trBalance a t l = received - spent
  where
    received, spent :: Value
    received = total outputsReceived + case a of
                                         AddrTreasury -> trFee t
                                         _otherwise   -> 0
    spent    = total outputsSpent    + case a of
                                         AddrGenesis -> trFresh t
                                         _otherwise  -> 0

    outputsReceived, outputsSpent :: [Output h a]
    outputsReceived = our $                            trOuts t
    outputsSpent    = our $ map (`inpSpentOutput'` l) (trIns' t)

    our :: [Output h a] -> [Output h a]
    our = filter (\o -> AddrRegular (outAddr o) == a)

    total :: [Output h a] -> Value
    total = sum . map outVal

-- | The outputs spent by this transaction
--
-- Defined only for consistency.
trSpentOutputs :: Transaction h a -> Set (Input h a)
trSpentOutputs = trIns

-- | The outputs generated by this transaction
trUnspentOutputs :: Hash h a => Transaction h a -> Set (Input h a)
trUnspentOutputs = utxoDomain . trUtxo

-- | The UTxO generated by this transaction
trUtxo :: Hash h a => Transaction h a -> Utxo h a
trUtxo t = utxoFromList $
             zipWith (\i o -> (Input (hash t) i, o)) [0..] (trOuts t)

{-------------------------------------------------------------------------------
  Outputs
-------------------------------------------------------------------------------}

-- | Transaction output
--
-- The hash argument here is not used but we include it for consistency.
--
-- NOTE: In the spec, this allows for @Address a@ rather than @a@. This is not
-- needed in Cardano, where that additional flexibility is not supported. We
-- therefore use this more restricted version.
data Output (h :: * -> *) a = Output {
      outAddr :: a
    , outVal  :: Value
    }
  deriving (Eq, Ord)

{-------------------------------------------------------------------------------
  Inputs
-------------------------------------------------------------------------------}

data Input h a = Input
    { inpTrans :: h (Transaction h a)
      -- ^ The hash of the 'Transaction' where the 'Output' that this 'Input'
      -- spends is found.
    , inpIndex :: Index
      -- ^ Index to a particular 'Output' among the 'trOut' outputs in
      -- the 'Transaction' idenfified  by 'inpTrans'. Said 'Output' is the one
      -- that this 'Input' is spending.
    }

deriving instance Hash h a => Eq  (Input h a)
deriving instance Hash h a => Ord (Input h a)

-- | Obtain the 'Transaction' to which 'Input' refers.
--
-- Returns 'Nothing' if the 'Transaction' is missing from the 'Ledger'.
inpTransaction :: Hash h a => Input h a -> Ledger h a -> Maybe (Transaction h a)
inpTransaction = findHash . inpTrans

-- | Obtain the 'Output' that the given 'Input' spent.
--
-- Returns 'Nothing' if the 'Transaction' to which this 'Input' refers is
-- missing from the 'Ledger'.
inpSpentOutput :: Hash h a => Input h a -> Ledger h a -> Maybe (Output h a)
inpSpentOutput i l = do
    t <- inpTransaction i l
    trOuts t `at` fromIntegral (inpIndex i)

-- | Obtain the 'Value' in the 'Output' spent by the given 'Input'.
--
-- Returns 'Nothing' if the 'Transaction' to which this 'Input' refers is
-- missing from the 'Ledger'.
inpVal :: Hash h a => Input h a -> Ledger h a -> Maybe Value
inpVal i l = outVal <$> inpSpentOutput i l

{-------------------------------------------------------------------------------
  Variations on the functions on inputs, when we are sure that the
  transaction is known and the input index is correct
-------------------------------------------------------------------------------}

inpTransaction' :: Hash h a
                => Input h a -> Ledger h a -> Transaction h a
inpTransaction' = findHash' . inpTrans

inpSpentOutput' :: (Hash h a, HasCallStack)
                => Input h a -> Ledger h a -> Output h a
inpSpentOutput' i l = fromJust err $
      trOuts (inpTransaction' i l) `at` fromIntegral (inpIndex i)
  where
    err = sformat ("Input index out of bounds: " % build) i

inpVal' :: Hash h a => Input h a -> Ledger h a -> Value
inpVal' i = outVal . inpSpentOutput' i

{-------------------------------------------------------------------------------
  Ledger
-------------------------------------------------------------------------------}

-- | Ledger (list of transactions)
--
-- The ledger is stored in newest-first order. To enforce this, the constructor
-- is marked as unsafe.
newtype Ledger h a = Ledger {
    ledgerTransactions :: NewestFirst [] (Transaction h a)
  }

ledgerEmpty :: Ledger h a
ledgerEmpty = Ledger (NewestFirst [])

ledgerSingleton :: Transaction h a -> Ledger h a
ledgerSingleton t = Ledger (NewestFirst [t])

ledgerToNewestFirst :: Ledger h a -> [Transaction h a]
ledgerToNewestFirst (Ledger l) = toList l

-- | Append single transaction to the ledger
ledgerAdd :: Transaction h a -> Ledger h a -> Ledger h a
ledgerAdd = ledgerAdds . NewestFirst . (:[])

-- | Append a bunch of transactions to the ledger
ledgerAdds :: NewestFirst [] (Transaction h a) -> Ledger h a -> Ledger h a
ledgerAdds (NewestFirst ts) (Ledger (NewestFirst l)) =
    Ledger (NewestFirst (ts ++ l))

-- | Each transaction in the ledger, along with its context (the transactions
-- it's allowed to refer to)
ledgerTails :: Ledger h a -> [(Transaction h a, Ledger h a)]
ledgerTails (Ledger (NewestFirst l)) =
    zipWith (\t ts -> (t, Ledger (NewestFirst ts))) l (tail (tails l))

ledgerBalance :: forall h a. (Hash h a, Eq a)
              => Address a -> Ledger h a -> Value
ledgerBalance a l = sum $ map (uncurry (trBalance a)) (ledgerTails l)

-- | Unspent outputs in the ledger
--
-- Should satisfy that
--
-- > ledgerUnspentOutputs l = Map.keysSet (ledgerUtxo l)
ledgerUnspentOutputs :: forall h a. Hash h a => Ledger h a -> Set (Input h a)
ledgerUnspentOutputs l = go (ledgerToNewestFirst l)
  where
    go :: [Transaction h a] -> Set (Input h a)
    go []     = Set.empty
    go (t:ts) = (go ts Set.\\ trSpentOutputs t) `Set.union` trUnspentOutputs t

-- | UTxO of a ledger
--
-- TODO: We should have a property relating this to 'ledgerBalance'.
ledgerUtxo :: forall h a. Hash h a => Ledger h a -> Utxo h a
ledgerUtxo l = go (ledgerToNewestFirst l)
  where
    go :: [Transaction h a] -> Utxo h a
    go = foldr utxoApply utxoEmpty

-- | Ledger validity
ledgerIsValid :: (Hash h a, Buildable a) => Ledger h a -> Validated Text ()
ledgerIsValid l = mapM_ (uncurry trIsAcceptable) (ledgerTails l)

-- | Extracts the set of addresses present in the ledger.
ledgerAddresses :: Ord a => Ledger h a -> Set a
ledgerAddresses = Set.fromList
    . map outAddr
    . concatMap trOuts
    . ledgerToNewestFirst

{-------------------------------------------------------------------------------
  We parameterize over the hashing function
-------------------------------------------------------------------------------}

-- | Generalization of a hashing function
--
-- Ideally we'd strip the @a@ parameter here, but that would mean we'd need
-- quantified contexts to model the superclass constraint, which sadly we
-- don't have in ghc yet.
class ( Ord       (h (Transaction h a))
      , Buildable (h (Transaction h a))
      ) => Hash h a where
  -- | Hash a transaction
  hash :: Transaction h a -> h (Transaction h a)

-- | Locate a transaction in the ledger, giving its hash
--
-- NOTE: Even when we instantiate @h@ to 'Identity', we still want to search
-- the ledger, because an input that refers to a transaction that isn't
-- actually in the ledger would be invalid.
findHash :: Hash h a
         => h (Transaction h a) -> Ledger h a -> Maybe (Transaction h a)
findHash h l = find (\t -> hash t == h) (ledgerToNewestFirst l)

-- | Variation on 'findHash', assumes hash refers to existing transaction
findHash' :: (Hash h a, HasCallStack)
          => h (Transaction h a) -> Ledger h a -> Transaction h a
findHash' h l = fromJust err (findHash h l)
  where
    err = sformat ("Hash not found: " % build) h

{-------------------------------------------------------------------------------
  Additional: UTxO
-------------------------------------------------------------------------------}

-- | Unspent transaction outputs.
--
-- The underlying representation is a @'Map' ('Input' h a) ('Output' h a)@,
-- which is not particularly helpful to understanding the meaning of this
-- type. Other ways to understand it are:
--
-- * A @'Set' ('Input' h a)@ where each input has an 'Output' detailing the
--   total amount of the 'Input' value and the address to which it belongs.
-- * A relation on with columns @input@, @coin@, and @address@, with
--   a primary index on the @input@
newtype Utxo h a = Utxo { utxoToMap :: Map (Input h a) (Output h a) }

deriving instance (Hash h a, Eq a) => Eq (Utxo h a)

-- | Empty UTxO
utxoEmpty :: Utxo h a
utxoEmpty = Utxo Map.empty

-- | Check if a UTxO is empty
utxoNull :: Utxo h a -> Bool
utxoNull = Map.null . utxoToMap

-- | Apply a transaction to a UTxO
--
-- We have that
--
-- > utxoApply t utxoEmpty == trUtxo t
utxoApply :: Hash h a => Transaction h a -> Utxo h a -> Utxo h a
utxoApply t u = utxoRemoveInputs (trSpentOutputs t) u `utxoUnion` trUtxo t

-- | Construct a 'Utxo' from a 'Map' of 'Input's. The 'Output' that each
-- 'Input' in the map point to should represent the total value of that
-- 'Input' along with the address that the 'Input' currently belongs to.
utxoFromMap :: Map (Input h a) (Output h a) -> Utxo h a
utxoFromMap = Utxo

-- | Construct a 'Utxo' from a list of 'Input's. The 'Output' that each
-- 'Input' is paired with should represent the total value of that 'Input'
-- along with the address that the 'Input' currently belongs to.
utxoFromList :: Hash h a => [(Input h a, Output h a)] -> Utxo h a
utxoFromList = utxoFromMap . Map.fromList

-- | Singleton UTxO
utxoSingleton :: Hash h a => Input h a -> Output h a -> Utxo h a
utxoSingleton i o = utxoFromList [(i, o)]

utxoToList :: Utxo h a -> [(Input h a, Output h a)]
utxoToList = Map.toList . utxoToMap

-- | For a given 'Input', return the 'Output' that contains the address of
-- the owner and value for the 'Input'.
utxoLookup :: Hash h a => Input h a -> Utxo h a -> Maybe (Output h a)
utxoLookup i = Map.lookup i . utxoToMap

-- | Look up the 'Value' amount for an 'Input' in the 'Utxo'.
utxoAmountForInput :: Hash h a => Input h a -> Utxo h a -> Maybe Value
utxoAmountForInput i = fmap outVal . utxoLookup i

-- | Look up the @address@ to which the given 'Input' belongs.
utxoAddressForInput :: Hash h a => Input h a -> Utxo h a -> Maybe a
utxoAddressForInput i = fmap outAddr . utxoLookup i

-- | This returns the set of 'Input' that are currently unspent. This
-- function discards the information about how much value is in the input
-- and to what address the input is sent.
utxoDomain :: Utxo h a -> Set (Input h a)
utxoDomain = Map.keysSet . utxoToMap

-- | This returns the 'Output's that make up the unspent inputs. The
-- 'Output's contain the total value and owning address for their
-- respective 'Input's.
utxoRange :: Utxo h a -> [Output h a]
utxoRange = Map.elems . utxoToMap

-- | Number of entries in the UTxO
utxoSize :: Utxo h a -> Int
utxoSize = Map.size . utxoToMap

utxoBalance :: Utxo h a -> Value
utxoBalance = sum . map outVal . Map.elems . utxoToMap

utxoUnion :: Hash h a => Utxo h a -> Utxo h a -> Utxo h a
utxoUnion (Utxo utxo) (Utxo utxo') = Utxo (utxo `Map.union` utxo')

utxoUnions :: Hash h a => [Utxo h a] -> Utxo h a
utxoUnions = Utxo . Map.unions . map utxoToMap

utxoInsert :: Hash h a => (Input h a, Output h a) -> Utxo h a -> Utxo h a
utxoInsert (i, o) = Utxo . Map.insert i o . utxoToMap

utxoDelete :: Hash h a => Input h a -> Utxo h a -> Utxo h a
utxoDelete i = Utxo . Map.delete i . utxoToMap

-- | Filter the 'Utxo' to only contain unspent transaction outputs whose
-- address satisfy the given predicate.
utxoRestrictToAddr :: (a -> Bool) -> Utxo h a -> Utxo h a
utxoRestrictToAddr p = Utxo . Map.filter (p . outAddr) . utxoToMap

utxoRestrictToInputs :: Hash h a => Set (Input h a) -> Utxo h a -> Utxo h a
utxoRestrictToInputs inps (Utxo utxo) = Utxo (utxo `restrictKeys` inps)

utxoRemoveInputs :: Hash h a => Set (Input h a) -> Utxo h a -> Utxo h a
utxoRemoveInputs inps (Utxo utxo) = Utxo (utxo `withoutKeys` inps)

{-------------------------------------------------------------------------------
  Additional: chain
-------------------------------------------------------------------------------}

-- | Block of transactions
type Block h a = OldestFirst [] (Transaction h a)

-- | A chain
--
-- A chain is just a series of blocks, here modelled simply as the transactions
-- they contain, since the rest of the block information can then be inferred.
type Chain h a = OldestFirst [] (Block h a)

chainToLedger :: Transaction h a -> Chain h a -> Ledger h a
chainToLedger boot = Ledger
                   . NewestFirst
                   . reverse
                   . (boot :)
                   . concatMap toList . toList

-- | Compute the UTxO after a block has been applied
--
-- Note: we process all transactions one by one. This may not be the most
-- efficient way to do this; this should be regarded as a specification, not
-- a realistic implementation.
utxoApplyBlock :: forall h a. Hash h a => Block h a -> Utxo h a -> Utxo h a
utxoApplyBlock = go . getOldestFirst
  where
    go :: [Transaction h a] -> Utxo h a -> Utxo h a
    go []     u = u
    go (t:ts) u = go ts (utxoApply t u)

{-------------------------------------------------------------------------------
  Instantiating the hash to the identity
-------------------------------------------------------------------------------}

-- | Instantiate the hash to identity function
--
-- NOTE: A lot of definitions in the DSL rely on comparing 'Input's. When using
-- 'Identity' as the " hash ", comparing 'Input's implies comparing their
-- 'Transactions', and hence the cost of comparing two inputs grows linearly
-- with their position in the chain.
newtype IdentityAsHash a = IdentityAsHash a

-- | We define 'Eq' for @IdentityAsHash (Transaction h a)@ instead of
-- for @Transaction h a@ directly, as we normally don't want to compare
-- transactions, but rather transaction hashes.
instance (Hash h a, Eq a) => Eq (IdentityAsHash (Transaction h a)) where
  IdentityAsHash tx1 == IdentityAsHash tx2 = and [
        trHash  tx1 == trHash  tx2  -- comparing given hash usually suffices
      , trFresh tx1 == trFresh tx2
      , trIns   tx1 == trIns   tx2
      , trOuts  tx1 == trOuts  tx2
      , trFee   tx1 == trFee   tx2
      , trExtra tx1 == trExtra tx2
      ]

-- | See comments for 'Eq' instance.
instance (Hash h a, Ord a) => Ord (IdentityAsHash (Transaction h a)) where
  compare (IdentityAsHash tx1) (IdentityAsHash tx2) = mconcat [
        compare (trHash  tx1) (trHash  tx2) -- comparing given hash usually suffices
      , compare (trFresh tx1) (trFresh tx2)
      , compare (trIns   tx1) (trIns   tx2)
      , compare (trOuts  tx1) (trOuts  tx2)
      , compare (trFee   tx1) (trFee   tx2)
      , compare (trExtra tx1) (trExtra tx2)
      ]

instance (Ord a, Buildable a) => Hash IdentityAsHash a where
  hash = IdentityAsHash

instance (Ord a, Buildable a) => Buildable (IdentityAsHash (Transaction IdentityAsHash a)) where
  build (IdentityAsHash t) = bprint build t

{-------------------------------------------------------------------------------
  Use the specified hash instead
-------------------------------------------------------------------------------}

newtype GivenHash a = GivenHash Int
  deriving (Eq, Ord)

instance Buildable (GivenHash a) where
  build (GivenHash i) = bprint build i

instance Hash GivenHash a where
  hash = GivenHash . trHash

-- | The given hash is independent from any actual hash function
givenHash :: Transaction h a -> GivenHash (Transaction h a)
givenHash = GivenHash . trHash

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable a => Buildable (Address a) where
  build AddrGenesis     = "AddrGenesis"
  build AddrTreasury    = "AddrTreasury"
  build (AddrRegular a) = bprint ("AddrRegular " % build) a

instance Buildable a => Buildable (Output h a) where
  build Output{..} = bprint
      ( "Output"
      % "{ addr: " % build
      % ", val:  " % build
      % "}"
      )
      outAddr
      outVal

instance Hash h a => Buildable (Input h a) where
  build Input{..} = bprint
      ( "Input"
      % "{ trans: " % build
      % ", index: " % build
      % "}"
      )
      inpTrans
      inpIndex

instance (Buildable a, Hash h a) => Buildable (Transaction h a) where
  build Transaction{..} = bprint
      ( "Transaction"
      % "{ fresh: " % build
      % ", ins:   " % listJson
      % ", outs:  " % mapJson
      % ", fee:   " % build
      % ", hash:  " % build
      % ", extra: " % listJson
      % "}"
      )
      trFresh
      trIns
      (Map.fromList (zip outputIndices trOuts))
      trFee
      trHash
      trExtra
    where
      -- The output is easier to read when we see actual indices for outputs
      outputIndices :: [Int]
      outputIndices = [0..]

instance (Buildable a, Hash h a) => Buildable (Chain h a) where
  build blocks = bprint
      ( "Chain"
      % "{ blocks: " % listJson
      % "}"
      )
      blocks

instance ( Buildable a, Hash h a, Foldable f) => Buildable (NewestFirst f (Transaction h a)) where
  build ts = bprint ("NewestFirst " % listJson) (toList ts)

instance (Buildable a, Hash h a, Foldable f) => Buildable (OldestFirst f (Transaction h a)) where
  build ts = bprint ("OldestFirst " % listJson) (toList ts)

instance (Buildable a, Hash h a) => Buildable (Ledger h a) where
  build (Ledger l) = bprint build l

instance (Buildable a, Hash h a) => Buildable (Utxo h a) where
  build (Utxo utxo) = bprint ("Utxo " % mapJson) utxo

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

data UtxoException = UtxoException CallStack Text

instance Show UtxoException where
  show (UtxoException cs err) =
    "Utxo exception: " ++ toString err ++ " at " ++ prettyCallStack cs

instance Exception UtxoException

-- | Throw a 'UtxoException' on 'Nothing'
--
-- NOTE: We cannot call 'error' from "Universum" because it doesn't have
-- a 'HasCallStack' context.
fromJust :: HasCallStack => Text -> Maybe a -> a
fromJust _ (Just a) = a
fromJust e Nothing  = throw (UtxoException callStack e)
