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
  , utxoToList
  , utxoDomain
  , utxoRange
  , utxoUnion
  , utxoUnions
  , utxoRestrictToAddr
  , utxoRestrictToInputs
  , utxoRemoveInputs
  , utxoOutputForInput
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
import qualified Data.Text.Buildable
import           Formatting (bprint, build, sformat, (%))
import           Pos.Util.Chrono
import           Prelude (Show (..))
import           Serokell.Util (listJson, mapJson)
import           Universum hiding (Foldable, tail, toList, foldr, sum)

import           Util
import           Util.Validated

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
    , trOuts  :: [Output a]
    -- ^ The list of outputs for this transaction.
    , trFee   :: Value
    -- ^ The fee charged to this transaction.
    , trHash  :: Int
    -- ^ The hash of this transaction. Must be unique in the entire chain.
    , trExtra :: [Text]
    -- ^ Free-form comments, used for debugging
    }

deriving instance (Hash h a, Eq  a) => Eq  (Transaction h a)
deriving instance (Hash h a, Ord a) => Ord (Transaction h a)

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
trBalance :: forall h a. (Hash h a, Eq a, Buildable a)
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

    outputsReceived, outputsSpent :: [Output a]
    outputsReceived = our $                            trOuts t
    outputsSpent    = our $ map (`inpSpentOutput'` l) (trIns' t)

    our :: [Output a] -> [Output a]
    our = filter (\o -> AddrRegular (outAddr o) == a)

    total :: [Output a] -> Value
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
-- NOTE: In the spec, this allows for @Address a@ rather than @a@. This is not
-- needed in Cardano, where that additional flexibility is not supported. We
-- therefore use this more restricted version.
data Output a = Output {
      outAddr :: a
    , outVal  :: Value
    }
  deriving (Eq, Ord)

{-------------------------------------------------------------------------------
  Inputs
-------------------------------------------------------------------------------}

data Input h a = Input {
      inpTrans :: h (Transaction h a)
    , inpIndex :: Index
    }

deriving instance Hash h a => Eq  (Input h a)
deriving instance Hash h a => Ord (Input h a)

inpTransaction :: Hash h a => Input h a -> Ledger h a -> Maybe (Transaction h a)
inpTransaction = findHash . inpTrans

inpSpentOutput :: Hash h a => Input h a -> Ledger h a -> Maybe (Output a)
inpSpentOutput i l = do
    t <- inpTransaction i l
    trOuts t `at` fromIntegral (inpIndex i)

inpVal :: Hash h a => Input h a -> Ledger h a -> Maybe Value
inpVal i l = outVal <$> inpSpentOutput i l

{-------------------------------------------------------------------------------
  Variations on the functions on inputs, when we are sure that the
  transaction is known and the input index is correct
-------------------------------------------------------------------------------}

inpTransaction' :: (Hash h a, Buildable a)
                => Input h a -> Ledger h a -> Transaction h a
inpTransaction' = findHash' . inpTrans

inpSpentOutput' :: (Hash h a, Buildable a, HasCallStack)
                => Input h a -> Ledger h a -> Output a
inpSpentOutput' i l = fromJust err $
      trOuts (inpTransaction' i l) `at` fromIntegral (inpIndex i)
  where
    err = sformat ("Input index out of bounds: " % build) i

inpVal' :: (Hash h a, Buildable a) => Input h a -> Ledger h a -> Value
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

ledgerBalance :: forall h a. (Hash h a, Eq a, Buildable a)
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
findHash' :: (Hash h a, Buildable a, HasCallStack)
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
newtype Utxo h a = Utxo { utxoToMap :: Map (Input h a) (Output a) }

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
utxoFromMap :: Map (Input h a) (Output a) -> Utxo h a
utxoFromMap = Utxo

-- | Construct a 'Utxo' from a list of 'Input's. The 'Output' that each
-- 'Input' is paired with should represent the total value of that 'Input'
-- along with the address that the 'Input' currently belongs to.
utxoFromList :: Hash h a => [(Input h a, Output a)] -> Utxo h a
utxoFromList = utxoFromMap . Map.fromList

utxoToList :: Utxo h a -> [(Input h a, Output a)]
utxoToList = Map.toList . utxoToMap

-- | For a given 'Input', return the 'Output' that contains the address of
-- the owner and value for the 'Input'.
utxoOutputForInput :: Hash h a => Input h a -> Utxo h a -> Maybe (Output a)
utxoOutputForInput i = Map.lookup i . utxoToMap

-- | Look up the 'Value' amount for an 'Input' in the 'Utxo'.
utxoAmountForInput :: Hash h a => Input h a -> Utxo h a -> Maybe Value
utxoAmountForInput i = fmap outVal . utxoOutputForInput i

-- | Look up the @address@ to which the given 'Input' belongs.
utxoAddressForInput :: Hash h a => Input h a -> Utxo h a -> Maybe a
utxoAddressForInput i = fmap outAddr . utxoOutputForInput i

-- | This returns the set of 'Input' that are currently unspent. This
-- function discards the information about how much value is in the input
-- and to what address the input is sent.
utxoDomain :: Utxo h a -> Set (Input h a)
utxoDomain = Map.keysSet . utxoToMap

-- | This returns the 'Output's that make up the unspent inputs. The
-- 'Output's contain the total value and owning address for their
-- respective 'Input's.
utxoRange :: Utxo h a -> [Output a]
utxoRange = Map.elems . utxoToMap

utxoUnion :: Hash h a => Utxo h a -> Utxo h a -> Utxo h a
utxoUnion (Utxo utxo) (Utxo utxo') = Utxo (utxo `Map.union` utxo')

utxoUnions :: Hash h a => [Utxo h a] -> Utxo h a
utxoUnions = Utxo . Map.unions . map utxoToMap

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
    go []     = identity
    go (t:ts) = go ts . utxoApply t

{-------------------------------------------------------------------------------
  Instantiating the hash to the identity

  NOTE: A lot of definitions in the DSL rely on comparing 'Input's. When using
  'Identity' as the " hash ", comparing 'Input's implies comparing their
  'Transactions', and hence the cost of comparing two inputs grows linearly
  with their position in the chain.
-------------------------------------------------------------------------------}

instance (Ord a, Buildable a) => Hash Identity a where
  hash = Identity

instance (Ord a, Buildable a) => Buildable (Identity (Transaction Identity a)) where
  build (Identity t) = bprint build t

{-------------------------------------------------------------------------------
  Use the specified hash instead
-------------------------------------------------------------------------------}

newtype GivenHash a = GivenHash Int
  deriving (Eq, Ord)

instance Buildable (GivenHash a) where
  build (GivenHash i) = bprint build i

instance Hash GivenHash a where
  hash = GivenHash . trHash

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable a => Buildable (Address a) where
  build AddrGenesis     = "AddrGenesis"
  build AddrTreasury    = "AddrTreasury"
  build (AddrRegular a) = bprint ("AddrRegular " % build) a

instance Buildable a => Buildable (Output a) where
  build Output{..} = bprint
      ( "Output"
      % "{ addr: " % build
      % ", val:  " % build
      % "}"
      )
      outAddr
      outVal

instance (Buildable a, Hash h a) => Buildable (Input h a) where
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
      % ", outs:  " % listJson
      % ", fee:   " % build
      % ", hash:  " % build
      % ", extra: " % listJson
      % "}"
      )
      trFresh
      trIns
      trOuts
      trFee
      trHash
      trExtra

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
