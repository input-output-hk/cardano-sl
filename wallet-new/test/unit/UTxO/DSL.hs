-- | "Translating and Unifying UTxO-based and Account-based Cryptocurrencies"
--
-- Haskell translation of the UTxO definitions in said paper, with the goal of
-- having a clean definition of UTxO-style accounting that could be used to
-- drive testing for the wallet (amongst other things).

{-# LANGUAGE DeriveAnyClass #-}

module UTxO.DSL (
    -- * Types
    Index
  , Value
  , Address(..)
  , Input(..)
  , Output(..)
  , Transaction(..)
  , TransactionType(..)
  , Ledger
    -- * Validity
  , isValidTransaction
  , isValidLedger
    -- * Derived info
  , out
  , balance
    -- * Examples
  , example1
    -- * Computation of the UTxO (not present in the paper)
  , Utxo(..)
  , utxoToList
  , utxoAfter
    -- * Additional types and functions
  , Block(..)
  , Chain(..)
  , chainToLedger
  , outputIsFee
    -- * Convenience re-exports
  , dumpStr
  ) where

import Universum
import Data.Bifunctor (first)
import Data.List ((!!))
import Formatting (bprint, build, (%))
import Serokell.Util (listJson, mapJson)
import Text.Show.Pretty (PrettyVal, dumpStr)
import qualified Data.Set        as Set
import qualified Data.Map.Strict as Map
import qualified Data.Text.Buildable

{-------------------------------------------------------------------------------
  Core UTxO formalization
-------------------------------------------------------------------------------}

type Index = Word32
type Value = Word64

-- | Address
--
-- The code is polymorphic in what we consider to be an "ordinary address",
-- so that for instance we can instantiate it to a wallet/account/address.
--
-- (No associated Definition, defined in intro to Section 2, Preliminaries.)
data Address a =
    AddrGenesis
  | AddrTreasury
  | AddrOrdinary a
  deriving (Show, Eq, Ord, Generic, PrettyVal)

-- | Transaction input
--
-- TODO: Do we want to keep the inductive nature of Bruno's paper here,
-- or do we want to introduce transaction IDs?
--
-- (Definition 1.)
data Input a = Input {
      inpTrans :: Transaction a
    , inpIndex :: Index
    }
  deriving (Show, Eq, Ord, Generic, PrettyVal)

-- | Transaction output
--
-- Invariant: outAddr /= AddrGenesis
--
-- (Definition 1.)
data Output a = Output {
      outAddr :: Address a
    , outVal  :: Value
    }
  deriving (Show, Eq, Ord, Generic, PrettyVal)

-- | Transaction
--
-- (Definition 1.)
data Transaction a = Transaction {
      trIns  :: [Input a]
    , trOuts :: [Output a]
    }
  deriving (Show, Eq, Ord, Generic, PrettyVal)

-- | Transaction type
--
-- (Definition 1.)
data TransactionType = TransRegular | TransGenesis

-- | Classify transactions
--
-- (Definition 1.)
transactionType :: Transaction a -> TransactionType
transactionType t =
    if null (trIns t)
      then TransGenesis
      else TransRegular

-- | The output spent by an input
--
-- (Definition 2.)
out :: Input a -> Output a
out i = trOuts (inpTrans i) !! fromEnum (inpIndex i)

-- | Validity of regular transactions
--
-- (Definition 3, part 1.)
isValidRegularTransaction :: Eq a => Transaction a -> Bool
isValidRegularTransaction t = and [
      and [ 0 <= inpIndex i && fromEnum (inpIndex i) < length (trOuts (inpTrans i))
          | i <- trIns t
          ]
    -- TODO: This comparison of _transactions_ feels strange somehow
    -- (related to the introduction of transaction IDs; at the moment
    -- comparing transactions for equality is a linear operation.)
    , and [ (trIns t !! j1) /= (trIns t !! j2)
          | j1 <- [0 .. length (trIns t) - 1]
          , j2 <- [0 .. length (trIns t) - 1]
          , j1 /= j2
          ]
    , sum [ outVal o | o <- trOuts t ] == sum [ outVal (out i) | i <- trIns t ]
    ]

-- | Validity of genesis transactions
--
-- (Definition 3, part 2.)
isValidGenesisTransaction :: Eq a => Transaction a -> Bool
isValidGenesisTransaction t = and [
      null (trIns t)
    , and [ outAddr o == AddrTreasury | o <- trOuts t ]
    ]

-- | Validity of transactions
isValidTransaction :: Eq a => Transaction a -> Bool
isValidTransaction t =
    case transactionType t of
      TransRegular -> isValidRegularTransaction t
      TransGenesis -> isValidGenesisTransaction t

-- | Ledger
--
-- Side condition: all transactions must be valid.
-- Most recent transaction is the head of the list.
--
-- (Definition 4.)
type Ledger a = [Transaction a]

{-
-- | Unspent outputs in a ledger
--
-- (Definition 5.)
--
-- TODO: This is very odd. Is a set of 'Output's really sufficient?
-- Surely multiple transaction outputs can have the same 'Output'?
unspentOutputs :: forall a. Ord a => Ledger a -> Set (Output a)
unspentOutputs l = allOutputs \\ spentOutputs
  where
    allOutputs, spentOutputs :: Set (Output a)
    allOutputs   = Set.fromList $ concatMap trOuts l
    spentOutputs = Set.fromList $ concatMap (map out . trIns) l
-}

-- | Ledger validity
--
-- NOTE: This doesn't check transaction validity. See 'isValidLedger'.
--
-- (Definition 6.)
isValidLedger' :: Ord a => Ledger a -> Bool
isValidLedger' []     = True
isValidLedger' (t:l) = isValidLedger' l
                    && and [ elem (inpTrans i) l &&
                             i `inUtxo` utxoAfter l
                           | i <- trIns t
                           ]

-- | Ledger and transaction validity
isValidLedger :: Ord a => Ledger a -> Bool
isValidLedger l = all isValidTransaction l && isValidLedger' l

-- | Balance
--
-- (Definition 7.)
balance :: Ord a => Ledger a -> Address a -> Value
balance l AddrGenesis = (-1) * sum [ outVal o
                                   | t <- l
                                   , o <- trOuts t
                                   , outAddr o == AddrTreasury
                                   ]
balance l a           = sum [ outVal o
                            | o <- outputs (utxoAfter l)
                            , outAddr o == a
                            ]

{-------------------------------------------------------------------------------
  Computation of the UTxO (not present in the paper)
-------------------------------------------------------------------------------}

data Utxo a = Utxo { utxoToMap :: Map (Input a) (Output a) }
  deriving (Show)

inUtxo :: Ord a => Input a -> Utxo a -> Bool
inUtxo inp (Utxo utxo) = Map.member inp utxo

outputs :: Utxo a -> [Output a]
outputs (Utxo utxo) = Map.elems utxo

utxoToList :: Utxo a -> [(Input a, Output a)]
utxoToList = Map.toList . utxoToMap

utxoAfter :: forall a. Ord a => Ledger a -> Utxo a
utxoAfter l = Utxo $ allOutputs `withoutKeys` allSpent
  where
    allOutputs :: Map (Input a) (Output a)
    allOutputs = Map.fromList $ concatMap outputsOf l

    outputsOf :: Transaction a -> [(Input a, Output a)]
    outputsOf t = map (first (Input t)) (zip [0..] (trOuts t))

    allSpent :: Set (Input a)
    allSpent = Set.fromList $ concatMap trIns l

{-------------------------------------------------------------------------------
  Example one from the paper
-------------------------------------------------------------------------------}

example1 :: Ledger Int
example1 = [t5, t4, t3, t2, t1]
  where
    t1, t2, t3, t4, t5 :: Transaction Int
    t1 = Transaction []           [ Output  AddrTreasury    1000 ]
    t2 = Transaction [Input t1 0] [ Output (AddrOrdinary 1) 1000 ]
    t3 = Transaction [Input t2 0] [ Output (AddrOrdinary 2)  800
                                  , Output (AddrOrdinary 1)  200 ]
    t4 = Transaction [Input t3 1] [ Output (AddrOrdinary 3)  199
                                  , Output  AddrTreasury       1 ]
    t5 = Transaction [Input t4 1] [ Output (AddrOrdinary 2)    1 ]

{-------------------------------------------------------------------------------
  Additional types and functions
-------------------------------------------------------------------------------}

-- | A block of transactions
--
-- The block must be signed by the slot leader for the given slot.
data Block sid a = Block {
      blockPrev  :: Maybe (Block sid a)
    , blockSId   :: sid
    , blockTrans :: [Transaction a]
    }
  deriving (Generic, PrettyVal)

-- | A chain
--
-- A chain is just a series of blocks, here modelled simply as the transactions
-- they contain, since the rest of the block information can then be inferred.
data Chain a = Chain {
      chainBlocks :: [[Transaction a]]
    }
  deriving (Generic, PrettyVal)

chainToLedger :: Chain a -> Ledger a
chainToLedger Chain{..} = reverse $ concat chainBlocks

-- | The DSL models fees as outpus to the treasury
outputIsFee :: Output a -> Bool
outputIsFee (Output AddrTreasury _) = True
outputIsFee _otherwise = False

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable a => Buildable (Chain a) where
  build Chain{..} = bprint
      ( "Chain"
      % "{ blocks: " % listJson
      % "}"
      )
      chainBlocks

instance Buildable a => Buildable [Transaction a] where
  build = bprint listJson

instance Buildable a => Buildable (Transaction a) where
  build Transaction{..} = bprint
      ( "Transaction"
      % "{ ins: " % listJson
      % ", outs: " % listJson
      % "}"
      )
      trIns
      trOuts

instance Buildable a => Buildable (Input a) where
  build Input{..} = bprint
      ( "Input"
      % "{ trans: " % build
      % ", index: " % build
      % "}"
      )
      inpTrans
      inpIndex

instance Buildable a => Buildable (Output a) where
  build Output{..} = bprint
      ( "Output"
      % "{ addr: " % build
      % ", val:  " % build
      % "}"
      )
      outAddr
      outVal

instance Buildable a => Buildable (Address a) where
  build AddrGenesis      = "AddrGenesis"
  build AddrTreasury     = "AddrTreasury"
  build (AddrOrdinary a) = bprint ("AddrOrdinary " % build) a

instance (Buildable a, Buildable sid) => Buildable (Block sid a) where
  build Block{..} = bprint
      ( "Block"
      % "{ prev:  " % build
      % ", sid:   " % build
      % ", trans: " % build
      % "}"
      )
      blockPrev
      blockSId
      blockTrans

instance (Buildable a, Ord a) => Buildable (Utxo a) where
  build (Utxo utxo) = bprint ("Utxo " % mapJson) utxo

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Remove a set of keys from the domain of a map
--
-- TODO: This function is available out of the box from containers >= 0.5.8.
withoutKeys :: Ord k => Map k a -> Set k -> Map k a
m `withoutKeys` s = m `Map.difference` Map.fromSet (const ()) s
