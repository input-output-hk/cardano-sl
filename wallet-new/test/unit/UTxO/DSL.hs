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
    -- * Additional types
  , Block(..)
  , Chain(..)
    -- * Convenience re-exports
  , dumpStr
  ) where

import Universum
import Data.List ((!!))
import Data.Set ((\\))
import Text.Show.Pretty (PrettyVal, dumpStr)
import qualified Data.Set as Set

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
    if length (trIns t) > 0
      then TransRegular
      else TransGenesis

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
      length (trIns t) == 0
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

-- | Unspent outputs in a ledger
--
-- (Definition 5.)
unspentOutputs :: forall a. Ord a => Ledger a -> Set (Output a)
unspentOutputs l = allOutputs \\ spentOutputs
  where
    allOutputs, spentOutputs :: Set (Output a)
    allOutputs   = Set.fromList $ concatMap trOuts l
    spentOutputs = Set.fromList $ concatMap (map out . trIns) l

-- | Ledger validity
--
-- TODO: This doesn't check transaction validity. The paper takes this as
-- implied in the definition of a ledger -- should we check?
--
-- (Definition 6.)
isValidLedger :: Ord a => Ledger a -> Bool
isValidLedger []     = True
isValidLedger (t:l) = isValidLedger l
                   && and [ elem (inpTrans i) l &&
                            out i `Set.member` unspentOutputs l
                          | i <- trIns t
                          ]

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
                            | o <- Set.toList (unspentOutputs l)
                            , outAddr o == a
                            ]

{-------------------------------------------------------------------------------
  Additional types
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
