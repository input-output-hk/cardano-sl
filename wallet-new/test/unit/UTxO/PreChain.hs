{-# LANGUAGE TupleSections #-}

-- | Fee calculation
--
-- See also
--
-- * Module "Pos.Core.Common.Fee"
-- * Function 'Pos.Client.Txp.Util.stabilizeTxFee'
module UTxO.PreChain (
    PreChain
  , preChain
  , FromPreChain(..)
  , fromPreChain
    -- * PreTree
  , BlockTree
  , PreTree
  , preTree
  , FromPreTree(..)
  , fromPreTree
  ) where

import           Universum

import qualified Data.Set as Set
import           Data.Tree (Tree)
import qualified Data.Tree as Tree
import           Pos.Client.Txp
import           Pos.Core
import           Pos.Txp.Toil
import           Pos.Util.Chrono

import           Cardano.Wallet.Kernel.Types (rawResolvedTx)

import           Util.DepIndep
import           UTxO.Bootstrap
import           UTxO.Context
import           UTxO.DSL
import           UTxO.Interpreter
import           UTxO.Translate

{-------------------------------------------------------------------------------
  Chain with some information still missing
-------------------------------------------------------------------------------}

-- | A chain with "holes" for the bootstrap transaction and the fees
--
-- We use the 'DepIndep' monad transformer here to make sure that the
-- effects can depend on the bootstrap transaction, but not on the fees.
-- This is important. Suppose we used
--
-- > Transaction h Addr -> [[Fee]] -> m (Blocks h Addr)
--
-- instead: we'd have to execute the action that generates the blocks
-- /twice/ (once before we know the fees, and once again after we computed
-- the fees), and the second time around we may in fact generate a very
-- different chain (think @m@ = QuickCheck 'Gen', for instance) -- which
-- would then require very different fees. No good. If we did
--
-- > m (Transaction h Addr -> [[Fee]] -> Blocks h Addr)
--
-- instead, then (thinking @m@ = 'Gen' again) we could not generate different
-- chains for different bootstrap transactions, which would also be no good.
--
-- It is still the responsibility of the 'PreChain' author to make sure that the
-- structure of the blockchain does not depend on the fees that are passed.
type PreChain h m a = DepIndep (Transaction h Addr) [[Fee]] m (Chain h Addr, a)

preChain :: Functor m
         => (Transaction h Addr -> m ([[Fee]] -> Chain h Addr))
         -> PreChain h m ()
preChain = fmap (, ()) . DepIndep

-- | Result of translating a 'PreChain'
--
-- See 'fromPreChain'
data FromPreChain h a = FromPreChain {
      -- | The boot transaction that was used in the translation
      fpcBoot   :: Transaction h Addr

      -- | The resulting chain (i.e., list of list of transactions)
      -- This does /not/ include the bootstrap transaction.
    , fpcChain  :: Chain h Addr

      -- | The resulting ledger (i.e., flat list of transactions)
      -- This /does/ include the bootstrap transaction .
    , fpcLedger :: Ledger h Addr

      -- | Any additional information that was included in the 'PreChain'.
    , fpcExtra  :: a
    }

fromPreChain :: (Hash h Addr, Monad m)
             => PreChain h m a -> TranslateT IntException m (FromPreChain h a)
fromPreChain pc = do
    fpcBoot <- asks bootstrapTransaction
    (txs, fpcExtra) <- calcChainFees fpcBoot =<< lift (runDepIndep pc fpcBoot)
    let fpcChain  = txs -- doesn't include the boot transactions
        fpcLedger = chainToLedger fpcBoot fpcChain
    return FromPreChain{..}

{-------------------------------------------------------------------------------
  Tree with some information still missing.
-------------------------------------------------------------------------------}

-- | Blocks organised in a tree structure.
type BlockTree h a = OldestFirst Tree (UTxO.DSL.Block h a)

-- | Generalisation of a 'PreChain'.
--   A 'PreChain' is the flattening of a 'PreTree' with a single branch.
type PreTree h m a = DepIndep (Transaction h Addr) (Tree [Fee]) m (BlockTree h Addr, a)

preTree :: Functor m
        => (Transaction h Addr -> m ((Tree [Fee]) -> BlockTree h Addr))
        -> PreTree h m ()
preTree = fmap (, ()) . DepIndep

-- | Result of translating a 'PreTree'
--
-- See 'fromPreTree'
-- Compared to 'FromPreChain':
-- - we do not calculate a ledger, since we have not yet fixed on a single chain.
--   Instead we include the list of all addresses in the tree. Note that this may
--   include addresses which have no transactions to/from them in the final
--   blockchain, because they were in blocks which were rolled back.
-- - the boot transaction is included in the chain.
data FromPreTree h a = FromPreTree {
      -- | The resulting tree
      fptTree      :: !(BlockTree h Addr)

      -- | The boot transaction
    , fptBoot      :: !(Transaction h Addr)

      -- | Addresses involved in this block tree
    , fptAddresses :: !(Set Addr)

      -- | Any additional information that was included in the 'PreChain'.
    , fptExtra     :: !a
    }

fromPreTree :: (Hash h Addr, Monad m)
            => PreTree h m a
            -> TranslateT IntException m (FromPreTree h a)
fromPreTree pc = do
    fptBoot <- asks bootstrapTransaction
    (fptTree, fptExtra) <- calcTreeFees fptBoot =<< lift (runDepIndep pc fptBoot)
    let fptAddresses = Set.fromList
          . map outAddr
          . concatMap trOuts
          . concatMap toList
          . Tree.flatten
          $ getOldestFirst fptTree
    return FromPreTree{..}

{-------------------------------------------------------------------------------
  Fee calculation
-------------------------------------------------------------------------------}

type Fee = Value

-- | Calculate fees for a bunch of transactions
--
-- Transaction fee calculation is a bit awkward.
--
-- First, when we want to construct a number of transactions, then it may not be
-- sufficient to only know the fee for each transaction locally. For example, if
-- we want to transfer X from A to B, and then the remainder of A's balance to
-- C, we need to transactions with outputs
--
-- >  [ X from A to B, balanceA - X - fee1 from A back to A ]
-- >  [ balanceA - X - fee1 - fee2 from A to C ]
--
-- where we need to know the fee of the first transaction in order to create
-- the second.
--
-- Second, in order to be able to even know what the fee of a transaction is,
-- we need to /construct/ the transaction since the fee depends on the
-- transaction size.
--
-- So, what we do is we first construct all transactions assuming the fee is 0.
-- We then calculate the fees, and finally construct the transactions again
-- with their proper fees.
--
-- The function constructing the transactions from the fees must satisfy
-- two conditions:
--
-- * The transactions returned cannot depend on the fees! Basically, the
--   transactions should regard these fees as uninspectable. If this condition
--   is not met the fees calculated will be incorrect. (Possibly we might
--   be able to address this at the type level with some PHOAS like
--   representation.)
--
-- * The function can assume that the list fees it is given contains a fee
--   for each transaction, in order, but it the list may be longer. The reason
--   is that initially we cannot even know how many transactions the function
--   returns, and hence we just provide an infinite list of zeroes.
--   (We could address this at the type level by using vectors.)
--
-- TODO: We should check that the fees of the constructed transactions match the
-- fees we calculuated. This ought to be true at the moment, but may break when
-- the size of the fee might change the size of the the transaction.
calcFees :: forall h m x t blocks
            . ( Hash h Addr, Monad m, Traversable t
              , blocks ~ OldestFirst t (UTxO.DSL.Block h Addr)
              )
         => Transaction h Addr
         -> (t [Fee] -> (blocks, x))
         -> t [Fee] -- ^ Initial fees
         -> ( Transaction h Addr
              -> blocks
              -> TranslateT IntException m (OldestFirst t (OldestFirst [] TxAux))
            )
         -> TranslateT IntException m (OldestFirst t (UTxO.DSL.Block h Addr), x)
calcFees boot f initialFees buildFees = do
    TxFeePolicyTxSizeLinear policy <- bvdTxFeePolicy <$> gsAdoptedBVData
    let txToLinearFee' :: TxAux -> TranslateT IntException m Value
        txToLinearFee' = mapTranslateErrors IntExTx
                       . fmap feeValue
                       . txToLinearFee policy

    txs <- buildFees boot $ fst (f initialFees)
    fees     <- mapM (mapM txToLinearFee') txs
    return $ f (unmarkOldestFirst fees)
  where
    int' :: Chain h Addr -> IntT h Void m (OldestFirst [] (OldestFirst [] TxAux))
    int' = mapM (mapM (fmap rawResolvedTx . int))

    unmarkOldestFirst :: OldestFirst t (OldestFirst [] a) -> t [a]
    unmarkOldestFirst = fmap toList . getOldestFirst

    feeValue :: TxFee -> Value
    feeValue (TxFee fee) = unsafeGetCoin fee

calcChainFees :: forall h m x . (Hash h Addr, Monad m)
              => Transaction h Addr
              -> ([[Fee]] -> (Blocks h Addr, x))
              -> TranslateT IntException m (Blocks h Addr, x)
calcChainFees boot f = calcFees boot f (repeat (repeat 0)) buildFees
  where
    buildFees boot' blocks = fst <$> runIntBoot boot' blocks

calcTreeFees  :: forall h m x . (Hash h Addr, Monad m)
              => Transaction h Addr
              -> (Tree [Fee] -> (OldestFirst Tree (UTxO.DSL.Block h Addr), x))
              -> TranslateT IntException m (OldestFirst Tree (UTxO.DSL.Block h Addr), x)
calcTreeFees boot f = calcFees boot f zeroTree buildFees
  where
    -- When building fees for a tree the usual State monad approach falls over,
    -- since the state needs to fork. Rather than rewriting this interpretation
    -- code, we cheat and unroll the state at each level of the tree, in order
    -- to fork it.
    buildFees boot' (OldestFirst tree) = OldestFirst
      <$> Tree.unfoldTreeM go (tree, initIntCtxt boot')
    go (Tree.Node val children, intCtxt) = do
      (val', intCtxt') <- runIntT intCtxt val
      return (val', fmap (, intCtxt') children)
    -- An infinite tree of zero fees
    zeroTree = Tree.unfoldTree (\_ -> (repeat 0, repeat ())) ()
