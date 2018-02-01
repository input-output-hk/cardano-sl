-- | Fee calculation
--
-- See also
--
-- * Module "Pos.Core.Common.Fee"
-- * Function 'Pos.Client.Txp.Util.stabilizeTxFee'
module UTxO.PreChain (
    PreChain(..)
  , FromPreChain(..)
  , fromPreChain
  ) where

import Universum

import Pos.Core
import Pos.Client.Txp
import Pos.Txp.Toil
import Pos.Util.Chrono

import UTxO.Bootstrap
import UTxO.Context
import UTxO.DSL
import UTxO.Interpreter
import UTxO.Translate

{-------------------------------------------------------------------------------
  Chain with some information still missing
-------------------------------------------------------------------------------}

newtype PreChain h = PreChain (h (Transaction h Addr) -> [[Fee]] -> Blocks h Addr)

data FromPreChain h = FromPreChain {
      fpcBoot   :: Transaction h Addr
    , fpcChain  :: Chain       h Addr
    , fpcLedger :: Ledger      h Addr
    }

fromPreChain :: Hash h Addr
             => PreChain h -> Translate IntException (FromPreChain h)
fromPreChain (PreChain f) = do
    fpcBoot <- asks bootstrapTransaction
    txs <- calcFees fpcBoot (f (hash fpcBoot))
    let fpcChain  = Chain txs -- doesn't include the boot transactions
        fpcLedger = chainToLedger fpcBoot fpcChain
    return FromPreChain{..}

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
calcFees :: Hash h Addr
         => Transaction h Addr
         -> ([[Fee]] -> Blocks h Addr)
         -> Translate IntException (Blocks h Addr)
calcFees boot f = do
    TxFeePolicyTxSizeLinear policy <- bvdTxFeePolicy <$> gsAdoptedBVData
    let txToLinearFee' :: TxAux -> Translate IntException Value
        txToLinearFee' = mapTranslateErrors IntExTx
                       . fmap feeValue
                       . txToLinearFee policy

    (txs, _) <- runIntBoot boot $ f (repeat [0..])
    fees     <- mapM (mapM txToLinearFee') txs
    return $ f (unmarkOldestFirst fees)
  where
    unmarkOldestFirst :: OldestFirst [] (OldestFirst [] a) -> [[a]]
    unmarkOldestFirst = map toList . toList

    feeValue :: TxFee -> Value
    feeValue (TxFee fee) = unsafeGetCoin fee
