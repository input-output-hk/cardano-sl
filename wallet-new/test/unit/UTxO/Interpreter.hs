-- | Interpreter from the DSL to Cardano types
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
module UTxO.Interpreter (
    -- * Translate the DSL to Cardano types
    Interpret(..)
  , IntException(..)
  , runInterpret
    -- * Convenience re-exports
  , SlotId(..)
  ) where

import Universum
import Data.Default (def)
import Prelude (Show(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict    as Map

import Pos.Block.Logic
import Pos.Client.Txp
import Pos.Core
import Pos.Crypto
import Pos.Ssc (defaultSscPayload)
import Pos.Txp.Toil
import Pos.Util.Chrono

import UTxO.Bootstrap
import UTxO.Context
import UTxO.Crypto
import UTxO.Translate
import qualified UTxO.DSL as DSL

{-------------------------------------------------------------------------------
  Translate the DSL UTxO definitions to Cardano types

  NOTE: Delegation in Cardano is described in the document
  "Delegation and Stake Locking in Cardano SL"
  <cardano-sl-articles/delegation/pdf/article.pdf>.
-------------------------------------------------------------------------------}

newtype PrettyCallStack = PrettyCallStack CallStack

instance Show PrettyCallStack where
  show (PrettyCallStack cs) = prettyCallStack cs

-- | Interpretation error
data IntException =
    IntExNonOrdinaryAddress PrettyCallStack
  | IntExClassifyInputs Text
  | IntExMkDlg          Text
  | IntExCreateBlock    Text
  | IntExMkSlot         Text
  | IntExTx             TxError -- ^ Occurs during fee calculation
  deriving (Show)

instance Exception IntException

class Interpret a where
  type Interpreted a :: *

  int :: HasCallStack => a -> Translate IntException (Interpreted a)

-- | Specialization of 'runTranslate' (just to fix the types)
runInterpret :: Translate IntException a -> a
runInterpret = runTranslate

instance Interpret Addr where
  type Interpreted Addr = (SomeKeyPair, Address)

  int :: Addr -> Translate IntException (SomeKeyPair, Address)
  int = asks . resolveAddr

instance Interpret a => Interpret (DSL.Address a) where
  type Interpreted (DSL.Address a) = Interpreted a

  int :: HasCallStack => DSL.Address a -> Translate IntException (Interpreted a)
  int (DSL.AddrOrdinary addr) = int addr
  int _ = throwError $ IntExNonOrdinaryAddress (PrettyCallStack ?callStack)

instance Interpret (DSL.Input Addr) where
  type Interpreted (DSL.Input Addr) = TxOwnedInput SomeKeyPair

  int :: HasCallStack
      => DSL.Input Addr -> Translate IntException (TxOwnedInput SomeKeyPair)
  int inp@(DSL.Input t _ix) | isBootstrapTransaction t = do
      (ownerKey, ownerAddr) <- int $ DSL.outAddr (DSL.out inp)
      -- See explanation at 'bootstrapTransaction'
      return (
            ownerKey
          , TxInUtxo {
                txInHash  = unsafeHash ownerAddr
              , txInIndex = 0
              }
          )
  int inp@DSL.Input{..} = do
      -- We figure out who must sign the input by looking at the output
      (ownerKey, _) <- int $ DSL.outAddr (DSL.out inp)
      inpTrans'     <- (hash . taTx) <$> int inpTrans
      return (
            ownerKey
          , TxInUtxo {
                txInHash  = inpTrans'
              , txInIndex = inpIndex
              }
          )

instance Interpret (DSL.Output Addr) where
  type Interpreted (DSL.Output Addr) = TxOutAux

  int :: HasCallStack => DSL.Output Addr -> Translate IntException TxOutAux
  int DSL.Output{..} = do
      (_, outAddr') <- int outAddr
      return TxOutAux {
          toaOut = TxOut {
              txOutAddress = outAddr'
            , txOutValue   = mkCoin outVal
            }
        }

-- | Interpretation of transactions
instance Interpret (DSL.Transaction Addr) where
  type Interpreted (DSL.Transaction Addr) = TxAux

  int :: HasCallStack => DSL.Transaction Addr -> Translate IntException TxAux
  int DSL.Transaction{..} = do
      trIns'  <- mapM int $ trIns
      trOuts' <- mapM int $ filter (not . DSL.outputIsFee) trOuts
      case classifyInputs trIns' of
        Left err ->
          throwError (IntExClassifyInputs err)
        Right (InputsRegular trIns'') -> withConfig $ return $
          makeMPubKeyTx
            (FakeSigner . regKpSec)
            (NE.fromList trIns'')
            (NE.fromList trOuts')
        Right (InputsRedeem (kp, inp)) -> withConfig $ return $
          makeRedemptionTx
            (redKpSec kp)
            (NE.fromList [inp])
            (NE.fromList trOuts')

-- | Interpretation of a block
--
-- NOTE:
--
-- * We don't test the shared seed computation
-- * We stay within a single epoch for now
-- * We use the genesis block from the test configuration
--   (which has implications for which slot leaders etc we have)
instance Interpret (DSL.Block SlotId Addr) where
  type Interpreted (DSL.Block SlotId Addr) = MainBlock

  int :: HasCallStack
      => DSL.Block SlotId Addr -> Translate IntException MainBlock
  int DSL.Block{..} = do
      blockTrans' <- mapM int blockTrans

      -- empty delegation payload
      dlgPayload <- mapTranslateErrors IntExMkDlg $
                      withConfig $ mkDlgPayload []

      -- empty update payload
      let updPayload = def

      -- previous block header
      -- if none specified, use genesis block
      prev <-
        case blockPrev of
          Just block -> (Right . view gbHeader) <$> int block
          Nothing    -> (Left  . view gbHeader) <$> asks (ccBlock0 . tcCardano)

      -- figure out who needs to sign the block
      BlockSignInfo{..} <- asks $ blockSignInfoForSlot blockSId

      mapTranslateErrors IntExCreateBlock $
        withConfig $ createMainBlockPure
          blockSizeLimit
          prev
          (Just (bsiPSK, bsiLeader))
          blockSId
          bsiKey
          (RawPayload
              blockTrans'
              (defaultSscPayload (siSlot blockSId))
              dlgPayload
              updPayload
            )
    where
      blockSizeLimit = 1 * 1024 * 1024 -- 1 MB

instance Interpret (DSL.Chain Addr) where
  type Interpreted (DSL.Chain Addr) = OldestFirst NE Block

  int :: HasCallStack
      => DSL.Chain Addr -> Translate IntException (OldestFirst NE Block)
  int DSL.Chain{..} = do
      blocks <- (OldestFirst . NE.fromList) <$> mkBlocks Nothing 0 chainBlocks
      mapM (liftM Right . int) blocks
    where
      -- TODO: Here (and elsewhere) we assume we stay within the first epoch
      mkBlocks :: Maybe (DSL.Block SlotId Addr)
               -> Word16
               -> [[DSL.Transaction Addr]]
               -> Translate IntException [DSL.Block SlotId Addr]
      mkBlocks _    _    []       = return []
      mkBlocks prev slot (ts:tss) = do
          lsi <- mapTranslateErrors IntExMkSlot $
                   withConfig $ mkLocalSlotIndex slot
          let block = DSL.Block {
                          blockPrev  = prev
                        , blockTrans = ts
                        , blockSId   = SlotId {
                              siEpoch = EpochIndex 0
                            , siSlot  = lsi
                            }
                        }
          (block :) <$> mkBlocks (Just block) (slot + 1) tss

-- | This filters out any payments to the treasury
instance Interpret (DSL.Utxo Addr) where
  type Interpreted (DSL.Utxo Addr) = Utxo

  int :: HasCallStack => DSL.Utxo Addr -> Translate IntException Utxo
  int = fmap Map.fromList
      . mapM aux
      . filter (not . DSL.outputIsFee . snd)
      . DSL.utxoToList
    where
      aux :: (DSL.Input Addr, DSL.Output Addr)
          -> Translate IntException (TxIn, TxOutAux)
      aux (inp, out) = do
          (_key, inp') <- int inp
          out'         <- int out
          return (inp', out')
