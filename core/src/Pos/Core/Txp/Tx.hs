module Pos.Core.Txp.Tx
       ( Tx (..)
       , checkTx
       , txInputs
       , txOutputs
       , txAttributes
       , txF

       , TxId
       , TxAttributes

       , TxIn (..)
       , isTxInUnknown

       , TxOut (..)
       , _TxOut
       ) where

import           Universum

import           Control.Lens (makeLenses, makePrisms)
import           Control.Monad.Except (MonadError (throwError))
import qualified Data.Text.Buildable as Buildable
import           Formatting (Format, bprint, build, builder, int, sformat, (%))
import           Serokell.Util.Base16 (base16F)
import           Serokell.Util.Text (listJson)
import           Serokell.Util.Verify (VerificationRes (..), verResSingleF, verifyGeneric)

import           Pos.Binary.Class (Bi)
import           Pos.Core.Common (Address (..), Coin (..), checkCoin, coinF)
import           Pos.Crypto (Hash, hash, shortHashF)
import           Pos.Data.Attributes (Attributes, areAttributesKnown)

----------------------------------------------------------------------------
-- Tx
----------------------------------------------------------------------------

-- | Transaction.
--
-- NB: transaction witnesses are stored separately.
data Tx = UnsafeTx
    { _txInputs     :: !(NonEmpty TxIn)  -- ^ Inputs of transaction.
    , _txOutputs    :: !(NonEmpty TxOut) -- ^ Outputs of transaction.
    , _txAttributes :: !TxAttributes     -- ^ Attributes of transaction
    } deriving (Eq, Ord, Generic, Show, Typeable)

instance Hashable Tx

instance Bi Tx => Buildable Tx where
    build tx@(UnsafeTx{..}) =
        bprint
            ("Tx "%build%
             " with inputs "%listJson%", outputs: "%listJson % builder)
            (hash tx) _txInputs _txOutputs attrsBuilder
      where
        attrs = _txAttributes
        attrsBuilder | areAttributesKnown attrs = mempty
                     | otherwise = bprint (", attributes: "%build) attrs

instance NFData Tx

-- | Specialized formatter for 'Tx'.
txF :: Bi Tx => Format r (Tx -> r)
txF = build

-- | Verify inputs and outputs are non empty; have enough coins.
checkTx
    :: MonadError Text m
    => Tx
    -> m ()
checkTx it =
    case verRes of
        VerSuccess -> pure ()
        failure    -> throwError $ sformat verResSingleF failure
  where
    verRes =
        verifyGeneric $
        concat $ zipWith outputPredicates [0 ..] $ toList (_txOutputs it)
    outputPredicates (i :: Word) TxOut {..} =
        [ ( txOutValue > Coin 0
          , sformat
                ("output #"%int%" has non-positive value: "%coinF)
                i txOutValue
          )
        , ( isRight (checkCoin txOutValue)
          , sformat
                ("output #"%int%" has invalid coin")
                i
          )
        ]

--------------------------------------------------------------------------------
-- TxId
--------------------------------------------------------------------------------

-- | Represents transaction identifier as 'Hash' of 'Tx'.
type TxId = Hash Tx

--------------------------------------------------------------------------------
-- TxAttributes
--------------------------------------------------------------------------------

-- | Represents transaction attributes: map from 1-byte integer to
-- arbitrary-type value. To be used for extending transaction with new
-- fields via softfork.
type TxAttributes = Attributes ()

--------------------------------------------------------------------------------
-- TxIn
--------------------------------------------------------------------------------

-- | Transaction arbitrary input.
data TxIn
    = TxInUtxo
    { -- | Which transaction's output is used
      txInHash  :: !TxId
      -- | Index of the output in transaction's outputs
    , txInIndex :: !Word32
    }
    | TxInUnknown !Word8 !ByteString
    deriving (Eq, Ord, Generic, Show, Typeable)

instance Hashable TxIn

instance Buildable TxIn where
    build TxInUtxo {..}        = bprint ("TxInUtxo "%shortHashF%" #"%int) txInHash txInIndex
    build (TxInUnknown tag bs) = bprint ("TxInUnknown "%int%" "%base16F) tag bs

instance NFData TxIn

isTxInUnknown :: TxIn -> Bool
isTxInUnknown (TxInUnknown _ _) = True
isTxInUnknown _                 = False

--------------------------------------------------------------------------------
-- TxOut
--------------------------------------------------------------------------------

-- | Transaction output.
data TxOut = TxOut
    { txOutAddress :: !Address
    , txOutValue   :: !Coin
    } deriving (Eq, Ord, Generic, Show, Typeable)

instance Hashable TxOut

instance Buildable TxOut where
    build TxOut {..} =
        bprint ("TxOut "%coinF%" -> "%build) txOutValue txOutAddress

instance NFData TxOut

makePrisms ''TxOut

--------------------------------------------------------------------------------
-- Tx Lenses
--------------------------------------------------------------------------------

makeLenses ''Tx
