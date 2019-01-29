{-# LANGUAGE RecordWildCards #-}

module Pos.Chain.Txp.Tx
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

       , TxValidationRules (..)
       , TxValidationRulesConfig (..)
       , mkLiveTxValidationRules
       ) where

import           Universum

import           Control.Lens (makeLenses, makePrisms)
import           Control.Monad.Except (MonadError (throwError))
import           Data.Aeson (FromJSON (..), FromJSONKey (..),
                     FromJSONKeyFunction (..), ToJSON (toJSON), ToJSONKey (..),
                     object, withObject, (.:), (.=))
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.Aeson.Types (toJSONKeyText)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as NE
import           Data.SafeCopy (base, deriveSafeCopySimple)
import qualified Data.Text as T
import           Formatting (Format, bprint, build, builder, int, sformat, (%))
import qualified Formatting.Buildable as Buildable
import           Numeric.Natural (Natural)
import qualified Serokell.Util.Base16 as B16
import           Serokell.Util.Text (listJson)
import           Serokell.Util.Verify (VerificationRes (..), verResSingleF,
                     verifyGeneric)

import           Pos.Binary.Class (Bi (..), Case (..), Cons (..), Field (..),
                     decodeKnownCborDataItem, decodeUnknownCborDataItem,
                     deriveSimpleBi, encodeKnownCborDataItem, encodeListLen,
                     encodeUnknownCborDataItem, enforceSize,
                     knownCborDataItemSizeExpr, szCases)
import           Pos.Core.Attributes (Attributes, areAttributesKnown,
                     unknownAttributesLength)
import           Pos.Core.Common (Address (..), Coin (..), checkCoin, coinF,
                     coinToInteger, decodeTextAddress, integerToCoin)
import           Pos.Core.Slotting (EpochIndex)
import           Pos.Core.Util.LogSafe (SecureLog (..))
import           Pos.Crypto (Hash, decodeAbstractHash, hash, hashHexF,
                     shortHashF)
import           Pos.Util.Util (toAesonError)

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

instance Buildable Tx where
    build tx@(UnsafeTx{..}) =
        bprint
            ("Tx "%build%
             " with inputs "%listJson%", outputs: "%listJson % builder)
            (hash tx) _txInputs _txOutputs attrsBuilder
      where
        attrs = _txAttributes
        attrsBuilder | areAttributesKnown attrs = mempty
                     | otherwise = bprint (", attributes: "%build) attrs

instance Bi Tx where
    encode tx = encodeListLen 3
                <> encode (_txInputs tx)
                <> encode (_txOutputs tx)
                <> encode (_txAttributes tx)
    decode = do
        enforceSize "Tx" 3
        UnsafeTx <$> decode <*> decode <*> decode

    encodedSizeExpr size pxy = 1
        + size (_txInputs     <$> pxy)
        + size (_txOutputs    <$> pxy)
        + size (_txAttributes <$> pxy)

instance NFData Tx

-- | Specialized formatter for 'Tx'.
txF :: Format r (Tx -> r)
txF = build

-- | Verify inputs and outputs are non empty; have enough coins.
-- HLint suggested to use (||) instead of if then else. The suggestion
-- was significantly less readable/intuitive.
{-# ANN checkTx ("HLint: ignore" :: Text) #-}
checkTx
    :: MonadError Text m
    => TxValidationRules
    -> Tx
    -> m ()
checkTx txValRules it =
    case verRes of
        VerSuccess -> pure ()
        failure    -> throwError $ verResSingleF failure
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
        -- The following rules check to see if we have passed a "cutoffEpoch"
        -- after which we reject transactions larger than a size specified
        -- in the `configuration.yaml` via the `TxValidationRules` struct.
        , ( if currentEpoch > cutoffEpoch
                then (fromIntegral $ tvrTxAttrSize txValRules)
                      > unknownAttributesLength (_txAttributes it)
                else True
          , sformat
                ("size of Tx unknown attributes in input #"%int%" is too large")
                i
          )
        , ( if currentEpoch > cutoffEpoch
                then all ( < (fromIntegral $ tvrAddrAttrSize txValRules))
                         (map unknownAttributesLength txOutAddrAttribs)
                else True
          , sformat
                ("size of Address unknown attributes in input #"%int%" is too large")
                i
          )
        ]
    currentEpoch = tvrCurrentEpoch txValRules
    cutoffEpoch = tvrAddrAttrCutoff txValRules
    txOutAddresses = map txOutAddress (NE.toList (_txOutputs it))
    txOutAddrAttribs = map addrAttributes txOutAddresses

-- | Because there is no limit on the size of Attributes
-- (which allows unecessary bloating of the blockchain)
-- this struct introduces limits configurable via the
-- `configuration.yaml` file which are activated at
-- the `tvrAddrAttrCutoff` epoch.

data TxValidationRules = TxValidationRules
    { tvrAddrAttrCutoff :: !EpochIndex
    , tvrCurrentEpoch   :: !EpochIndex
    , tvrAddrAttrSize   :: !Natural
    , tvrTxAttrSize     :: !Natural
    } deriving (Eq, Generic, Show)

-- This second datatype goes into the `Configuration` config and is
-- read from disk (and thus doesn't know what the current epoch is).
-- The above `TxValidationRules` is has the current epoch inserted
-- and is passed down the validation call graph.
data TxValidationRulesConfig = TxValidationRulesConfig
    { tvrcAddrAttrCutoff :: !EpochIndex
    , tvrcAddrAttrSize   :: !Natural
    , tvrcTxAttrSize     :: !Natural
    } deriving (Eq, Generic, Show)

instance FromJSON TxValidationRulesConfig where
    parseJSON = withObject "txValidationRules" $ \v -> TxValidationRulesConfig
        <$> v .: "attribResrictEpoch"
        <*> v .: "addrAttribSize"
        <*> v .: "txAttribSize"

instance ToJSON TxValidationRulesConfig where
    toJSON (TxValidationRulesConfig aaCutoff aaSize taSize) =
        object [ "attribResrictEpoch" .= aaCutoff
               , "addrAttribSize" .= aaSize
               , "txAttribSize" .= taSize
               ]

mkLiveTxValidationRules :: EpochIndex -> TxValidationRulesConfig -> TxValidationRules
mkLiveTxValidationRules currentEpoch tvrc =
    TxValidationRules { tvrAddrAttrCutoff = tvrcAddrAttrCutoff tvrc
                      , tvrCurrentEpoch = currentEpoch
                      , tvrAddrAttrSize = tvrcAddrAttrSize tvrc
                      , tvrTxAttrSize = tvrcTxAttrSize tvrc
                      }

--------------------------------------------------------------------------------
-- TxId
--------------------------------------------------------------------------------

-- | Represents transaction identifier as 'Hash' of 'Tx'.
type TxId = Hash Tx

instance Buildable (SecureLog TxId) where
    build _ = "<txid>"

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
      -- | TxId = Which transaction's output is used
      -- | Word32 = Index of the output in transaction's outputs
    = TxInUtxo TxId Word32
    | TxInUnknown !Word8 !ByteString
    deriving (Eq, Ord, Generic, Show, Typeable)

instance FromJSON TxIn where
    parseJSON v = toAesonError =<< txInFromText <$> parseJSON v

instance ToJSON TxIn where
    toJSON = toJSON . txInToText

instance FromJSONKey TxIn where
    fromJSONKey = FromJSONKeyTextParser (toAesonError . txInFromText)

instance ToJSONKey TxIn where
    toJSONKey = toJSONKeyText txInToText

instance Hashable TxIn

instance Buildable TxIn where
    build (TxInUtxo txInHash txInIndex) =
        bprint ("TxInUtxo "%shortHashF%" #"%int) txInHash txInIndex
    build (TxInUnknown tag bs) =
        bprint ("TxInUnknown "%int%" "%B16.base16F) tag bs

instance Bi TxIn where
    encode (TxInUtxo txInHash txInIndex) =
        encodeListLen 2 <>
        encode (0 :: Word8) <>
        encodeKnownCborDataItem (txInHash, txInIndex)
    encode (TxInUnknown tag bs) =
        encodeListLen 2 <>
        encode tag <>
        encodeUnknownCborDataItem (LBS.fromStrict bs)
    decode = do
        enforceSize "TxIn" 2
        tag <- decode @Word8
        case tag of
            0 -> uncurry TxInUtxo <$> decodeKnownCborDataItem
            _ -> TxInUnknown tag  <$> decodeUnknownCborDataItem
    encodedSizeExpr size _ = 2 + (knownCborDataItemSizeExpr $
        szCases [ let TxInUtxo txInHash txInIndex = error "unused"
                  in  Case "TxInUtxo" (size ((,) <$> pure txInHash <*> pure txInIndex))
                ])

instance NFData TxIn

isTxInUnknown :: TxIn -> Bool
isTxInUnknown (TxInUnknown _ _) = True
isTxInUnknown _                 = False

txInFromText :: Text -> Either Text TxIn
txInFromText t = case T.splitOn "_" t of
    ["TxInUtxo", h, idx]     -> TxInUtxo <$> decodeAbstractHash h <*> readEither idx
    ["TxInUnknown", tag, bs] -> TxInUnknown <$> readEither tag <*> B16.decode bs
    _                        -> Left $ "Invalid TxIn " <> t

txInToText :: TxIn -> Text
txInToText (TxInUtxo txInHash txInIndex) =
    sformat ("TxInUtxo_"%hashHexF%"_"%int) txInHash txInIndex
txInToText (TxInUnknown tag bs) =
    sformat ("TxInUnknown_"%int%"_"%B16.base16F) tag bs

--------------------------------------------------------------------------------
-- TxOut
--------------------------------------------------------------------------------

-- | Transaction output.
data TxOut = TxOut
    { txOutAddress :: !Address
    , txOutValue   :: !Coin
    } deriving (Eq, Ord, Generic, Show, Typeable)

instance FromJSON TxOut where
    parseJSON = withObject "TxOut" $ \o -> do
        txOutValue   <- toAesonError . integerToCoin =<< o .: "coin"
        txOutAddress <- toAesonError . decodeTextAddress =<< o .: "address"
        return $ TxOut {..}

instance ToJSON TxOut where
    toJSON TxOut{..} = object [
        "coin"    .= coinToInteger txOutValue,
        "address" .= sformat build txOutAddress ]

instance Hashable TxOut

instance Buildable TxOut where
    build TxOut {..} =
        bprint ("TxOut "%coinF%" -> "%build) txOutValue txOutAddress

instance NFData TxOut

makePrisms ''TxOut

makeLenses ''Tx

deriveSimpleBi ''TxOut [
    Cons 'TxOut [
        Field [| txOutAddress :: Address |],
        Field [| txOutValue   :: Coin    |]
    ]]

deriveSafeCopySimple 0 'base ''TxIn
deriveSafeCopySimple 0 'base ''TxOut
deriveSafeCopySimple 0 'base ''Tx

deriveJSON defaultOptions ''Tx
