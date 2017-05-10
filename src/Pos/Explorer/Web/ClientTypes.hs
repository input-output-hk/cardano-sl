{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- | Types for using in purescript-bridge

module Pos.Explorer.Web.ClientTypes
       ( CHash (..)
       , CAddress (..)
       , CTxId (..)
       , CBlockEntry (..)
       , CTxEntry (..)
       , CBlockSummary (..)
       , CAddressType (..)
       , CAddressSummary (..)
       , CTxBrief (..)
       , CNetworkAddress (..)
       , CTxSummary (..)
       , TxInternal (..)
       , CCoin
       , EpochIndex (..)
       , LocalSlotIndex (..)
       , mkCCoin
       , toCHash
       , fromCHash
       , toCAddress
       , fromCAddress
       , toCTxId
       , fromCTxId
       , toBlockEntry
       , toTxEntry
       , toBlockSummary
       , toTxBrief
       , toPosixTime
       , convertTxOutputs
       , tiToTxEntry
       ) where

import           Control.Arrow          ((&&&))
import           Control.Lens           (_Left)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.List.NonEmpty     as NE
import           Data.Time.Clock.POSIX  (POSIXTime)
import           Formatting             (sformat)
import           Prelude                ()
import           Serokell.Util.Base16   as SB16
import           Servant.API            (FromHttpApiData (..))
import           Universum

import qualified Pos.Binary             as Bi
import           Pos.Crypto             (Hash, hash)
import           Pos.DB                 (MonadDB (..))
import qualified Pos.DB.GState          as GS
import           Pos.Explorer           (TxExtra (..))
import           Pos.Merkle             (getMerkleRoot, mtRoot)
import           Pos.Slotting           (MonadSlots (..), getSlotStart)
import           Pos.Ssc.Class          (SscHelpersClass)
import           Pos.Txp                (Tx (..), TxId, TxOut (..), TxOutAux (..),
                                         _txOutputs)
import           Pos.Types              (Address, Coin, EpochIndex, LocalSlotIndex,
                                         MainBlock, SlotId (..), Timestamp, addressF,
                                         blockSlot, blockTxs, coinToInteger,
                                         decodeTextAddress, gbHeader, gbhConsensus,
                                         getEpochIndex, getSlotIndex, headerHash, mcdSlot,
                                         mkCoin, prevBlockL, sumCoins, unsafeAddCoin,
                                         unsafeGetCoin, unsafeIntegerToCoin)

-------------------------------------------------------------------------------------
-- Hash types
-------------------------------------------------------------------------------------

-- | Client hash
newtype CHash = CHash Text
    deriving (Show, Eq, Generic, Buildable, Hashable)

-- | Client address
newtype CAddress = CAddress Text
    deriving (Show, Eq, Generic, Buildable, Hashable)

-- | Client transaction id
newtype CTxId = CTxId CHash
    deriving (Show, Eq, Generic, Buildable, Hashable)

-- | Transformation of core hash-types to client representations and vice versa
encodeHashHex :: Hash a -> Text
encodeHashHex = decodeUtf8 . B16.encode . Bi.encodeStrict

decodeHashHex :: Text -> Either Text (Hash a)
decodeHashHex hashText = do
    hashBinary <- SB16.decode hashText
    over _Left toText $ Bi.decodeFull $ BSL.fromStrict hashBinary

toCHash :: Hash a -> CHash
toCHash = CHash . encodeHashHex

fromCHash :: CHash -> Either Text (Hash a)
fromCHash (CHash h) = decodeHashHex h

toCAddress :: Address -> CAddress
toCAddress = CAddress . sformat addressF

fromCAddress :: CAddress -> Either Text Address
fromCAddress (CAddress addr) = decodeTextAddress addr

toCTxId :: TxId -> CTxId
toCTxId = CTxId . toCHash

fromCTxId :: CTxId -> Either Text TxId
fromCTxId (CTxId (CHash txId)) = decodeHashHex txId

-------------------------------------------------------------------------------------
-- Composite types
-------------------------------------------------------------------------------------


newtype CCoin = CCoin
    { getCoin :: Text
    } deriving (Show, Generic)

mkCCoin :: Coin -> CCoin
mkCCoin = CCoin . show . unsafeGetCoin

-- | List of block entries is returned from "get latest N blocks" endpoint
data CBlockEntry = CBlockEntry
    { cbeEpoch      :: !Word64
    , cbeSlot       :: !Word16
    , cbeBlkHash    :: !CHash
    , cbeTimeIssued :: !(Maybe POSIXTime)
    , cbeTxNum      :: !Word
    , cbeTotalSent  :: !CCoin
    , cbeSize       :: !Word64
    , cbeRelayedBy  :: !(Maybe Text)
    } deriving (Show, Generic)

toPosixTime :: Timestamp -> POSIXTime
toPosixTime = (/ 1e6) . fromIntegral

toBlockEntry
    :: (SscHelpersClass ssc, MonadSlots m, MonadThrow m)
    => MainBlock ssc
    -> m CBlockEntry
toBlockEntry blk = do
    blkSlotStart <- getSlotStart (blk ^. gbHeader . gbhConsensus . mcdSlot)
    let headerSlot    = blk ^. blockSlot
        cbeEpoch      = getEpochIndex $ siEpoch headerSlot
        cbeSlot       = getSlotIndex  $ siSlot  headerSlot
        cbeBlkHash    = toCHash $ headerHash blk
        cbeTimeIssued = toPosixTime <$> blkSlotStart
        txs           = blk ^. blockTxs
        cbeTxNum      = fromIntegral $ length txs
        addCoins c    = unsafeAddCoin c . totalTxMoney
        cbeTotalSent  = mkCCoin $ foldl' addCoins (mkCoin 0) txs
        -- TODO: is there a way to get it more efficiently?
        cbeSize       = fromIntegral . BSL.length $ Bi.encode blk
        cbeRelayedBy  = Nothing
    return CBlockEntry {..}


-- | List of tx entries is returned from "get latest N transactions" endpoint
data CTxEntry = CTxEntry
    { cteId         :: !CTxId
    , cteTimeIssued :: !POSIXTime
    , cteAmount     :: !CCoin
    } deriving (Show, Generic)

totalTxMoney :: Tx -> Coin
totalTxMoney = unsafeIntegerToCoin . sumCoins .
               map txOutValue . _txOutputs

toTxEntry :: Timestamp -> Tx -> CTxEntry
toTxEntry ts tx = CTxEntry {..}
  where cteId = toCTxId $ hash tx
        cteTimeIssued = toPosixTime ts
        cteAmount = mkCCoin $ totalTxMoney tx

-- | Data displayed on block summary page
data CBlockSummary = CBlockSummary
    { cbsEntry      :: !CBlockEntry
    , cbsPrevHash   :: !CHash
    , cbsNextHash   :: !(Maybe CHash)
    , cbsMerkleRoot :: !CHash
    } deriving (Show, Generic)

toBlockSummary
    :: (SscHelpersClass ssc, MonadSlots m, MonadDB m)
    => MainBlock ssc
    -> m CBlockSummary
toBlockSummary blk = do
    cbsEntry <- toBlockEntry blk
    cbsNextHash <- fmap toCHash <$> GS.resolveForwardLink blk
    let cbsPrevHash = toCHash $ blk ^. prevBlockL
        cbsMerkleRoot = toCHash . getMerkleRoot . mtRoot $ blk ^. blockTxs
    return CBlockSummary {..}

data CAddressType =
      CPubKeyAddress
    | CScriptAddress
    | CRedeemAddress
    | CUnknownAddress
    deriving (Show, Generic)

data CAddressSummary = CAddressSummary
    { caAddress :: !CAddress
    , caType    :: !CAddressType
    , caTxNum   :: !Word
    , caBalance :: !CCoin
    , caTxList  :: ![CTxBrief]
    } deriving (Show, Generic)

data CTxBrief = CTxBrief
    { ctbId         :: !CTxId
    , ctbTimeIssued :: !POSIXTime
    , ctbInputs     :: ![(CAddress, CCoin)]
    , ctbOutputs    :: ![(CAddress, CCoin)]
    , ctbInputSum   :: !CCoin
    , ctbOutputSum  :: !CCoin
    } deriving (Show, Generic)

-- FIXME: newtype?
data CNetworkAddress = CNetworkAddress !Text
    deriving (Show, Generic)

data CTxSummary = CTxSummary
    { ctsId              :: !CTxId
    , ctsTxTimeIssued    :: !POSIXTime
    , ctsBlockTimeIssued :: !(Maybe POSIXTime)
    , ctsBlockHeight     :: !(Maybe Word)
    , ctsRelayedBy       :: !(Maybe CNetworkAddress)
    , ctsTotalInput      :: !CCoin
    , ctsTotalOutput     :: !CCoin
    , ctsFees            :: !CCoin
    , ctsInputs          :: ![(CAddress, CCoin)]
    , ctsOutputs         :: ![(CAddress, CCoin)]
    } deriving (Show, Generic)

--------------------------------------------------------------------------------
-- FromHttpApiData instances
--------------------------------------------------------------------------------

instance FromHttpApiData CHash where
    parseUrlPiece = fmap toCHash . decodeHashHex

instance FromHttpApiData CAddress where
    parseUrlPiece = pure . CAddress

instance FromHttpApiData CTxId where
    parseUrlPiece = pure . CTxId . CHash

-- TODO: When we have a generic enough `readEither`
-- instance FromHttpApiData LocalSlotIndex where
--     parseUrlPiece = readEither

--------------------------------------------------------------------------------
-- Helper types and conversions
--------------------------------------------------------------------------------

data TxInternal = TxInternal
    { tiTimestamp :: !Timestamp
    , tiTx        :: !Tx
    } deriving (Show, Eq, Ord)

tiToTxEntry :: TxInternal -> CTxEntry
tiToTxEntry TxInternal{..} = toTxEntry tiTimestamp tiTx

convertTxOutputs :: [TxOut] -> [(CAddress, Coin)]
convertTxOutputs = map (toCAddress . txOutAddress &&& txOutValue)

toTxBrief :: TxInternal -> TxExtra -> CTxBrief
toTxBrief txi txe = CTxBrief {..}
  where
    tx            = tiTx txi
    ts            = tiTimestamp txi
    ctbId         = toCTxId $ hash tx
    ctbTimeIssued = toPosixTime ts
    ctbInputs     = map (second mkCCoin) txinputs
    ctbOutputs    = map (second mkCCoin) txOutputs
    ctbInputSum   = sumCoinOfInputsOutputs txinputs
    ctbOutputSum  = sumCoinOfInputsOutputs txOutputs

    txinputs      = convertTxOutputs $ map toaOut $ NE.toList $ teInputOutputs txe
    txOutputs     = convertTxOutputs . NE.toList $ _txOutputs tx

-- TODO (ks) : Needs to be refactored!
sumCoinOfInputsOutputs :: [(CAddress, Coin)] -> CCoin
sumCoinOfInputsOutputs addressList =
    mkCCoin $ mkCoin $ fromIntegral $ sum addressCoinList
      where
        -- | Get total number of coins from an address
        addressCoins :: (CAddress, Coin) -> Integer
        addressCoins (_, coin) = coinToInteger coin

        -- | Arbitrary precision, so we don't overflow
        addressCoinList :: [Integer]
        addressCoinList = addressCoins <$> addressList
