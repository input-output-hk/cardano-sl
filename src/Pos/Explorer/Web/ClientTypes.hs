-- | Types for using in purescript-bridge

module Pos.Explorer.Web.ClientTypes
       ( CHash (..)
       , CAddress (..)
       , CTxId (..)
       , CBlockEntry (..)
       , toBlockEntry
       , CTxEntry (..)
       ) where

import           Universum

import qualified Data.ByteString.Lazy  as BSL
import           Data.Time.Clock.POSIX (POSIXTime)
import           Pos.Binary            (encode)
import           Pos.Slotting          (MonadSlots (..), getSlotStart)
import           Pos.Types             (Coin, MainBlock (..), Tx (..), TxOut (..),
                                        blockTxs, difficultyL, gbHeader, gbhConsensus,
                                        mcdSlot, mkCoin, sumCoins, unsafeAddCoin,
                                        unsafeIntegerToCoin)

-- | Client hash
newtype CHash = CHash Text deriving (Show, Eq, Generic, Buildable, Hashable)

-- | Client address
newtype CAddress = CAddress CHash deriving (Show, Eq, Generic, Hashable, Buildable)

-- | Client transaction id
newtype CTxId = CTxId CHash deriving (Show, Eq, Generic, Hashable)

-- | List of block entries is returned from "get latest N blocks" endpoint
data CBlockEntry = CBlockEntry
    { cbeHeight     :: !Word
    , cbeTimeIssued :: !POSIXTime
    , cbeTxNum      :: !Word
    , cbeTotalSent  :: !Coin
    , cbeSize       :: !Word64
    , cbeRelayedBy  :: !(Maybe Text)
    } deriving (Show, Generic)

toBlockEntry :: MonadSlots m => MainBlock ssc -> m CBlockEntry
toBlockEntry blk = do
    blkSlotStart <- getSlotStart $
                    blk ^. gbHeader . gbhConsensus . mcdSlot
    let cbeHeight = fromIntegral $ blk ^. difficultyL
        cbeTimeIssued = (/ 1e6) . fromIntegral $ blkSlotStart
        txs = blk ^. blockTxs
        cbeTxNum = fromIntegral $ length txs
        addCoins c Tx {..} = unsafeAddCoin c $
            unsafeIntegerToCoin . sumCoins $
            map txOutValue txOutputs
        cbeTotalSent = foldl' addCoins (mkCoin 0) txs
        -- TODO: is there a way to get it more efficiently?
        -- cbeSize = fromIntegral . BSL.length $ encode blk
        cbeSize = 0
        cbeRelayedBy = Nothing
    return CBlockEntry {..}

-- | List of tx entries is returned from "get latest N transactions" endpoint
data CTxEntry = CTxEntry
    { cteId         :: !CTxId
    , cteTimeIssued :: !POSIXTime
    , cteAmount     :: !Coin
    } deriving (Show, Generic)
