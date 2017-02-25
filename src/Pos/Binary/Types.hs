{-# LANGUAGE UndecidableInstances #-}

-- | Binary serialization of Pos.Types.* modules

module Pos.Binary.Types () where

import           Data.Binary.Get     (label)
import           Data.Binary.Put     (putWord8)
import           Data.Ix             (inRange)
import           Formatting          (formatToString, int, (%))
import           Universum           hiding (putByteString)

import           Pos.Binary.Class    (Bi (..), UnsignedVarInt (..))
import qualified Pos.Binary.Coin     as BinCoin
import           Pos.Binary.Merkle   ()
import           Pos.Binary.Script   ()
import           Pos.Binary.Version  ()
import           Pos.Constants       (epochSlots)
import qualified Pos.Data.Attributes as A
import qualified Pos.Types.Core      as T
import qualified Pos.Types.Types     as T

-- kind of boilerplate, but anyway that's what it was made for --
-- verbosity and clarity

instance Bi (A.Attributes ()) where
    get = label "Attributes" $
        A.getAttributes (\_ () -> Nothing) (Just (128 * 1024 * 1024)) ()
    put = A.putAttributes (\() -> [])

instance Bi T.Coin where
    put = mapM_ putWord8 . BinCoin.encode
    get = label "Coin" $ BinCoin.decode

instance Bi T.CoinPortion where
    put = put . T.getCoinPortion
    get = label "CoinPortion" $ get >>= T.mkCoinPortion

instance Bi T.LocalSlotIndex where
    get = label "LocalSlotIndex" $ T.LocalSlotIndex . getUnsignedVarInt <$> get
    put (T.LocalSlotIndex c) = put (UnsignedVarInt c)

instance Bi T.SlotId where
    put (T.SlotId e s) = put e >> put s
    get = label "SlotId" $ do
        siEpoch <- get
        siSlot <- get
        let errMsg =
                formatToString ("get@SlotId: invalid slotId ("%int%")") siSlot
        unless (inRange (0, epochSlots - 1) siSlot) $ fail errMsg
        return $ T.SlotId {..}

-- serialized as vector of TxInWitness
--instance Bi T.TxWitness where

instance Bi T.SharedSeed where
    put (T.SharedSeed bs) = put bs
    get = label "SharedSeed" $ T.SharedSeed <$> get

instance Bi T.ChainDifficulty where
    get = label "ChainDifficulty" $ T.ChainDifficulty . getUnsignedVarInt <$> get
    put (T.ChainDifficulty c) = put (UnsignedVarInt c)
