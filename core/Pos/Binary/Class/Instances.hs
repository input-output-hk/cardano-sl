{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Useful basic instances.

module Pos.Binary.Class.Instances
       (
       ) where

import           Universum

import qualified Data.Map                    as M
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Internal    as BS
import qualified Data.HashMap.Strict         as HM
import qualified Data.HashSet                as HS
import qualified Data.Set                    as S
import qualified Data.Store.Core             as Store
import qualified Data.Store.Internal         as Store
import           Data.Tagged                 (Tagged (..))
import           Data.Time.Units             (Microsecond, Millisecond)
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import           Serokell.Data.Memory.Units  (Byte, fromBytes, toBytes)
import           System.IO.Unsafe            (unsafePerformIO)

import           Pos.Binary.Class.Core       (Bi (..), getSize)
import           Pos.Binary.Class.Numbers    (UnsignedVarInt (..), getWord8, putWord8)
import           Pos.Binary.Class.Store      (Size (..), StaticSize, combineSize,
                                              execPoke, mkPoke, sizeOf)

----------------------------------------------------------------------------
-- Primitive types
----------------------------------------------------------------------------

instance Bi () where
    put ()  = pure ()
    get     = pure ()
    size = ConstSize 0

instance Bi Bool where
    put False = putWord8 0
    put True  = putWord8 1
    get       = getWord8 >>= toBool
      where
        toBool 0 = return False
        toBool 1 = return True
        toBool c = fail ("Could not map value " ++ show c ++ " to Bool")
    size = ConstSize 1

-- [CSL-1122] restore this instance, I guess
{-

instance Bi Char where
    {-# INLINE put #-}
    put = putCharUtf8
    get = do
        let getByte = (fromIntegral :: Word8 -> Int) <$> get
            shiftL6 = flip shiftL 6 :: Int -> Int
        w <- getByte
        r <- case () of
                _ | w < 0x80  -> return w
                  | w < 0xe0  -> do
                                    x <- xor 0x80 <$> getByte
                                    return (x .|. shiftL6 (xor 0xc0 w))
                  | w < 0xf0  -> do
                                    x <- xor 0x80 <$> getByte
                                    y <- xor 0x80 <$>  getByte
                                    return (y .|. shiftL6 (x .|. shiftL6
                                            (xor 0xe0 w)))
                  | otherwise -> do
                                x <- xor 0x80 <$> getByte
                                y <- xor 0x80 <$> getByte
                                z <- xor 0x80 <$> getByte
                                return (z .|. shiftL6 (y .|. shiftL6
                                        (x .|. shiftL6 (xor 0xf0 w))))
        getChr r
      where
        getChr w
          | w <= 0x10ffff = return $! toEnum $ fromEnum w
          | otherwise = fail "Not a valid Unicode code point!"

-}

----------------------------------------------------------------------------
-- Numeric data
----------------------------------------------------------------------------

-- CSL-1122: All these instances should use the same endianness on all
-- platforms (the instances defined in store have architecture-dependent
-- endianness). Also, all these instances should be implemented manually
-- instead of using store.

instance Bi Integer where
    size = Store.size
    put = Store.poke
    get = Store.peek

instance Bi Word16 where
    size = Store.size
    put = Store.poke
    get = Store.peek

instance Bi Word32 where
    size = Store.size
    put = Store.poke
    get = Store.peek

instance Bi Word64 where
    size = Store.size
    put = Store.poke
    get = Store.peek

instance Bi Int32 where
    size = Store.size
    put = Store.poke
    get = Store.peek

----------------------------------------------------------------------------
-- Tagged
----------------------------------------------------------------------------

instance Bi a => Bi (Tagged s a) where
    put (Tagged a) = put a
    get = Tagged <$> get
    size = sizeOf unTagged

----------------------------------------------------------------------------
-- Containers
----------------------------------------------------------------------------

instance (Bi a, Bi b) => Bi (a, b) where
    {-# INLINE size #-}
    size = combineSize (fst, snd)
    {-# INLINE put #-}
    put (a, b) = put a *> put b
    {-# INLINE get #-}
    get = liftM2 (,) get get

instance (Bi a, Bi b, Bi c) => Bi (a, b, c) where
    {-# INLINE size #-}
    size = combineSize (view _1, view _2, view _3)
    {-# INLINE put #-}
    put (a, b, c) = put a *> put b *> put c
    {-# INLINE get #-}
    get = liftM3 (,,) get get get

instance (Bi a, Bi b, Bi c, Bi d) => Bi (a, b, c, d) where
    {-# INLINE size #-}
    size = combineSize (view _1, view _2, view _3, view _4)
    {-# INLINE put #-}
    put (a, b, c, d) = put a *> put b *> put c *> put d
    {-# INLINE get #-}
    get = liftM4 (,,,) get get get get

-- Copy-pasted from
-- https://github.com/fpco/store/blob/master/src/Data/Store/Internal.hs#L378-L389
--
-- CSL-1122: define via putBytes
instance Bi ByteString where
    size = VarSize $ \x ->
        let l = BS.length x in
        getSize (UnsignedVarInt l) + l
    put x = do
        let (sourceFp, sourceOffset, sourceLength) = BS.toForeignPtr x
        put $ UnsignedVarInt sourceLength
        Store.pokeFromForeignPtr sourceFp sourceOffset sourceLength
    get = do
        UnsignedVarInt len <- get
        fp <- Store.peekToPlainForeignPtr "Data.ByteString.ByteString" len
        return (BS.PS fp 0 len)

-- CSL-1122: copy the instance here
instance Bi LByteString where
    size = Store.size
    put = Store.poke
    get = Store.peek

-- CSL-1122: copy the instance here
instance Bi Text where
    size = Store.size
    put = Store.poke
    get = Store.peek

-- CSL-1122: copy the instance here and define via putBytes
instance KnownNat n => Bi (StaticSize n ByteString) where
    size = Store.size
    put = Store.poke
    get = Store.peek

instance Bi a => Bi [a] where
    size =
        VarSize $ \t ->
            let s = getSize (UnsignedVarInt (length t))
            in case size :: Size a of
                   ConstSize n -> n * length t + s
                   VarSize f   -> foldl' (\acc x -> acc + f x) s t
    put t = do
        put (UnsignedVarInt $ length t)
        mkPoke (\ptr offset ->
            foldlM (\offset' a -> execPoke (put a) ptr offset') offset t)
    get = do
      UnsignedVarInt len <- get
      replicateM len get

instance (Bi a, Bi b) => Bi (Either a b) where
    size = case (size @a, size @b) of
        (ConstSize s1, ConstSize s2) | s1 == s2 ->
            ConstSize (s1 + 1)
        _other ->
            VarSize $ \case
                Left  a -> getSize a + 1
                Right b -> getSize b + 1
    put (Left  a) = putWord8 0 *> put a
    put (Right b) = putWord8 1 *> put b
    get = do
        w <- getWord8
        case w of
            0 -> Left  <$> get
            1 -> Right <$> get
            _ -> fail "unexpected Either tag"

instance Bi a => Bi (NonEmpty a) where
    get = maybe (fail "Empty list") pure . nonEmpty =<< get
    put = put . toList
    size = sizeOf toList

instance (Bi a) => Bi (Maybe a) where
    size = VarSize $ \case
              Just x -> 1 + getSize x
              _ -> 1
    put Nothing  = putWord8 0
    put (Just x) = putWord8 1 *> put x
    get = do
        w <- getWord8
        case w of
            0 -> return Nothing
            1 -> Just <$> get
            _ -> fail "unexpected Maybe tag"

instance (Hashable k, Eq k, Bi k, Bi v) => Bi (HM.HashMap k v) where
    get = fmap HM.fromList get
    put = put . HM.toList
    size = sizeOf HM.toList

instance (Ord k, Eq k, Bi k, Bi v) => Bi (M.Map k v) where
    get = fmap M.fromList get
    put = put . M.toList
    size = sizeOf M.toList

instance (Hashable k, Eq k, Bi k) => Bi (HashSet k) where
    get = fmap HS.fromList get
    put = put . HS.toList
    size = sizeOf HS.toList

instance (Ord k, Bi k) => Bi (Set k) where
    get = S.fromList <$> get
    put = put . S.toList
    size = sizeOf S.toList

-- Copy-pasted w/ modifications, license:
-- https://github.com/bos/vector-binary-instances/blob/master/LICENSE

instance Bi a => Bi (V.Vector a) where
    get = do
        UnsignedVarInt n <- get
        v <- pure $ unsafePerformIO $ GM.unsafeNew n
        let go 0 = return ()
            go i = do
                x <- get
                () <- pure $ unsafePerformIO $ GM.unsafeWrite v (n-i) x
                go (i-1)
        () <- go n
        pure $ unsafePerformIO $ G.unsafeFreeze v
    put v = do
        put (UnsignedVarInt (G.length v))
        G.mapM_ put v
    size = VarSize $ \v ->
        (getSize (UnsignedVarInt (G.length v))) +
        G.foldl' (\a b -> a + getSize b) 0 v

instance Bi Void where
    put = absurd
    get = fail "instance Bi Void: you shouldn't try to deserialize Void"
    size = error "instance Bi Void: you shouldn't try to serialize Void"

----------------------------------------------------------------------------
-- Other types
----------------------------------------------------------------------------

instance Bi Millisecond where
    put = put . toInteger
    get = fromInteger <$> get
    size = sizeOf toInteger

instance Bi Microsecond where
    put = put . toInteger
    get = fromInteger <$> get
    size = sizeOf toInteger

instance Bi Byte where
    put = put . toBytes
    get = fromBytes <$> get
    size = sizeOf toBytes
