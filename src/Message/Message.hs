{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Message.Message
    ( MessageName
    ) where

import qualified Data.Binary                       as Bin
import qualified Data.Binary.Get as Bin
import qualified Data.Binary.Put                  as Bin
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Builder.Extra as BS
import Data.Conduit (Sink)
import GHC.Generics (Generic)
import Data.String (IsString)
import qualified Data.ByteString.Lazy              as LBS
import Data.Monoid ((<>))
import Mockable.Channel (Channel, ChannelT, newChannel, writeChannel)
import Mockable.Class (Mockable)


-- * Message name

newtype MessageName = MessageName BS.ByteString
deriving instance Eq MessageName
deriving instance Ord MessageName
deriving instance Show MessageName
deriving instance Generic MessageName
deriving instance IsString MessageName
instance Bin.Binary MessageName


-- * Parts of message.
-- This code describes different parts of message. which are enought for serializing
-- message / could be extracted on deserializing.

-- | Serializes / deserialized directly
newtype ContentData r = ContentData r

-- | Designates data given from incoming message, but not deserialized to any specified
-- object.
newtype RawData = RawData LBS.ByteString

-- | Message's header & something else
data WithHeaderData h r = WithHeaderData h r

data FullData h = FullData h RawData Extractable


-- * Dummy channel

-- * Serialization strategy

type ParseError = String

data Input t where
    End :: Input t
    NoParse :: Input t
    Input :: t -> Input t

-- | Defines a way to serialize object @r@ with given packing type @p@.
class Packable p r where
    -- | Way of packing data to raw bytes.
    packMsg :: p -> r -> LBS.ByteString

-- | Defines a way to deserealize data with given packing type @p@ and extract object @r@.
class Unpackable p r where
    unpackMsg :: ( Mockable Channel m )
              => p -> ChannelT m (Maybe BS.ByteString) -> m (Input r)

data Extractable = Extractable
    { extract :: forall p m r . ( Unpackable p r, Mockable Channel m )
              => p -> m (Input r)
    }

prepareExtract :: ( Mockable Channel m ) => LBS.ByteString -> m Extractable
prepareExtract bs = do
    chan <- newChannel
    mapM_ (writeChannel chan . Just) $ LBS.toChunks bs
    writeChannel chan Nothing
    return $ Extractable $ flip unpackMsg chan


-- * Instances for message parts

instance ( Packable p (ContentData h), Packable p (ContentData r),
           Packable p LBS.ByteString )
        => Packable p (WithHeaderData h (ContentData r)) where
    packMsg p (WithHeaderData h r) =
        packMsg p (WithHeaderData h (RawData $ packMsg p r))

instance ( Packable p (ContentData h) )
        => Packable p (WithHeaderData h RawData) where
    packMsg p (WithHeaderData h (RawData raw)) =
        packMsg p (ContentData h) <> raw

forInput :: Input t -> (t -> m (Input a)) -> m (Input a)
forInput End       _ = pure End
forInput NoParse   _ = pure NoParse
forInput (Input t) f = f t

instance ( Unpackable p (ContentData h), Unpackable p LBS.ByteString )
        => Unpackable p (FullData h) where
    unpackMsg p chan = do
        iheader <- unpackMsg p chan
        forInput iheader $ \(ContentData h) -> do
            iraw <- unpackMsg p chan
            forInput iraw $ \raw ->
                let body = prepareExtract raw
                in  return $ Input $ FullData h (RawData raw) body


-- * Default instances

data BinaryP = BinaryP

serialise :: Bin.Put -> LBS.ByteString
serialise = BS.toLazyByteStringWith
                 (BS.untrimmedStrategy 256 4096)
                 LBS.empty
              . Bin.execPut

instance Bin.Binary r => Packable BinaryP (ContentData r) where
    packMsg p (ContentData r) = serialise $ Bin.put r

instance Bin.Binary r => Unpackable BinaryP (ContentData r) where
    unpackMsg p chan = do
        mx <- readChannel chan
        case mx of
            Nothing -> pure End
            Just bs -> do
                (part, trailing) <- recvPart chan bs
                unless (BS.null trailing) $
                    unGetChannel chan (Just trailing)
                case part of
                    Nothing -> pure NoParse
                    Just t -> pure (Input t)

-- FIXME
-- Serializing to "" is a problem. (), for instance, can't be used as data
-- over the wire.
-- It serializes to "". If we demand that the "" appear as a separate piece
-- of the channel then we're fine with the current implementation of recvNext.
-- If not, then even if a Nothing is pulled from the channel, we may still
-- parse a ().

recvPart
    :: ( Mockable Channel m, Bin.Binary thing )
    => ChannelT m (Maybe BS.ByteString)    -- source
    -> BS.ByteString                       -- prefix
    -> m (Maybe thing, BS.ByteString)      -- trailing
recvPart chan prefix =
    go (Bin.pushChunk (Bin.runGetIncremental Bin.get) prefix)
  where
    go (Bin.Done trailing _ a) = return (Just a, trailing)
    go (Bin.Fail trailing _ _) = return (Nothing, trailing)
    go (Bin.Partial continue) = do
        mx <- readChannel chan
        go (continue mx)
