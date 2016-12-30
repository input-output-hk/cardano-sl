{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Message.Message
    ( Input (..)
    , Packable (..)
    , Unpackable (..)
    , Extractable (..)

    , ContentData (..)
    , MessageName
    , WithHeaderData (..)
    , FullData (..)
    , RawData (..)

    , BinaryP (..)

    , sinkGetSafe
    ) where

import           Control.Monad                 (forM_, unless)
import           Control.Monad.Trans           (lift)
import qualified Data.Binary                   as Bin
import qualified Data.Binary.Get               as Bin
import qualified Data.Binary.Put               as Bin
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Builder.Extra as BS
import qualified Data.ByteString.Lazy          as LBS
import           Data.Conduit                  (Conduit, Sink, Source, await,
                                                awaitForever, fuseReturnLeftovers,
                                                leftover, yield, ($$), (=$=))
import qualified Data.Conduit.List             as CL
import           Data.Functor.Identity         (runIdentity)
import           Data.Maybe                    (fromMaybe)
import           Data.Monoid                   ((<>))
import           Data.String                   (IsString)
import           GHC.Generics                  (Generic)
import           Mockable.Channel              (Channel, ChannelT, newChannel,
                                                readChannel, unGetChannel, writeChannel)
import           Mockable.Class                (Mockable)

import Message.Util (fuseChannel)


-- * Message name

newtype MessageName = MessageName BS.ByteString
deriving instance Eq MessageName
deriving instance Ord MessageName
deriving instance Show MessageName
deriving instance Generic MessageName
deriving instance IsString MessageName
instance Bin.Binary MessageName


-- * Parts of message.
-- This code describes different parts of message, which serialization /
-- deserialization plays with.

-- | Some content. Serializes to `ByteString` directly, you would probably define
-- `Packable` / `Unpackable` instances with custom packing for this type.
newtype ContentData r = ContentData r

-- | Designates data given from incoming message, but not deserialized to any specified
-- object.
newtype RawData = RawData LBS.ByteString

-- | Message's header & something else
data WithHeaderData h r = WithHeaderData h r

-- | All available data from message, i.e. header, content in raw form, way to extract
-- content when its type gets known.
data FullData h = FullData h RawData Extractable


-- * Serialization strategy

data Input t where
    End :: Input t
    NoParse :: Input t
    Input :: t -> Input t

forInput :: Applicative m => Input t -> (t -> m (Input a)) -> m (Input a)
forInput End       _ = pure End
forInput NoParse   _ = pure NoParse
forInput (Input t) f = f t

-- | Extracts message of type @t@ from bytestream and passes it to handler.
receiving :: ( Unpackable p t, Monad m )
          => p
          -> (t -> Sink BS.ByteString m (Input a))
          -> Sink BS.ByteString m (Input a)
receiving p f = unpackMsg p >>= flip forInput f


-- | Defines a way to serialize object @r@ with given packing type @p@.
class Packable p r where
    -- | Way of packing data to raw bytes.
    -- TODO: use Data.ByteString.Builder?
    packMsg :: p -> r -> LBS.ByteString

-- | Defines a way to deserealize data with given packing type @p@ and extract object @r@.
class Unpackable p t where
    -- | Way to extract object @r@ from
    unpackMsg :: Monad m => p -> Sink BS.ByteString m (Input t)

-- | Allows to deserialize some bytestring when type of required result gets known
data Extractable = Extractable
    { extract :: forall p t . ( Unpackable p t )
              => p -> Input t
    }

-- | Contructs `Extractuble`
prepareExtract :: BS.ByteString -> Extractable
prepareExtract bs = Extractable $
    \p -> runIdentity (yield bs $$ unpackMsg p)


-- * Instances for message parts

instance ( Packable p (ContentData h), Packable p (ContentData r),
           Packable p (ContentData LBS.ByteString) )
        => Packable p (WithHeaderData h (ContentData r)) where
    packMsg p (WithHeaderData h r) =
        packMsg p (WithHeaderData h (RawData $ packMsg p r))

instance ( Packable p (ContentData h) )
        => Packable p (WithHeaderData h RawData) where
    packMsg p (WithHeaderData h (RawData raw)) =
        packMsg p (ContentData h) <> Bin.encode raw


instance ( Unpackable p (ContentData h), Unpackable p (ContentData BS.ByteString) )
        => Unpackable p (FullData h) where
    unpackMsg p = do
        receiving p $ \(ContentData h) ->
            receiving p $ \(ContentData raw) ->
                let body    = prepareExtract raw
                    rawData = RawData $ LBS.fromStrict raw
                in  return . Input $ FullData h rawData body


-- * Default instances

data BinaryP = BinaryP

instance Bin.Binary t => Packable BinaryP (ContentData t) where
    packMsg p (ContentData r) =
        BS.toLazyByteStringWith
            (BS.untrimmedStrategy 256 4096)
            LBS.empty
        . Bin.execPut
        $ Bin.put r

instance Bin.Binary t => Unpackable BinaryP (ContentData t) where
    unpackMsg p = sinkGetSafe (ContentData <$> Bin.get)

sinkGetSafe :: Monad m => Bin.Get t -> Sink BS.ByteString m (Input t)
sinkGetSafe f = sink (Bin.runGetIncremental f)
  where
    sink (Bin.Done bs _ v)  = do unless (BS.null bs) $ leftover bs
                                 return (Input v)
    sink (Bin.Fail u o e)   = return NoParse
    sink (Bin.Partial next) = do mx <- await
                                 case mx of
                                    Nothing -> return End
                                    Just x  -> sink (next (Just x))
