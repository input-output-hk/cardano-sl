{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DefaultSignatures     #-}
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
    , Serializable

    , Message (..)
    , messageName'

    , MessageName (..)
    , BinaryP (..)
    , recvNext
    ) where

import           Control.Monad                 (unless)
import qualified Data.Binary                   as Bin
import qualified Data.Binary.Get               as Bin
import qualified Data.Binary.Put               as Bin
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Builder.Extra as BS
import qualified Data.ByteString.Lazy          as LBS
import           Data.Data                     (Data, dataTypeName, dataTypeOf)
import           Data.Proxy                    (Proxy (..), asProxyTypeOf)
import           Data.String                   (IsString)
import           Data.String                   (fromString)
import qualified Data.Text                     as T
import           Data.Void                     (Void, absurd)
import qualified Formatting                    as F
import           GHC.Generics                  (Generic)
import           Mockable.Channel              (Channel, ChannelT, readChannel,
                                                unGetChannel)
import           Mockable.Class                (Mockable)


-- * Message name

newtype MessageName = MessageName BS.ByteString
deriving instance Eq MessageName
deriving instance Ord MessageName
deriving instance Show MessageName
deriving instance Generic MessageName
deriving instance IsString MessageName
instance Bin.Binary MessageName

-- | Defines type with it's own `MessageName`.
class Message m where
    -- | Uniquely identifies this type
    messageName :: Proxy m -> MessageName
    default messageName :: Data m => Proxy m -> MessageName
    messageName proxy =
         MessageName . fromString . dataTypeName . dataTypeOf $
            undefined `asProxyTypeOf` proxy

    -- | Description of message, for debug purposes
    formatMessage :: m -> T.Text
    default formatMessage :: F.Buildable m => m -> T.Text
    formatMessage = F.sformat F.build


-- | Common instances

instance Message Void where
    formatMessage = absurd


-- | As `messageName`, but accepts message itself, may be more convinient is most cases.
messageName' :: Message m => m -> MessageName
messageName' = messageName . proxyOf
  where
    proxyOf :: a -> Proxy a
    proxyOf _ = Proxy


-- * Serialization strategy

data Input t where
    End :: Input t
    NoParse :: Input t
    Input :: t -> Input t

-- | Defines a way to serialize object @r@ with given packing type @p@.
class Packable packing thing where
    -- | Way of packing data to raw bytes.
    -- TODO: use Data.ByteString.Builder?
    packMsg :: packing -> thing -> LBS.ByteString

-- | Defines a way to deserealize data with given packing type @p@ and extract object @t@.
class Unpackable packing thing where
    unpackMsg :: ( Mockable Channel m )
              => packing
              -> ChannelT m (Maybe BS.ByteString)
              -> m (Input thing)

type Serializable packing thing =
    ( Packable packing thing
    , Unpackable packing thing
    )


-- * Default instances

data BinaryP = BinaryP

instance Bin.Binary t => Packable BinaryP t where
    packMsg _ t =
        BS.toLazyByteStringWith
            (BS.untrimmedStrategy 256 4096)
            LBS.empty
        . Bin.execPut
        $ Bin.put t

instance Bin.Binary t => Unpackable BinaryP t where
    unpackMsg _ = recvNext Bin.get

-- | Receive input from a channel.
--   If the channel's first element is 'Nothing' then it's the end of
--   input and you'll get 'End', otherwise we try to parse the 'thing'.
--   Unconsumed input is pushed back into the channel so that subsequent
--   'recvNext's will use it.
recvNext
    :: ( Mockable Channel m )
    => Bin.Get thing
    -> ChannelT m (Maybe BS.ByteString)
    -> m (Input thing)
recvNext get' chan = do
    mx <- readChannel chan
    case mx of
        Nothing -> pure End
        Just bs -> do
            (part, trailing) <- recvPart get' chan bs
            unless (BS.null trailing) $
                unGetChannel chan (Just trailing)
            case part of
                Nothing -> pure NoParse
                Just t  -> pure (Input t)

recvPart
    :: ( Mockable Channel m )
    => Bin.Get thing
    -> ChannelT m (Maybe BS.ByteString)    -- source
    -> BS.ByteString                       -- prefix
    -> m (Maybe thing, BS.ByteString)      -- trailing
recvPart get' chan prefix =
    go (Bin.pushChunk (Bin.runGetIncremental get') prefix)
  where
    go (Bin.Done trailing _ a) = return (Just a, trailing)
    go (Bin.Fail trailing _ _) = return (Nothing, trailing)
    go (Bin.Partial continue) = do
      mx <- readChannel chan
      go (continue mx)
