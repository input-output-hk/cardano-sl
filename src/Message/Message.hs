{-# LANGUAGE ConstraintKinds       #-}
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

    , MessageName
    , BinaryP (..)
    ) where

import           Control.Monad                 (forM_, unless)
import           Control.Monad.Trans           (lift)
import qualified Data.Binary                   as Bin
import qualified Data.Binary.Get               as Bin
import qualified Data.Binary.Put               as Bin
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Builder.Extra as BS
import qualified Data.ByteString.Lazy          as LBS
import           Data.Functor.Identity         (runIdentity)
import           Data.Maybe                    (fromMaybe)
import           Data.Monoid                   ((<>))
import           Data.String                   (IsString)
import           GHC.Generics                  (Generic)
import           Mockable.Channel              (Channel, ChannelT, newChannel,
                                                readChannel, unGetChannel, writeChannel)
import           Mockable.Class                (Mockable)


-- * Message name

newtype MessageName = MessageName BS.ByteString
deriving instance Eq MessageName
deriving instance Ord MessageName
deriving instance Show MessageName
deriving instance Generic MessageName
deriving instance IsString MessageName
instance Bin.Binary MessageName


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
    packMsg p t =
        BS.toLazyByteStringWith
            (BS.untrimmedStrategy 256 4096)
            LBS.empty
        . Bin.execPut
        $ Bin.put t

instance Bin.Binary t => Unpackable BinaryP t where
    unpackMsg p = recvNext

-- | Receive input from a channel.
--   If the channel's first element is 'Nothing' then it's the end of
--   input and you'll get 'End', otherwise we try to parse the 'thing'.
--   Unconsumed input is pushed back into the channel so that subsequent
--   'recvNext's will use it.
recvNext
    :: ( Mockable Channel m, Bin.Binary thing )
    => ChannelT m (Maybe BS.ByteString)
    -> m (Input thing)
recvNext chan = do
    mx <- readChannel chan
    case mx of
        Nothing -> pure End
        Just bs -> do
            (part, trailing) <- recvPart chan bs
            unless (BS.null trailing) $
                unGetChannel chan (Just trailing)
            case part of
                Nothing -> pure NoParse
                Just t  -> pure (Input t)

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
