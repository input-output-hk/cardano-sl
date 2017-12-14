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

module Node.Message.Class
    ( PackingType (..)
    , Serializable (..)

    , Packing (..)

    , pack
    , unpack

    , Message (..)
    , messageCode'

    , MessageCode
    ) where

import qualified Data.ByteString.Lazy as LBS
import           Data.Proxy (Proxy (..))
import qualified Data.Text as T
import           Data.Word (Word16)
import qualified Formatting as F
import           Node.Message.Decoder (Decoder, hoistDecoder)

-- * Message name

type MessageCode = Word16

-- | Defines type with it's own `MessageCode`.
class Message m where
    -- | Uniquely identifies this type
    messageCode :: Proxy m -> MessageCode

    -- | Description of message, for debug purposes
    formatMessage :: m -> T.Text
    default formatMessage :: F.Buildable m => m -> T.Text
    formatMessage = F.sformat F.build

-- | As `messageName`, but accepts message itself, may be more convinient is most cases.
messageCode' :: Message m => m -> MessageCode
messageCode' = messageCode . proxyOf
  where
    proxyOf :: a -> Proxy a
    proxyOf _ = Proxy

class PackingType packingType where
    type PackM packingType :: * -> *
    type UnpackM packingType :: * -> *

-- | Defines a way to serialize object @r@ with given packing type @p@.
-- The @PackM@, @UnpackM@ monadic contexts of the given packing type are used.
--
-- TODO should use a proxy on packing rather than a value, no?
class ( PackingType packingType ) => Serializable packingType thing where
    -- | Way of packing data to raw bytes.
    packMsg :: Proxy packingType -> thing -> PackM packingType LBS.ByteString
    -- | Incrementally unpack.
    unpackMsg :: Proxy packingType -> Decoder (UnpackM packingType) thing

-- | Picks out a packing type and injections to make it useful within a given
-- monad.
data Packing packingType m = Packing
    { packingType :: Proxy packingType
    , packM       :: forall t . PackM packingType t -> m t
    , unpackM     :: forall t . UnpackM packingType t -> m t
    }

pack :: ( Serializable packingType t ) => Packing packingType m -> t -> m LBS.ByteString
pack Packing {..} = packM . packMsg packingType

unpack :: ( Functor m, Serializable packingType t ) => Packing packingType m -> Decoder m t
unpack Packing {..} = hoistDecoder unpackM (unpackMsg packingType)
