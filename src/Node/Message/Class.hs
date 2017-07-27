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
    , messageName'

    , MessageName (..)
    ) where

import qualified Data.Binary                   as Bin
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import           Data.Data                     (Data, dataTypeName, dataTypeOf)
import           Data.Hashable                 (Hashable)
import           Data.Proxy                    (Proxy (..), asProxyTypeOf)
import           Data.String                   (IsString, fromString)
import qualified Data.Text                     as T
import           Data.Text.Buildable           (Buildable)
import qualified Data.Text.Buildable           as B
import qualified Formatting                    as F
import           GHC.Generics                  (Generic)
import           Serokell.Util.Base16          (base16F)
import           Node.Message.Decoder          (Decoder, hoistDecoder)

-- * Message name

newtype MessageName = MessageName BS.ByteString
deriving instance Eq MessageName
deriving instance Ord MessageName
deriving instance Show MessageName
deriving instance Generic MessageName
deriving instance IsString MessageName
deriving instance Hashable MessageName
deriving instance Monoid MessageName
instance Bin.Binary MessageName

instance Buildable MessageName where
    build (MessageName mn) = F.bprint base16F mn

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

-- | As `messageName`, but accepts message itself, may be more convinient is most cases.
messageName' :: Message m => m -> MessageName
messageName' = messageName . proxyOf
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
    , packM :: forall t . PackM packingType t -> m t
    , unpackM :: forall t . UnpackM packingType t -> m t
    }

pack :: ( Serializable packingType t ) => Packing packingType m -> t -> m LBS.ByteString
pack Packing {..} = packM . packMsg packingType

unpack :: ( Functor m, Serializable packingType t ) => Packing packingType m -> Decoder m t
unpack Packing {..} = hoistDecoder unpackM (unpackMsg packingType)
