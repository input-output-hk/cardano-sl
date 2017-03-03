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

module Node.Message
    ( Packable (..)
    , Unpackable (..)
    , Serializable
    , Bin.Decoder(..)

    , Message (..)
    , messageName'

    , MessageName (..)
    , BinaryP (..)
    ) where

import qualified Data.Binary                   as Bin
import qualified Data.Binary.Get               as Bin
import qualified Data.Binary.Put               as Bin
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Builder.Extra as BS
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

-- * Serialization strategy

-- | Defines a way to serialize object @r@ with given packing type @p@.
class Packable packing thing where
    -- | Way of packing data to raw bytes.
    -- TODO: use Data.ByteString.Builder?
    packMsg :: packing -> thing -> LBS.ByteString

-- | Defines a way to deserealize data with given packing type @p@ and extract object @t@.
class Unpackable packing thing where
    unpackMsg :: packing -> Bin.Decoder thing

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
    unpackMsg _ = Bin.runGetIncremental Bin.get
