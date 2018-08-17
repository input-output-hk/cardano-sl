{-# LANGUAGE PolyKinds #-}

module Cardano.Wallet.API.V1.Generic
       ( gconsNames
       , gconsName
       ) where

import           Universum hiding (All, Generic)

import           Data.List ((!!))
import           Generics.SOP

--
-- Misc
--

-- | Get all constructors names available of an ADT
gconsNames
    :: forall a. (HasDatatypeInfo a, SListI (Code a))
    => Proxy a -> [Text]
gconsNames =
  map toText . hcollapse . hliftA (K . constructorName) . gconsInfos


-- | Get the ADT constructor's name of the given value
gconsName
  :: forall a. (Generic a, HasDatatypeInfo a)
  => a -> Text
gconsName a =
  gconsNames (Proxy @a) !! hindex (from a)


--
-- INTERNALS
--

gconsInfos
    :: forall a. (HasDatatypeInfo a)
    => Proxy a -> NP ConstructorInfo (Code a)
gconsInfos pa = case datatypeInfo pa of
    Newtype _ _ conInfo -> conInfo :* Nil
    ADT _ _ consInfo    -> consInfo

