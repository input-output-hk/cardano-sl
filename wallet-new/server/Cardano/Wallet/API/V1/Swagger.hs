{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cardano.Wallet.API.V1.Swagger where

import           Cardano.Wallet.API
import           Cardano.Wallet.API.Types
import           Cardano.Wallet.API.V1.Types

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Monoid
import           Data.String.Conv
import           Data.Swagger
import           Data.Swagger.Declare
import qualified Data.Text                   as T
import           Data.Typeable
import           GHC.TypeLits
import           Servant.API.Sub
import           Servant.Swagger
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Random

--
-- Helper functions
--

-- | Generate an example for type @a@ with a static seed.
genExample :: (ToJSON a, Arbitrary a) => a
genExample = (unGen (resize 3 arbitrary)) (mkQCGen 42) 42

fromArbitraryJSON :: (ToJSON a, Typeable a, Arbitrary a)
                  => proxy a
                  -> Declare (Definitions Schema) NamedSchema
fromArbitraryJSON (_ :: proxy a) = do
    let (randomSample :: a) = genExample
    return $ NamedSchema (Just $ toS $ show $ typeOf randomSample) (sketchSchema randomSample)

-- | Adds a randomly-generated but valid example to the spec, formatted as a JSON.
withExample :: (ToJSON a, Arbitrary a) => proxy a -> T.Text -> T.Text
withExample (_ :: proxy a) desc =
  desc <> " Here's an example:<br><br><pre>" <> toS (encodePretty $ toJSON @a genExample) <> "</pre>"

--
-- Extra Typeclasses
--

-- TODO: Writing instances this way is a bit verbose. Is there a better way?
class (ToJSON a, Typeable a, Arbitrary a) => ToDocs a where
  annotate :: (proxy a -> Declare (Definitions Schema) NamedSchema)
           -> proxy a
           -> Declare (Definitions Schema) NamedSchema

--
-- Instances
--

instance ( KnownSymbol summary , HasSwagger subApi) => HasSwagger (Summary summary :> subApi) where
    toSwagger _ =
        let summaryTxt = toS (symbolVal (Proxy @summary))
            swgr       = toSwagger (Proxy @subApi)
        in swgr & (operationsOf swgr) . summary ?~ summaryTxt

instance ToDocs APIVersion where
  annotate f p = do
    s <- f p
    return $ s & (schema . description ?~ (withExample p "The API version. We currently support v0 and v1."))
               . (schema . example ?~ toJSON @APIVersion genExample)

instance ToSchema APIVersion where
  declareNamedSchema = annotate fromArbitraryJSON

instance ToDocs Account where
  annotate f p = do
    s <- f p
    return $ s & (schema . description ?~ "An Account")
               . (schema . example ?~ toJSON @Account genExample)

instance ToSchema Account where
  declareNamedSchema = annotate fromArbitraryJSON

instance ToDocs Address where
  annotate f p = do
    s <- f p
    return $ s & (schema . description ?~ withExample p "An Address.")
               . (schema . example ?~ toJSON @Address genExample)

instance ToSchema Address where
  declareNamedSchema = annotate fromArbitraryJSON

instance ToSchema Metadata where
  declareNamedSchema = fromArbitraryJSON

instance ToDocs a => ToDocs (ExtendedResponse a) where
  annotate f p = (f p)

instance (ToJSON a, ToDocs a, Typeable a, Arbitrary a) => ToSchema (ExtendedResponse a) where
  declareNamedSchema = annotate fromArbitraryJSON

instance (ToDocs a) => ToDocs [a] where
  annotate f p = do
    s <- f p
    return $ s & (schema . description ?~ withExample p ("A list of " <> toS (show $ typeOf (genExample @ a)) <> "."))

instance (ToDocs a, ToDocs b) => ToDocs (OneOf a b) where
  annotate f p = do
    s <- f p
    return $ s & (schema . description ?~ desc)
    where
      typeOfA, typeOfB :: T.Text
      typeOfA = toS (show $ typeOf @b exampleOfB)
      typeOfB = toS (show $ typeOf @a exampleOfA)
      exampleOfA = genExample
      exampleOfB = genExample

      desc :: T.Text
      desc = "OneOf <b>a</b> <b>b</b> is a type introduced to limit with Swagger 2.0's limitation of returning " <>
             "different types depending on the parameter of the request. While this has been fixed " <>
             "in OpenAPI 3, we effectively mimick its behaviour in 2.x. The idea is to return either " <>
             typeOfA <> " or " <> typeOfB <>
             " depending on whether or not the extended response format has been requested. Whilst using the " <>
             " api this type is erased away in the HTTP response, so that, in case the user requested the 'normal' " <>
             (withExample (Proxy @ a) " response format, an 'a' will be returned.") <>
             (withExample (Proxy @ b) " In case the user selected the extended format, a full 'ExtendedResponse' will be yielded.")

instance ( ToDocs a, ToDocs b) => ToSchema (OneOf a b) where
  declareNamedSchema = annotate fromArbitraryJSON

--
-- The API
--

api :: Swagger
api = toSwagger walletAPI
  & info.title   .~ "Cardano Wallet API"
  & info.version .~ "2.0"
  & info.description ?~ "This is an API that tests swagger integration"
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")
