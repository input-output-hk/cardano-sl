{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module APISpec where

import           Universum

import           Control.Exception
import           Network.HTTP.Client              hiding (Proxy)
import           Network.HTTP.Types
import           Servant.API.Sub
import           Servant.QuickCheck
import           Servant.QuickCheck.Internal
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances        ()

import           Cardano.Wallet.API
import           Cardano.Wallet.API.Types
import           Cardano.Wallet.API.V1.Parameters
import           Cardano.Wallet.Server

--
-- Instances to allow use of `servant-quickcheck`.
--

instance HasGenRequest sub => HasGenRequest (Tags tags :> sub) where
    genRequest (Proxy :: Proxy (Tags tags :> sub)) = genRequest (Proxy :: Proxy sub)

instance HasGenRequest sub => HasGenRequest (Summary sum :> sub) where
    genRequest (Proxy :: Proxy (Summary sum :> sub)) = genRequest (Proxy :: Proxy sub)

instance HasGenRequest sub => HasGenRequest (WalletRequestParams :> sub) where
    genRequest _ = genRequest (Proxy @(WithWalletRequestParams sub))

--
-- RESTful-abiding predicates
--

-- | Checks that every DELETE request should return a 204 NoContent.
deleteReqShouldReturn204 :: RequestPredicate
deleteReqShouldReturn204 = RequestPredicate $ \req mgr ->
     if (method req == methodDelete)
       then do
         resp <- httpLbs req mgr
         let status = responseStatus resp
         when (statusIsSuccessful status && status /= status204) $
           throw $ PredicateFailure "deleteReqShouldReturn204" (Just req) resp
         return [resp]
       else return []

-- | Checks that every PUT request is idempotent. Calling an endpoint with a PUT
-- twice should return the same result.
putIdempotency :: RequestPredicate
putIdempotency = RequestPredicate $ \req mgr ->
     if (method req == methodPut)
       then do
         resp1 <- httpLbs req mgr
         resp2 <- httpLbs req mgr
         let body1 = responseBody resp1
         let body2 = responseBody resp2
         when (body1 /= body2) $
           throw $ PredicateFailure "putIdempotency" (Just req) resp1
         return [resp1, resp2]
       else return []

-- | Checks that every request which is not a 204 No Content
-- does not have an empty body, but it always returns something.
noEmptyBody :: RequestPredicate
noEmptyBody = RequestPredicate $ \req mgr -> do
  resp <- httpLbs req mgr
  let body   = responseBody resp
  let status = responseStatus resp
  when (status /= status204 && body == mempty) $
    throw $ PredicateFailure "noEmptyBody" (Just req) resp
  return [resp]

-- Our API apparently is returning JSON Arrays which is considered bad practice as very old
-- browsers can be hacked: https://haacked.com/archive/2009/06/25/json-hijacking.aspx/
-- TODO: Should we worry about this?
spec :: Spec
spec = describe "Servant API Properties" $ do
  it "follows best practices & is RESTful abiding" $ do
   withServantServer walletAPI (return walletServer) $ \burl ->
     serverSatisfies walletAPI burl stdArgs (not500
                                   <%> deleteReqShouldReturn204
                                   <%> putIdempotency
                                   <%> noEmptyBody
                                   <%> mempty)
