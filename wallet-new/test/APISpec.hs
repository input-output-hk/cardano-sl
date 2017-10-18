{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module APISpec where

import           Control.Exception
import           Control.Monad
import           Network.HTTP.Client         hiding (Proxy)
import           Network.HTTP.Types
import           Servant.API.Sub
import           Servant.QuickCheck
import           Servant.QuickCheck.Internal
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances   ()

import           Cardano.Wallet.API
import           Cardano.Wallet.API.Types
import           Cardano.Wallet.Server

instance HasGenRequest sub => HasGenRequest (Tags tags :> sub) where
    genRequest (Proxy :: Proxy (Tags tags :> sub)) = genRequest (Proxy :: Proxy sub)

instance HasGenRequest sub => HasGenRequest (Summary sum :> sub) where
    genRequest (Proxy :: Proxy (Summary sum :> sub)) = genRequest (Proxy :: Proxy sub)

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

-- Our API apparently is returning JSON Arrays which is considered bad practice as very old
-- browsers can be hacked: https://haacked.com/archive/2009/06/25/json-hijacking.aspx/
-- TODO: Should we worry about this?
spec :: Spec
spec = describe "API Servant Properties" $ do
  it "follows best practices" $ do
   withServantServer walletAPI (return walletServer) $ \burl ->
     serverSatisfies walletAPI burl stdArgs (not500
                                   -- Apparently Servant is lacking here: https://github.com/haskell-servant/servant/issues/489
                                   -- <%> notAllowedContainsAllowHeader
                                   <%> deleteReqShouldReturn204
                                   <%> mempty)
