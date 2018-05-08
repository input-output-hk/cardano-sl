{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NumDecimals #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module QuickCheckSpecs (spec) where

import Universum

import           Network.HTTP.Client hiding (Proxy)
import           Network.HTTP.Types
import           Servant
import           Servant.QuickCheck
import           Servant.QuickCheck.Internal
import           Test.Hspec
import           Test.QuickCheck

import           Cardano.Wallet.API.Request (FilterBy, SortBy)
import           Cardano.Wallet.API.Types (AlternativeApiArg, Tags, WithDefaultApiArg)
import qualified Cardano.Wallet.API.V1 as V0
import qualified Cardano.Wallet.API.V1 as V1
import           Cardano.Wallet.API.V1.Parameters (WalletRequestParams, WithWalletRequestParams)

-- Our API apparently is returning JSON Arrays which is considered bad practice as very old
-- browsers can be hacked: https://haacked.com/archive/2009/06/25/json-hijacking.aspx/
-- The general consensus, after discussing this with the team, is that we can be moderately safe.
-- stack test cardano-sl-wallet-new --fast --test-arguments '-m "Servant API Properties"'
spec :: Spec
spec = do
    let burl = BaseUrl
            { baseUrlScheme = Http
            , baseUrlHost = "localhost"
            , baseUrlPort = 8090
            , baseUrlPath = ""
            }
    describe "Servant API Properties" $ do
        it "V0 API follows best practices & is RESTful abiding" $ do
            serverSatisfies (Proxy @V0.API) burl stdArgs predicates
        it "V1 API follows best practices & is RESTful abiding" $ do
            serverSatisfies (Proxy @V1.API) burl stdArgs predicates
--
-- Instances to allow use of `servant-quickcheck`.
--

instance HasGenRequest (apiType a :> sub) =>
         HasGenRequest (WithDefaultApiArg apiType a :> sub) where
    genRequest _ = genRequest (Proxy @(apiType a :> sub))

instance HasGenRequest (argA a :> argB a :> sub) =>
         HasGenRequest (AlternativeApiArg argA argB a :> sub) where
    genRequest _ = genRequest (Proxy @(argA a :> argB a :> sub))

-- NOTE(adinapoli): This can be improved to produce proper filtering & sorting
-- queries.
instance HasGenRequest sub => HasGenRequest (SortBy syms res :> sub) where
    genRequest _ = genRequest (Proxy @sub)

instance HasGenRequest sub => HasGenRequest (FilterBy syms res :> sub) where
    genRequest _ = genRequest (Proxy @sub)

instance HasGenRequest sub => HasGenRequest (Tags tags :> sub) where
    genRequest _ = genRequest (Proxy :: Proxy sub)

instance HasGenRequest (sub :: *) => HasGenRequest (WalletRequestParams :> sub) where
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
                throwM $ PredicateFailure "deleteReqShouldReturn204" (Just req) resp
            return [resp]
        else
            return []

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
                 throwM $ PredicateFailure "putIdempotency" (Just req) resp1
             return [resp1, resp2]
         else
             return []

-- | Checks that every request which is not a 204 No Content
-- does not have an empty body, but it always returns something.
noEmptyBody :: RequestPredicate
noEmptyBody = RequestPredicate $ \req mgr -> do
    resp <- httpLbs req mgr
    let body   = responseBody resp
    let status = responseStatus resp
    when (status `notElem` [status204, status404] && body == mempty) $
        throwM $ PredicateFailure "noEmptyBody" (Just req) resp
    return [resp]

-- | All the predicates we want to enforce in our API.
predicates :: Predicates
predicates = not500
         <%> deleteReqShouldReturn204
         <%> putIdempotency
         <%> noEmptyBody
         <%> mempty
