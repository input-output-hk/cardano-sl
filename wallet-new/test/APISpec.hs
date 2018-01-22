{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module APISpec where

import           Universum

import qualified Control.Concurrent.STM as STM
import           Data.Default (def)
import           Network.HTTP.Client hiding (Proxy)
import           Network.HTTP.Types
import           Pos.Communication (SendActions)
import           Pos.Util.CompileInfo (withCompileInfo)
import           Pos.Wallet.WalletMode (WalletMempoolExt)
import           Pos.Wallet.Web.Methods (AddrCIdHashes (..))
import           Pos.Wallet.Web.Mode (WalletWebMode, WalletWebModeContext (..))
import           Pos.Wallet.Web.Sockets (ConnectionsVar)
import           Pos.Wallet.Web.State (WalletState)
import           Pos.WorkMode (RealModeContext (..))
import           Serokell.AcidState.ExtendedState
import           Servant
import           Servant.QuickCheck
import           Servant.QuickCheck.Internal
import           Test.Hspec
import           Test.Pos.Util (withDefConfigurations)
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Cardano.Wallet.API.Types
import qualified Cardano.Wallet.API.V1 as V0
import qualified Cardano.Wallet.API.V1 as V1
import qualified Cardano.Wallet.API.V1.Handlers as V0
import qualified Cardano.Wallet.API.V1.Handlers as V1
import qualified Cardano.Wallet.API.V1.Migration as Migration
import           Cardano.Wallet.API.V1.Parameters

--
-- Instances to allow use of `servant-quickcheck`.
--

instance HasGenRequest (apiType a :> sub) =>
         HasGenRequest (WithDefaultApiArg apiType a :> sub) where
    genRequest _ = genRequest (Proxy @(apiType a :> sub))

instance HasGenRequest (argA a :> argB a :> sub) =>
         HasGenRequest (AlternativeApiArg argA argB a :> sub) where
    genRequest _ = genRequest (Proxy @(argA a :> argB a :> sub))

instance HasGenRequest sub => HasGenRequest (Tags tags :> sub) where
    genRequest _ = genRequest (Proxy :: Proxy sub)

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
           throwM $ PredicateFailure "deleteReqShouldReturn204" (Just req) resp
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
           throwM $ PredicateFailure "putIdempotency" (Just req) resp1
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
    throwM $ PredicateFailure "noEmptyBody" (Just req) resp
  return [resp]

-- | All the predicates we want to enforce in our API.
predicates :: Predicates
predicates = not500
         <%> deleteReqShouldReturn204
         <%> putIdempotency
         <%> noEmptyBody
         <%> mempty

-- | "Lowers" V0 Handlers from our domain-specific monad to a @Servant@ 'Handler'.
v0Server :: ( Migration.HasConfigurations
            , Migration.HasCompileInfo
            ) => IO (Server V0.API)
v0Server = do
  -- TODO(adinapoli): If the monadic stack ends up diverging between V0 and V1,
  -- it's obviously incorrect using 'testV1Context' here.
  ctx <- testV1Context
  return (V0.handlers (Migration.v1MonadNat ctx))

-- | "Lowers" V1 Handlers from our domain-specific monad to a @Servant@ 'Handler'.
v1Server :: ( Migration.HasConfigurations
            , Migration.HasCompileInfo
            ) => IO (Server V1.API)
v1Server = do
  ctx <- testV1Context
  return (V1.handlers (Migration.v1MonadNat ctx))

-- | Returns a test 'V1Context' which can be used for the API specs.
-- Such context will use an in-memory database.
testV1Context :: Migration.HasConfiguration => IO Migration.V1Context
testV1Context =
    WalletWebModeContext <$> testStorage
                         <*> testConnectionsVar
                         <*> testAddrCIdHashes
                         <*> testSendActions
                         <*> testRealModeContext
  where
    testStorage :: IO WalletState
    testStorage = openMemoryExtendedState def

    testConnectionsVar :: IO ConnectionsVar
    testConnectionsVar = STM.newTVarIO def

    testAddrCIdHashes :: IO AddrCIdHashes
    testAddrCIdHashes = AddrCIdHashes <$> newIORef mempty

    testSendActions :: IO (STM.TMVar (SendActions WalletWebMode))
    testSendActions = STM.newEmptyTMVarIO

    -- For some categories of tests we won't hit the 'RealModeContext', so that's safe
    -- for now to leave it unimplemented.
    testRealModeContext :: IO (RealModeContext WalletMempoolExt)
    testRealModeContext = return (error "testRealModeContext is currently unimplemented")

-- Our API apparently is returning JSON Arrays which is considered bad practice as very old
-- browsers can be hacked: https://haacked.com/archive/2009/06/25/json-hijacking.aspx/
-- The general consensus, after discussing this with the team, is that we can be moderately safe.
spec :: Spec
spec = withCompileInfo def $ do
    withDefConfigurations $ do
      xdescribe "Servant API Properties" $ do
        it "V0 API follows best practices & is RESTful abiding" $ do
          withServantServer (Proxy @V0.API) v0Server $ \burl ->
            serverSatisfies (Proxy @V0.API) burl stdArgs predicates
        it "V1 API follows best practices & is RESTful abiding" $ do
          withServantServer (Proxy @V1.API) v1Server $ \burl ->
            serverSatisfies (Proxy @V1.API) burl stdArgs predicates
