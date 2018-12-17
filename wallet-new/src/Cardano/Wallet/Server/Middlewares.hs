{- | A collection of middlewares used by this edge node.
     @Middleware@ is a component that sits between the server and application.
     It can do such tasks as GZIP encoding or response caching.
-}

module Cardano.Wallet.Server.Middlewares
    ( withMiddlewares
    , throttleMiddleware
    , withDefaultHeader
    , unsupportedMimeTypeMiddleware
    , faultInjectionHandleIgnoreAPI
    ) where

import           Universum hiding (toStrict)

import           Data.Aeson (encode)
import           Data.Binary.Builder (fromByteString)
import           Data.ByteString.Lazy (toStrict)
import qualified Data.List as List
import           Network.HTTP.Types.Header (Header)
import           Network.HTTP.Types.Method (methodPatch, methodPost, methodPut)
import           Network.HTTP.Types.Status (status200, status415)
import           Network.Wai (Application, Middleware, Response, ifRequest,
                     modifyResponse, requestHeaders, requestMethod,
                     responseBuilder, responseLBS, responseStatus)
import qualified Network.Wai.Middleware.Throttle as Throttle


import           Cardano.Wallet.API.Response (UnsupportedMimeTypeError (..))
import           Cardano.Wallet.API.V1.Headers (applicationJson)
import qualified Cardano.Wallet.API.V1.Types as V1

import           Pos.Infra.InjectFail (FInject (..), FInjects, testLogFInject)
import           Pos.Launcher.Configuration (ThrottleSettings (..))


-- | "Attaches" the middlewares to this 'Application'.
withMiddlewares :: [Middleware] -> Application -> Application
withMiddlewares = flip $ foldr ($)

unsupportedMimeTypeMiddleware :: Middleware
unsupportedMimeTypeMiddleware =
    modifyResponse responseModifier
  where
      responseModifier :: Response -> Response
      responseModifier r
          | responseStatus r == status415 =
            responseBuilder status415
            [ ("Content-Type", "application/json") ]
            ( fromByteString .
              toStrict .
              encode $
              UnsupportedMimeTypePresent "The API expects the Content-Type's main MIME-type to be 'application/json'"
            )
          | otherwise = r


-- | Only apply a @Middleware@ to request with bodies (we don't consider
-- "DELETE" as one of them).
ifRequestWithBody :: Middleware -> Middleware
ifRequestWithBody =
    ifRequest ((`List.elem` [methodPost, methodPut, methodPatch]) . requestMethod)

-- | A @Middleware@ to throttle requests.
throttleMiddleware :: Maybe ThrottleSettings -> Middleware
throttleMiddleware Nothing app = app
throttleMiddleware (Just ts) app = \req respond -> do
    throttler <- Throttle.initThrottler
    Throttle.throttle throttleSettings throttler app req respond
  where
    throttleSettings = Throttle.defaultThrottleSettings
        { Throttle.onThrottled = \microsTilRetry ->
            let
                err = V1.RequestThrottled microsTilRetry
            in
                responseLBS (V1.toHttpErrorStatus err) [applicationJson] (encode err)
        , Throttle.throttleRate = fromIntegral $ tsRate ts
        , Throttle.throttlePeriod = fromIntegral $ tsPeriod ts
        , Throttle.throttleBurst = fromIntegral $ tsBurst ts
        }

-- | A @Middleware@ to default a specific Header when not provided
withDefaultHeader :: Header -> Middleware
withDefaultHeader header = ifRequestWithBody $ \app req send ->
    let
        headers =
            requestHeaders req

        req' =
            if any (on (==) fst header) headers then
                req
            else
                req { requestHeaders = header : headers }
    in
        app req' send

-- | A @Middleware@ to optionally turn the Application stack into a constant responder.
faultInjectionHandleIgnoreAPI :: FInjects IO -> Middleware
faultInjectionHandleIgnoreAPI fInjects app = do
  \req respond -> do
    doFail <- testLogFInject fInjects FInjIgnoreAPI
    if doFail
    then respond $ responseLBS status200 [("Content-Type", "text/plain")] "0"
    else app req respond
