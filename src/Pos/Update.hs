-- | Functions for updating the application.

module Pos.Update
       ( downloadHash
       ) where

import qualified Data.ByteArray          as BA
import           Data.String             (String)
import           Formatting              (formatToString, (%))
import           Network.HTTP.Client     (Manager, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Simple     (getResponseBody, getResponseStatus,
                                          getResponseStatusCode, httpLBS, parseRequest,
                                          setRequestManager)
import qualified Serokell.Util.Base16    as B16
import           Serokell.Util.Text      (listJsonIndent)
import           System.FilePath         ((</>))
import           Universum

import           Pos.Constants           (updateServers)
import           Pos.Crypto              (Hash, hash)

-- | Download a file by its hash.
--
-- Tries all servers in turn, fails if none of them work.
downloadHash :: Hash LByteString -> IO (Either String LByteString)
downloadHash h = do
    manager <- newManager tlsManagerSettings

    let -- try all servers in turn until there's a Right
        go errs (serv:rest) = do
            let uri = serv </> toString (B16.encode (BA.convert h))
            downloadUri manager uri h >>= \case
                Left e -> go (e:errs) rest
                Right r -> return (Right r)

        -- if there were no servers, that's really weird
        go [] [] = panic "no update servers are known"

        -- if we've tried all servers already, fail
        go errs [] = return . Left $
            formatToString ("all update servers failed: "%listJsonIndent 2)
                           (reverse errs)

    go [] updateServers

-- | Download a file and check its hash.
downloadUri :: Manager
            -> String
            -> Hash LByteString
            -> IO (Either String LByteString)
downloadUri manager uri h = do
    request <- setRequestManager manager <$> parseRequest uri
    resp <- httpLBS request
    let (st, stc) = (getResponseStatus resp, getResponseStatusCode resp)
        h' = hash (getResponseBody resp)
    return $ if | stc /= 200 -> Left ("error, " ++ show st)
                | h /= h'    -> Left "hash mismatch"
                | otherwise  -> Right (getResponseBody resp)

{- TODO
=======

* check timeouts?
* how should we in general deal with e.g. 1B/s download speed?
* if we expect updates to be big, use laziness/conduits (httpLBS isn't lazy,
  despite the “L” in its name)

-}
