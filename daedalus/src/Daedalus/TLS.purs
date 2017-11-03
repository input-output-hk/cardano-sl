module Daedalus.TLS
       ( FS
       , TLSOptions
       , initTLS
       , getWSSOptions
       , WSSOptions
       , getWSUrl
       ) where

-- TLS
import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.FS as FS
import Node.HTTP.Client (protocol, hostname, port, RequestOptions)
import Data.Options (opt, (:=), Option, Options (..))
import Data.Array (filter, head)
import Data.Tuple (fst, snd)
import Node.Buffer (Buffer)
import Data.Maybe (Maybe (..))
import Data.Foreign (readString, readInt)
import Control.Monad.Except (runExcept)
import Data.Either (either)

-- TODO: rename to HTTPSOptions
type TLSOptions = Options RequestOptions
type WSSOptions = Options RequestOptions
type FS = FS.FS

-- | Certificate Authority
ca :: Option RequestOptions Buffer
ca = opt "ca"

initTLS :: forall eff. Buffer -> Eff (fs :: FS, err :: EXCEPTION | eff) TLSOptions
initTLS caFile = do
    pure $ daedalusTLSOptions <> ca := caFile
    -- TODO: check does this reueses openned TLS connection or is a new connection
    -- initialized on every request. If later, then we have to reuse it (look at nodejs https agents)
  where
    daedalusTLSOptions :: Options RequestOptions
    daedalusTLSOptions =
           protocol := "https:"
        <> hostname := "localhost"
        <> port     := 8090

getWSSOptions :: TLSOptions -> WSSOptions
getWSSOptions (Options opts) = Options $ filter ((==) "ca" <<< fst) opts

getWSUrl :: TLSOptions -> Maybe String
getWSUrl (Options opts) = Just "wss://localhost:8090"
    -- TODO: don't hardcode ws url but instead create it from TLSOptions as shown bellow
    -- FIXME: this has to be debugged. For some reason its returning Nothing unexpectedly
    --foldl (lift2 (<>)) (Just mempty) [mProtocol, Just "//", mHostname, Just ":", mPort]
  where
    toMaybe = either (const Nothing) Just <<< runExcept
    isKey key = (==) key <<< fst
    http2ws p | p == Just "https:" = Just "wss:"
              | p == Just "http:" = Just "ws:"
              | otherwise = Nothing
    mProtocol = http2ws <<< join <<< map (toMaybe <<< readString <<< snd) <<< head $ filter (isKey "protocol") opts
    mHostname = join <<< map (toMaybe <<< readString <<< snd) <<< head $ filter (isKey "hostname") opts
    mPort = join <<< map (toMaybe <<< map show <<< readInt <<< snd) <<< head $ filter (isKey "port") opts
