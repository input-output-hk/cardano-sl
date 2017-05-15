module Daedalus.TLS
       ( FS
       , TLSOptions
       , initTLS
       ) where

-- TLS
import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.FS.Sync (readFile)
import Node.FS as FS
import Node.HTTP.Client (protocol, hostname, port, RequestOptions)
import Data.Options (opt, (:=), Option, Options)
import Node.Buffer (Buffer)

type TLSOptions = Options RequestOptions
type FS = FS.FS

-- | Certificate Authority
ca :: Option RequestOptions Buffer
ca = opt "ca"

initTLS :: forall eff. String -> Eff (fs :: FS, err :: EXCEPTION | eff) TLSOptions
initTLS caFilePath = do
    caFile <- readFile caFilePath
    pure $ daedalusTLSOptions <> ca := caFile
    -- TODO: check does this reueses openned TLS connection or is a new connection
    -- initialized on every request. If later, then we have to reuse it (look at nodejs https agents)
  where
    daedalusTLSOptions :: Options RequestOptions
    daedalusTLSOptions =
           protocol := "https"
        <> hostname := "127.0.0.1"
        <> port     := 8090
