module Daedalus.Crypto where

import Prelude

import Data.ArrayBuffer.Types (Uint8Array)
import Data.String (replaceAll, Pattern (..), Replacement (..))

import Control.Monad.Eff (Eff)

foreign import data CRYPTO :: !

foreign import isValidMnemonic :: String -> Boolean
foreign import generateMnemonic :: forall eff. Eff (crypto :: CRYPTO | eff) String

-- TODO: make this newtypes
type DataB64 = String
type DataB64U = String

foreign import blake2b :: String -> Uint8Array

foreign import b64ToBytes :: DataB64 -> Uint8Array
foreign import bytesToB64 :: Uint8Array -> DataB64

-- RFC 4648/URL-safe alphabet conversion
--
-- A commonly used variant of Base64 encodes to URL-safe strings and thus
-- uses a slightly different alphabet:
-- ```
-- RFC 4648: "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
-- URL-safe: "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
-- ```
--
-- Note that URL-safe encoding also removes any `=` padding.
-- The following functions help to convert between the two alphabets.
toUrlSafe :: DataB64 -> DataB64U
toUrlSafe =
    replaceAll (Pattern "=") (Replacement "")
    <<< replaceAll (Pattern "/") (Replacement "_")
    <<< replaceAll (Pattern "+") (Replacement "-")
