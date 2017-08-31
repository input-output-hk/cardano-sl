{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, UnicodeSyntax #-}

module Parsehelper where

-- import           Control.Monad                    (mapM)
import           Data.Foldable                    (asum)
import           Data.Aeson                    as AE
import           Data.Aeson.Types              as AE
import qualified Data.ByteString.Lazy          as BL
import           Data.ByteString.Lazy.Char8       (ByteString)
import qualified Data.ByteString.Lazy.UTF8     as LBU
import           Data.Text                     as T
import           Data.Vector                   as V
import           Data.Word

data X = X Text deriving (Show)
instance FromJSON X where
  parseJSON x = asum
    [ AE.withText  "foo" (\v → pure $ X v)  x
    , AE.withArray "bar" (\v → do
                             xs ← Prelude.sequence ((parseJSON <$>) v)
                             pure . X . T.pack . LBU.toString . BL.pack . V.toList $ xs) x
    ] -- (X . T.pack . LBU.toString . BL.pack . V.toList) <$> V.mapM parseWord8 v

parseWord8 ∷ AE.Value → AE.Parser Word8
parseWord8 = AE.withText "Word8" $ \v → pure $ read (T.unpack v)

oldmain ∷ IO ()
oldmain = do
  let (input ∷ ByteString) = "[\
\[ 27, 91, 57, 50, 109, 91, 110, 111, 100, 101, 58, 68, 69, 66, 85, 71, 58, 84, 104, 114, 101, 97, 100, 73, 100, 32, 49, 48, 53, 52, 93, 32, 27, 91, 48, 109, 91, 50, 48, 49, 55, 45, 48, 56, 45, 48, 49, 32, 48, 48, 58, 49, 49, 58, 48, 54, 32, 85, 84, 67, 93, 32, 119, 97, 105, 116, 105, 110, 103, 32, 102, 111, 114, 32, 91, 93, 32, 111, 117, 116, 98, 111, 117, 110, 100, 32, 105, 110, 98, 111, 117, 110, 100 ],\
\\"lalala\"]"
      Just word8 = AE.decode "114" ∷ Maybe Word8
      Just x     = AE.decode "\"lalala\"" ∷ Maybe X
      Just xs    = AE.decode "[\"lalala\"]" ∷ Maybe [X]
      Just x'    = AE.decode "[80, 81, 82]" ∷ Maybe X
      Just final = AE.decode input ∷ Maybe [X]

  putStrLn "yay"
  putStrLn $ show word8
  putStrLn $ show x
  putStrLn $ show xs
  putStrLn $ show x'
  putStrLn $ show final
  pure ()
--
--λ> main
--yay
--114
--X "lalala"
--[X "lalala"]
--X "PQR"
--[X "\ESC[92m[node:DEBUG:ThreadId 1054] \ESC[0m[2017-08-01 00:11:06 UTC] waiting for [] outbound inbound",X "lalala"]
