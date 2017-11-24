
-- | Module for small utility functions.
module Cardano.Wallet.Util
       ( headToLower
       , stripFieldPrefix
       , mkJsonKey
       ) where

import           Universum

import           Data.Char (isUpper, toLower)

--
-- String manipulation utils
--

headToLower :: String -> Maybe String
headToLower []     = Nothing
headToLower (x:xs) = Just $ toLower x : xs

stripFieldPrefix :: String -> String
stripFieldPrefix = dropWhile (not . isUpper)

mkJsonKey :: String -> String
mkJsonKey s = fromMaybe s . headToLower $ stripFieldPrefix s
