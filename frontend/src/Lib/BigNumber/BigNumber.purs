module BigNumber
    ( BIGNUMBER
    , BigNumber
    , BigNumberFormat(..)
    , defaultFormat
    , dividedByInt
    , fromString
    , format
    , toFormat
    , toFormat'
    , toString
    , toString'
    )
    where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)

-- | BigNumber effect
foreign import data BIGNUMBER :: !
-- | An reeeaaaaally big integer
foreign import data BigNumber :: *

bigNumberBase :: Int
bigNumberBase = 10

instance sBigNumber :: Show BigNumber where
    show bn = toString bn bigNumberBase

foreign import bigNumberImpl :: forall a. Fn3 (a -> Maybe a) (Maybe a) a (Maybe BigNumber)

bigNumber :: forall a. a -> Maybe BigNumber
bigNumber value = runFn3 bigNumberImpl Just Nothing value

fromString :: String -> Maybe BigNumber
fromString = bigNumber

foreign import toStringImpl :: Fn2 BigNumber Int String

toString :: BigNumber -> Int -> String
toString = runFn2 toStringImpl

toString' :: BigNumber -> String
toString' bn = runFn2 toStringImpl bn bigNumberBase

foreign import dividedByImpl :: forall a. Fn2 BigNumber a BigNumber

dividedByInt :: BigNumber -> Int -> BigNumber
dividedByInt = runFn2 dividedByImpl

-- Format definition
-- http://mikemcl.github.io/bignumber.js/#format
newtype BigNumberFormat = BigNumberFormat
    { decimalSeparator :: String
    , groupSeparator :: String
    , groupSize :: Int
    , secondaryGroupSize :: Int
    , fractionGroupSeparator :: String
    , fractionGroupSize :: Int
    }

derive instance ntBigNumberFormat :: Newtype BigNumberFormat _

defaultFormat :: BigNumberFormat
defaultFormat = BigNumberFormat
    { decimalSeparator: "."
    , groupSeparator: ","
    , groupSize: 3
    , secondaryGroupSize: 0
    , fractionGroupSeparator: " "
    , fractionGroupSize: 0
    }

foreign import format :: forall eff. BigNumberFormat -> Eff (bigNumber :: BIGNUMBER | eff) Unit

-- | `toFormat` function by a given value of decimal place
foreign import toFormatImpl :: Fn2 BigNumber Int String
toFormat :: BigNumber -> Int -> String
toFormat = runFn2 toFormatImpl

-- | Extended `toFormat` function to use `BigNumberFormat`, too
foreign import toFormatImpl_ :: forall eff. Fn3 BigNumber BigNumberFormat Int (Eff (bigNumber :: BIGNUMBER | eff) String)
toFormat' :: forall eff. BigNumber -> BigNumberFormat -> Int -> Eff (bigNumber :: BIGNUMBER | eff) String
toFormat' = runFn3 toFormatImpl_
