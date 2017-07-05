module BigNumber
    ( BIGNUMBER
    , BigNumber
    , BigNumberFormat(..)
    , defaultFormat
    , dividedByInt
    , fromString
    , format
    , toFormat
    , toString
    , toString'
    )
    where

import Prelude
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn3, runEffFn1, runEffFn3)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)

-- | BigNumber effect
foreign import data BIGNUMBER :: Effect
-- | An reeeaaaaally big integer
foreign import data BigNumber :: Type

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

foreign import formatImpl :: forall eff. EffFn1 (bigNumber :: BIGNUMBER | eff) BigNumberFormat Unit

format :: forall eff. BigNumberFormat -> Eff (bigNumber :: BIGNUMBER | eff) Unit
format = runEffFn1 formatImpl

-- | `toFormat` function using `BigNumberFormat` and
-- | a given value of decimal place
foreign import toFormatImpl :: forall eff. EffFn3 (bigNumber :: BIGNUMBER | eff) BigNumber BigNumberFormat Int String

toFormat :: forall eff. BigNumber -> BigNumberFormat -> Int -> Eff (bigNumber :: BIGNUMBER | eff) String
toFormat = runEffFn3 toFormatImpl
