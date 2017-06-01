module BigNumber
    ( BigNumber
    , fromString
    , toString
    , dividedBy
    )
    where

import Prelude
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Maybe (Maybe(..))

-- | An reeeaaaaally big integer
foreign import data BigNumber :: *
instance sBigNumber :: Show BigNumber where
    show = toString

foreign import bigNumberImpl :: forall a. Fn3 (a -> Maybe a) (Maybe a) a (Maybe BigNumber)

bigNumber :: forall a. a -> Maybe BigNumber
bigNumber value = runFn3 bigNumberImpl Just Nothing value

fromString :: String -> Maybe BigNumber
fromString = bigNumber

foreign import toString :: BigNumber -> String

foreign import dividedBy :: BigNumber -> BigNumber
