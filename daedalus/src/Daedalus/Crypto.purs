module Daedalus.Crypto where

import Control.Monad.Eff (Eff)

foreign import data CRYPTO :: !

foreign import isValidMnemonic :: String -> Boolean
foreign import generateMnemonic :: forall eff. Eff (crypto :: CRYPTO | eff) String
