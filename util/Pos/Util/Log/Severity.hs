
module Pos.Util.Log.Severity
       ( Severity(..)
       ) where


import           Universum



-- | abstract libraries' severity
data Severity = Debug | Info | Warning | Notice | Error
                deriving (Show)

