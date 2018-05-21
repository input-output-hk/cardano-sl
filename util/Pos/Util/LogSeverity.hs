
module Pos.Util.LogSeverity
       ( Severity(..)
       ) where


import           Universum



-- | abstract libraries' severity
data Severity = Debug | Info | Warning | Notice | Error
                deriving (Show)

