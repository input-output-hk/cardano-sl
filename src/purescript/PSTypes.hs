module PSTypes
    ( psPosixTime
    ) where

import           Language.PureScript.Bridge.TypeInfo (PSType, TypeInfo (..))

psPosixTime :: PSType
psPosixTime = TypeInfo "" "Data.Types" "NominalDiffTime" []
