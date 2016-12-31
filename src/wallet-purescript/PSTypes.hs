module PSTypes
    ( psPosixTime
    , psHash
    ) where

import           Language.PureScript.Bridge.TypeInfo (PSType, TypeInfo (..))

psPosixTime :: PSType
psPosixTime = TypeInfo "" "Data.Types" "NominalDiffTime" []

psHash :: PSType
psHash = TypeInfo "" "Data.Types" "Hash" []
