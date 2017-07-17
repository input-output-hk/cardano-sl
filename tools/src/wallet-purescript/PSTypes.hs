module PSTypes
    ( psPosixTime
    , psInt53
    ) where

import           Language.PureScript.Bridge.TypeInfo (PSType, TypeInfo (..))

psPosixTime :: PSType
psPosixTime = TypeInfo "" "Data.Types" "NominalDiffTime" []

psInt53 :: PSType
psInt53 = TypeInfo "" "Data.Int53" "Int53" []
