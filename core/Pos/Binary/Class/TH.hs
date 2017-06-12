{-# LANGUAGE TemplateHaskell #-}

-- | TH helpers for Bi
module Pos.Binary.Class.TH
       ( deriveSimpleBi
       , Cons (..)
       , Field (..)
       ) where
import           Universum

import qualified Data.Text           as T
import           Formatting          (build, sformat, shown, (%))
import           Language.Haskell.TH
import           TH.ReifySimple      (DataCon (..), DataType (..), reifyDataType)

data Cons
    = Cons {
      cName   :: Name
    , cFields :: [Field]
    }

data Field
    = Field Name Name
    -- ^ Field name and field type
    | Unused Name
    -- ^ Name of unused field

deriveSimpleBi :: Name -> [Cons] -> Q [Dec]
deriveSimpleBi dataName constrs = do
    dt <- reifyDataType dataName
    case (matchAllConstrs constrs (dtCons dt)) of
        MissedCons missedCons ->
            fail . T.unpack .
                    sformat ("Constructor '"%shown%"' isn't passed to deriveSimpleBi") $
                    missedCons
        UnknownCons unknownCons ->
            fail . T.unpack .
                    sformat ("Unknown constructor '"%shown%"' is passed to deriveSimpleBi") $
                    unknownCons
        MatchedCons matchedConstrs ->
            forM_ (zip constrs matchedConstrs) $ \(Cons{..}, DataCon{..}) -> do
                whenJust (checkAllFields cFields dcFields) $ \field ->
                    fail . T.unpack $
                            sformat ("Unknown field '"%shown%"' of constructor '"
                                     %shown%"' is passed to deriveSimpleBi")
                            field cName
    fail "not implemented yet"

data ConsFail
    = MatchedCons [DataCon]
    -- ^ Constructors in matched order
    | MissedCons Name
    | UnknownCons Name

matchAllConstrs :: [Cons] -> [DataCon] -> ConsFail
matchAllConstrs = undefined

checkAllFields :: [Field] -> [(Maybe Name, Type)] -> Maybe Name
checkAllFields = undefined

