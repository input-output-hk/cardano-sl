{-
TH helpers for Bi.

Suppose you have the following datatype:

data User
    = Login {
      login :: String
    , age   :: Int
    }
    | FullName {
      firstName  :: String
    , lastName   :: String
    , sex        :: Bool
    }

then the following deriveSimpleBi:

deriveSimpleBi ''User [
    Cons 'Login [
        Field [| login :: String |],
        Field [| age   :: Int    |],
    ],
    Cons 'FullName [
        Field [| firstName :: String |],
        Field [| lastName  :: String |],
        Field [| sex       :: Bool   |]
    ]]

will generate:

instance Bi User where
    encode = \x -> case x of
        val@Login{} -> encodeListLen 3 <> encode (0 :: Word8)
                                       <> encode (login val)
                                       <> encode (age val)
        val@FullName{} -> encodeListLen 3 <> encode (1 :: Word8)
                                          <> encode (firstName val)
                                          <> encode (sex val)
    decode = do
        expectedLen <- decodeListLenCanonical
        tag <- decode @Word8
        case tag of
            0 -> do
                matchSize 3 "Login" expectedLen
                login <- decode
                age <- decode
                pure $ Login {..}
            1 -> do
                matchSize 3 "FullName" expectedLen
                firstName <- decode
                lastName  <- decode
                sex       <- decode
                pure $ FullName {..}
            _ -> cborError "Found invalid tag while getting User"
-}

module Pos.Binary.Class.TH
       ( deriveSimpleBi
       , deriveSimpleBiCxt
       , Cons (..)
       , Field (Field)
       ) where

import           Universum hiding (Type)

import qualified Codec.CBOR.Decoding as Cbor
import qualified Codec.CBOR.Encoding as Cbor
import           Control.Lens (imap)
import           Data.List (nubBy)
import           Formatting (sformat, shown, (%))
import           Language.Haskell.TH
import           TH.ReifySimple (DataCon (..), DataType (..), reifyDataType)
import           TH.Utilities (plainInstanceD)

import qualified Pos.Binary.Class.Core as Bi

-- | This function must match the one from 'Pos.Util.Util'. It is copied here
-- to avoid a dependency and facilitate parallel builds.
toTemplateHaskellError :: Either Text a -> Q a
toTemplateHaskellError = either (fail . toString) return

templateHaskellError :: Text -> Q a
templateHaskellError = toTemplateHaskellError . Left

data Cons = Cons
    { -- | Name of a constructor.
      cName   :: Name
      -- | Field of a constructor.
    , cFields :: [Field]
    }

data Field
    = Field {
    -- | The constructor means that you want
    -- a field to participate in serialisation/deserialization
      fFieldAndType :: ExpQ
    -- ^ You're expected to write something like @[|foo :: Bar|]@ here
    }

-- | Turn something like @[|foo :: Bar|]@ into @(foo, Bar)@.
expToNameAndType :: ExpQ -> Q (Name, Type)
expToNameAndType ex = ex >>= \case
    SigE (VarE n) t -> pure (n, t)
    other           -> templateHaskellError $
        "expToNameAndType: the expression should look \
        \like [|fname :: FType|], but it doesn't: " <> show other

fieldToPair :: Field -> Q (Name, Maybe Type)
fieldToPair (Field ex)  = over _2 Just <$> expToNameAndType ex

-- Some part of code copied from
-- https://hackage.haskell.org/package/store-0.4.3.1/docs/src/Data-Store-TH-Internal.html#makeStore

-- | Takes the name of datatype and constructors of datatype and generates Bi instances.
-- You should pass all constructors explicitly. Also, you should pass all fields explicitly.
-- and the real type of field and the passed (in the Field) type should be same.
-- All field of datatype should be named explicitly.
-- The numbers of constructors must be at least one and at most 255.
-- The order of fields matter: it corresponds to order of put's and get's.
-- If some of these statements is violated,
-- you will get compile error with the corresponding message.
deriveSimpleBi :: Name -> [Cons] -> Q [Dec]
deriveSimpleBi = deriveSimpleBiInternal Nothing

deriveSimpleBiCxt :: TypeQ -> Name -> [Cons] -> Q [Dec]
deriveSimpleBiCxt = deriveSimpleBiInternal . Just

deriveSimpleBiInternal :: Maybe TypeQ -> Name -> [Cons] -> Q [Dec]
deriveSimpleBiInternal predsMB headTy constrs = do
    when (null constrs) $
        templateHaskellError "You passed no constructors to deriveSimpleBi"
    when (length constrs > 255) $
        templateHaskellError "You passed too many constructors to deriveSimpleBi"
    when (length (nubBy ((==) `on` cName) constrs) /= length constrs) $
        templateHaskellError "You passed two constructors with the same name"
    preds <- maybe (pure []) (fmap one) predsMB
    dt <- reifyDataType headTy
    case matchAllConstrs constrs (dtCons dt) of
        MissedCons cons -> templateHaskellError $
            sformat ("Constructor '"%shown%"' isn't passed to deriveSimpleBi") $
            cons
        UnknownCons cons -> templateHaskellError $
            sformat ("Unknown constructor '"%shown%"' is passed to deriveSimpleBi") $
            cons
        MatchedCons matchedConstrs ->
            forM_ (zip constrs matchedConstrs) $ \(Cons{..}, DataCon{..}) -> do
                let realFields = mapMaybe (\(n, t) -> (,t) <$> n) dcFields
                when (length realFields /= length dcFields) $ templateHaskellError $
                    sformat ("Some field of "%shown
                    %" constructor doesn't have an explicit name") cName
                cResolvedFields <- mapM fieldToPair cFields
                let fieldCheck = checkAllFields cResolvedFields realFields
                case fieldCheck of
                    MatchedFields -> return ()
                    MissedField field -> templateHaskellError $
                        sformat ("Field '"%shown%"' of the constructor '"
                                %shown%"' isn't passed to deriveSimpleBi")
                        field cName
                    UnknownField field -> templateHaskellError $
                        sformat ("Unknown field '"%shown%"' of the constructor '"
                                %shown%"' is passed to deriveSimpleBi")
                        field cName
                    TypeMismatched field realType passedType -> templateHaskellError $
                        sformat ("The type of '"%shown%"' of the constructor '"
                                %shown%"' is mismatched: real type '"
                                %shown%"', passed type '"%shown%"'")
                        field cName realType passedType
    ty <- conT headTy
    makeBiInstanceTH preds ty <$> biEncodeExpr <*> biDecodeExpr
  where
    shortNameTy :: Text
    shortNameTy = toText $ nameBase headTy
    -- Meta information about constructors --

    -- Constructor and its used fields.
    filteredConstrs :: [Cons]
    filteredConstrs = map (\Cons{..} -> Cons cName cFields) constrs

    -- Useful variables for @size@, @put@, @get@ --
    tagType :: TypeQ
    tagType = [t| Word8 |]

    -- Decode definition --
    biEncodeExpr :: Q Exp
    biEncodeExpr = do
        x <- newName "x"
        lam1E (varP x) $
          caseE (varE x) $
              imap biEncodeConstr filteredConstrs

    -- Generate the following code:
    -- val@Constr{} -> encodeListLen 4
    --              <> encode (3 :: Word8)
    --              <> encode (field1 val)
    --              <> encode (field2 val)
    --              <> encode (field3 val)
    biEncodeConstr :: Int -> Cons -> MatchQ
    biEncodeConstr ix (Cons cName cFields) = do
        val <- newName $ if null cFields then "_" else "val"
        match (asP val (recP cName [])) (body (varE val)) []
      where
        body val = normalB $
            if length constrs >= 2 then
                mconcatE (encodeFlat (length cFields + 1) : encodeTag ix : map (encodeField val) cFields)
            else
                mconcatE (encodeFlat (length cFields) : map (encodeField val) cFields)

    -- Ensure the encoding of constructors with multiple arguments are encoded as a flat term.
    encodeFlat :: Int -> Q Exp
    encodeFlat listLen = [| Cbor.encodeListLen listLen |]

    encodeTag :: Int -> Q Exp
    encodeTag ix = [| Bi.encode (ix :: $tagType) |]

    encodeField :: ExpQ -> Field -> Q Exp
    encodeField val Field{..} = do
        (fName, _) <- expToNameAndType fFieldAndType
        [| Bi.encode ($(varE fName) $val) |]

    actualLen :: Name
    actualLen = mkName "actualLen"

    -- Decode definition --
    biDecodeExpr :: Q Exp
    biDecodeExpr = case constrs of
        []     -> templateHaskellError $
            sformat ("Attempting to decode type without constructors "%shown) headTy
        [cons] -> do
          doE [ bindS (varP actualLen)  [| Cbor.decodeListLenCanonical |]
              , noBindS (biDecodeConstr cons) -- There is one constructor
              ]
        _      -> do
            let tagName = mkName "tag"
            let getMatch ix con = match (litP (IntegerL (fromIntegral ix)))
                                              (normalB (biDecodeConstr con)) []
            let mismatchConstr =
                    match wildP (normalB
                        [| cborError $ "Found invalid tag while decoding " <> shortNameTy |]) []
            doE
                [ bindS (varP actualLen)  [| Cbor.decodeListLenCanonical |]
                , bindS (varP tagName)    [| Bi.decode |]
                , noBindS (caseE
                                (sigE (varE tagName) tagType)
                                (imap getMatch constrs ++ [mismatchConstr]))
                ]

    biDecodeConstr :: Cons -> Q Exp
    biDecodeConstr (Cons name []) = do
        let expectedLen          = varE actualLen
        let prettyName :: String = show name
        let tagsIfAny :: Int = if length constrs >= 2 then 1 else 0
        doE [ noBindS [| Bi.matchSize tagsIfAny ("biDecodeConstr(no_fields)@" <> prettyName) $expectedLen |]
            , noBindS (appE (varE 'pure) (conE name))
            ]
    biDecodeConstr Cons{..} = do
        let usedFieldsNum        = length cFields
        let prettyName :: String = show cName
        let expectedLen          = varE actualLen
        -- We need to take into account the possibility we are dealing
        -- with a "tagged" encoding or not.
        let tagsIfAny :: Int = if length constrs >= 2 then 1 else 0

        fieldNames :: [Name] <- mapM (fmap fst . fieldToPair) cFields
        varNames   :: [Name] <- mapM (newName . nameBase) fieldNames
        varPs :: [Pat] <- mapM varP varNames
        biGets :: [Exp] <- replicateM (length varPs) [| Bi.decode |]
        bindExprs :: [Stmt] <- mapM (uncurry bindS . bimap pure pure) (zip varPs biGets)
        let recWildUsedVars = map (\(f, ex) -> (f,) <$> varE ex) $ zip fieldNames varNames
        -- The +1 is because we need to take into account the tag we used to discriminate on the
        -- different constructors in the encoding phase.
        let lenCheck =
                noBindS [| Bi.matchSize (usedFieldsNum + tagsIfAny) ("biDecodeConstr@" <> prettyName) $expectedLen |]
        recordWildCardReturn <- noBindS $
                                appE (varE 'pure) $
                                recConE cName $
                                recWildUsedVars
        doE $ lenCheck : (map pure bindExprs ++ [pure recordWildCardReturn])

makeBiInstanceTH :: Cxt -> Type -> Exp -> Exp -> [Dec]
makeBiInstanceTH preds ty encodeE decodeE = one $
  plainInstanceD
        preds -- context
        (AppT (ConT ''Bi.Bi) ty)
        [ ValD (VarP 'Bi.encode) (NormalB encodeE) []
        , ValD (VarP 'Bi.decode) (NormalB decodeE) []
        ]

data MatchConstructors
    = MatchedCons [DataCon]
    -- ^ Constructors in matched order
    | MissedCons Name
    -- ^ Some constructor aren't passed
    | UnknownCons Name
    -- ^ Passed unknown constructor

matchAllConstrs :: [Cons] -> [DataCon] -> MatchConstructors
matchAllConstrs (map cName -> passedNames) realCons@(map dcName -> realNames)
    | Just nm <- passedNames `inclusion` realNames = UnknownCons nm
    | Just nm <- realNames `inclusion` passedNames = MissedCons nm
    | otherwise =
        let ret = mapMaybe (\x -> find ((x==) . dcName) realCons) passedNames in
        if length ret /= length passedNames then
            error "Something went wrong. Matched list of constructors has different length"
        else
            MatchedCons ret
  where
    inclusion :: [Name] -> [Name] -> Maybe Name
    inclusion c1 c2 = find (`notElem` c2) c1

data MatchFields
    = MatchedFields
    -- ^ All fields are matched
    | MissedField Name
    -- ^ Some field aren't passed
    | UnknownField Name
    -- ^ Passed field with unknown name
    | TypeMismatched Name Type Type
    -- ^ Some field has mismatched type

checkAllFields :: [(Name, Maybe Type)] -> [(Name, Type)] -> MatchFields
checkAllFields passedFields realFields
    | Just nm <- map fst passedFields `inclusion` map fst realFields = UnknownField nm
    | Just nm <- map fst realFields `inclusion` map fst passedFields = MissedField nm
    | otherwise =
        let ret = mapMaybe (\x -> find ((fst x ==) . fst) realFields) passedFields in
        if length ret /= length passedFields then
            error "Something went wrong. Matched list of fields has different length"
        else
            case dropWhile checkTypes (zip realFields passedFields) of
                []                                -> MatchedFields
                (((n, real), (_, Just passed)):_) -> TypeMismatched n real passed
                (((_, _), (_, Nothing)):_)        -> error "Something went wrong: illegal mismatch type"
  where
    checkTypes :: ((Name, Type), (Name, Maybe Type)) -> Bool
    checkTypes (_, (_, Nothing))       = True
    checkTypes ((_, t1), (_, Just t2)) = t1 == t2

    inclusion :: [Name] -> [Name] -> Maybe Name
    inclusion c1 c2 = find (`notElem` c2) c1

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

-- | Put '(<>)' between expressions.
mconcatE :: [ExpQ] -> ExpQ
mconcatE = foldr (\a b -> infixApp a [| (<>) |] b) [| mempty |]
