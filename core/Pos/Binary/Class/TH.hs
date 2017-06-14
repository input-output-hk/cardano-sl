{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | TH helpers for Bi
module Pos.Binary.Class.TH
       ( deriveSimpleBi
       , Cons (..)
       , Field (..)
       ) where

import           Universum
import           Unsafe                (unsafeTail)

import           Data.Default          (def)
import           Data.List             (notElem, nubBy, zipWith3)
import           Data.Store            (Size (..))
import qualified Data.Text             as T
import           Formatting            (sformat, shown, (%))
import           Language.Haskell.TH
import           TH.ReifySimple        (DataCon (..), DataType (..), reifyDataType)
import           TH.Utilities          (plainInstanceD)

import           Pos.Binary.Class.Core (Bi (..))
import qualified Pos.Binary.Class.Core as Bi

data Cons
    = Cons {
      cName   :: Name
    , cFields :: [Field]
    }

data Field
    = Field {
      fName :: Name
    , fType :: Name
    }
    -- ^ Field name and field type
    | Unused Name
    -- ^ Name of unused field

-- Some part of code copied from
-- https://hackage.haskell.org/package/store-0.4.3.1/docs/src/Data-Store-TH-Internal.html#makeStore
deriveSimpleBi :: Name -> [Cons] -> Q [Dec]
deriveSimpleBi headTy constrs = do
    when (null constrs) $
        failText "You passed no constructors to deriveSimpleBi"
    when (length constrs > 255) $
        failText "You passed too many constructors to deriveSimpleBi"
    when (length (nubBy (\x y -> cName x == cName y) constrs) /= length constrs) $
        failText "You passed two constructors with the same name"
    dt <- reifyDataType headTy
    case matchAllConstrs constrs (dtCons dt) of
        MissedCons cons ->
            failText .
                sformat ("Constructor '"%shown%"' isn't passed to deriveSimpleBi") $
                cons
        UnknownCons cons ->
            failText .
                sformat ("Unknown constructor '"%shown%"' is passed to deriveSimpleBi") $
                cons
        MatchedCons matchedConstrs ->
            forM_ (zip constrs matchedConstrs) $ \(Cons{..}, DataCon{..}) -> do
                let realFields = mapMaybe (\(n, t) -> (,t) <$> n) dcFields
                when (length realFields /= length dcFields) $
                    failText $ sformat ("Some field of "%shown
                                       %" constructor doesn't have a explicit name") cName
                case checkAllFields cFields realFields of
                    MissedField field ->
                        failText $ sformat ("Field '"%shown%"' of the constructor '"
                                            %shown%"' isn't passed to deriveSimpleBi")
                                   field cName
                    UnknownField field ->
                        failText $ sformat ("Unknown field '"%shown%"' of the constructor '"
                                            %shown%"' is passed to deriveSimpleBi")
                                   field cName
                    TypeMismatched field realType passedType ->
                        failText $ sformat ("The type of '"%shown%"' of the constructor '"
                                            %shown%"' is mismatched: real type '"
                                            %shown%"', passed type '"%shown%"'")
                                   field cName realType passedType
                    MatchedFields -> pass
    ty <- conT headTy
    makeBiInstanceTH ty <$> biSizeExpr <*> biPutExpr <*> biGetExpr
  where
    -- Meta information about constructors --

    -- Total numbers of field in the each constructor
    numOfFields :: [Int]
    numOfFields = map (\Cons{..} -> length cFields) constrs

    -- Constructor and its used fields.
    filteredConstrs :: [Cons]
    filteredConstrs = map (\Cons{..} -> Cons cName (filter isUsed cFields)) constrs

    -- All used fields:
    -- [Fields of Constr1 [(FieldName, FieldType)],  Fields of Constr2 [(FieldName, FieldType)], ..]
    -- @filteredConstrs@ and @allUsedFields@ are almost same,
    -- but @filteredConstrs@ store the name of constructors.
    allUsedFields :: [[(Name, Name)]]
    allUsedFields = map (\Cons{..} -> map (\Field{..} -> (fName, fType)) cFields) filteredConstrs

    -- Useful variables for @size@, @put@, @get@ --
    tagType :: Name
    tagType = ''Word8

    tagSize :: Int
    tagSize = 1

    -- Useful variable for case statement in the @size@ and @put@.
    valName :: Name
    valName = mkName "val"

    -- Helpers --
    failText :: MonadFail m => T.Text -> m a
    failText = fail . T.unpack

    isUsed :: Field -> Bool
    isUsed (Unused _) = False
    isUsed _          = True

    appendName :: Name -> Name -> Name
    appendName a b = mkName $ show a <> show b

    prependPrefToFields :: [Name] -> [Name]
    prependPrefToFields prefixes =
        concatMap (\(p, flds) -> map ((p `appendName`) . fst) flds) $
                  zip prefixes allUsedFields

    -- Put definition --
    biPutExpr :: Q Exp
    biPutExpr = lamE [varP valName] $
        caseE (varE valName) $ zipWith3 biPutConstr numOfFields [0..] filteredConstrs

    biPutConstr :: Int -> Int -> Cons -> MatchQ
    biPutConstr num ix (Cons cName cFields) = do
        let wilds = replicate num wildP
        match (conP cName wilds) body []
      where
        body = normalB $
            if length constrs >= 2 then
                doE (putTag ix : map putField cFields)
            else
                doE (map putField cFields)

    putTag :: Int -> Q Stmt
    putTag ix = noBindS [| poke (ix :: $(conT tagType)) |]

    putField :: Field -> Q Stmt
    putField Field{..}  = noBindS [| Bi.put $(appE (varE fName) (varE valName)) |]
    putField (Unused _) = fail "Something went wrong: put Unused field"

    -- Size definition --
    biSizeExpr :: Q Exp
    biSizeExpr = caseE (tupE (concatMap (map (sizeAtType . snd)) allUsedFields))
                       [matchConstSize, matchVarSize]

    -- Generate code like "size :: Word8", "size :: Int"
    sizeAtType :: Name -> ExpQ
    sizeAtType ty = [| size :: Size $(conT ty) |]

    -- Variables for total size of used fields in the each constructor.
    sizeNames :: [Name]
    sizeNames = take (length constrs) $
                      map (mkName . ("sz" ++) . show) ([0..] :: [Int])

    -- Prefixes of fields (corresponding to each constructor)
    shortNames :: [Name]
    shortNames = take (length constrs) $
                      map (mkName . ("c" ++) . show) ([0..] :: [Int])

    -- Generate the following code:
    -- (ConstSize sz0f0,
    -- ConstSize sz1f0, ConstSize sz1f1, ConstSize sz1f2,
    -- ConstSize sz2f2) | sz0 == sz1 -> ConstSize (1 + sz0)
    --   where
    --     sz0 = sz0f0
    --     sz1 = sz1f0 + sz1f1 + sz1f2
    --     sz2 = sz2f0
    matchConstSize :: MatchQ
    matchConstSize = do
        let sz0 = VarE (mkName "sz0")
        let flatUsedFields = prependPrefToFields sizeNames
        let sumOfFieldsDecls = zipWith constrSumOfFields sizeNames allUsedFields
        -- Generate sz0 == sz1 | sz0 == sz2 | ..
        sameSizeExpr <-
            foldl (\l r -> [| $(l) && $(r) |]) [| True |] $
            map (\szn -> [| $(return sz0) == $(varE szn) |]) $
            unsafeTail sizeNames
        result <- [| ConstSize (tagSize + $(return sz0)) |]
        match (tupP (map (conP 'ConstSize . one . varP) flatUsedFields))
              (guardedB [return (NormalG sameSizeExpr, result)])
              sumOfFieldsDecls

    -- Generate the following code:
    -- sz0 = sz0f1 + sz0f2 + sz0f3.
    constrSumOfFields :: Name -> [(Name, Name)] -> DecQ
    constrSumOfFields szn [] = valD (varP szn) (normalB [| 0 |]) []
    constrSumOfFields szn (map fst -> names) = valD (varP szn) body []
      where
        body = normalB $
            foldl1 (\l r -> [| $(l) + $(r) |]) $
            map (varE . (szn `appendName`)) names

    -- Generate the following code:
    -- (c0f0, c0f1, c1f0) -> VarSize $ \x -> 1 +
    --    case x of
    --        Bar {..} -> getSizeWith c0f0 f0 + getSizeWith c0f1 f1
    --        Baz {..} -> getSizeWith c1f0 f0
    matchVarSize :: MatchQ
    matchVarSize = do
        let flatUsedFields = prependPrefToFields shortNames
        match (tupP (map varP flatUsedFields))
              (normalB
                 [| VarSize $ \val ->
                        tagSize + $(caseE [| val |]
                                          (zipWith3 matchVarCons shortNames numOfFields filteredConstrs)) |])
              []

    -- Generate the following code:
    -- Bar _ _ _ -> getSizeWith c0field1 (field1 val) + getSizeWith c0field2 (field2 val)
    matchVarCons :: Name -> Int -> Cons -> MatchQ
    matchVarCons _ _ (Cons cName [])  = match (conP cName []) (normalB [| 0 |]) []
    matchVarCons shortName num (Cons cName (map fName -> cFields)) = do
        let wilds = replicate num wildP
        match (conP cName wilds) body []
      where
        body = normalB $
            foldl1 (\l r -> [| $(l) + $(r) |]) $
            map (\fn -> [| getSizeWith $(varE (shortName `appendName` fn)) $(appE (varE fn) (varE valName)) |]) cFields

    -- Get definition --
    biGetExpr :: Q Exp
    biGetExpr = case constrs of
        []        ->
            failText $ sformat ("Attempting to peek type without constructors "%shown) headTy
        (cons:[]) ->
            biGetConstr cons -- There is one constructor
        _         -> do
            let tagName = mkName "tag"
            let getMatch (ix, con) = match (litP (IntegerL ix)) (normalB (biGetConstr con)) []
            let mismatchConstr =
                    match wildP (normalB
                        [| peekException (sformat ("Found invalid tag while getting "%build) headTy) |]) []
            doE
                [ bindS (varP tagName) [| Bi.get |]
                , noBindS (caseE
                                (sigE (varE tagName) (conT tagType))
                                (map getMatch (zip [0..] constrs) ++ [mismatchConstr]))
                ]

    biGetConstr :: Cons -> Q Exp
    biGetConstr (Cons name []) = appE (varE 'pure) (conE name)
    biGetConstr Cons{..} = do
        let usedFields = filter isUsed cFields
        let unusedFields = filter (not . isUsed) cFields

        varNames :: [Name] <- mapM (newName . show . fName) usedFields
        varPs :: [Pat] <- mapM varP varNames
        biGets :: [Exp] <- replicateM (length varPs) [| Bi.get |]
        bindExprs :: [Stmt] <- mapM (uncurry bindS . bimap pure pure) (zip varPs biGets)
        let recWildUsedVars = map (\(f, ex) -> (fName f,) <$> varE ex) $ zip usedFields varNames
        let recWildUnusedVars = map (\f -> (fName f,) <$> [| def |]) unusedFields
        recordWildCardReturn <- noBindS $
                                appE (varE 'pure) $
                                recConE cName $
                                recWildUsedVars ++ recWildUnusedVars
        doE $ map pure bindExprs ++ [pure recordWildCardReturn]

makeBiInstanceTH :: Type -> Exp -> Exp -> Exp -> [Dec]
makeBiInstanceTH ty sizeE putE getE = one $
  plainInstanceD
        [] -- empty context
        (AppT (ConT ''Bi) ty)
        [ ValD (VarP 'size) (NormalB sizeE) []
        , ValD (VarP 'put) (NormalB putE) []
        , ValD (VarP 'get) (NormalB getE) []
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
        let ret = catMaybes $ map (\x -> find ((x==) . dcName) realCons) passedNames in
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

checkAllFields :: [Field] -> [(Name, Type)] -> MatchFields
checkAllFields (map (\Field{..} -> (fName, ConT fType)) -> passedFields) realFields
    | Just nm <- map fst passedFields `inclusion` map fst realFields = UnknownField nm
    | Just nm <- map fst realFields `inclusion` map fst passedFields = MissedField nm
    | otherwise =
        let ret = catMaybes $ map (\x -> find ((fst x ==) . fst) realFields) passedFields in
        if length ret /= length passedFields then
            error "Something went wrong. Matched list of fields has different length"
        else
            case dropWhile checkTypes (zip realFields passedFields) of
                []                           -> MatchedFields
                (((n, real), (_, passed)):_) -> TypeMismatched n real passed
  where
    checkTypes :: ((Name, Type), (Name, Type)) -> Bool
    checkTypes ((_, t1), (_, t2)) = t1 == t2

    inclusion :: [Name] -> [Name] -> Maybe Name
    inclusion c1 c2 = find (`notElem` c2) c1

