{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Database.MongoDB.Structured.TH
  ( deriveSerializedEntity
  , deriveSerializedEntityWith
  , DeriveSerializedEntityOptions(..)
  , defaultDeriveSerializedEntityOptions
  , deriveSerializedValue

  , stripEntityFieldPrefix
  , capitalize
  , uncapitalize
  ) where

import Control.Applicative ((<$>), (<*>), (<|>), pure)
import qualified Data.Bson as Bson
import qualified Data.Char as Char
import Data.List (foldl', stripPrefix)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH
import Text.Read (readEither)

import Database.MongoDB.Structured.Types

capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = Char.toUpper c : cs

uncapitalize :: String -> String
uncapitalize [] = []
uncapitalize (c:cs) = Char.toLower c : cs

stripEntityFieldPrefix :: (String -> String) -> String -> String -> String
stripEntityFieldPrefix f e n = f $ fromMaybe n $ stripPrefix ('_' : uncapitalize e) n <|> stripPrefix (uncapitalize e) n

data DeriveSerializedEntityOptions = DeriveSerializedEntityOptions
  { mkFieldName :: String -> String -> String
  , mkConTag :: String -> String -> String
  , mkEntityFieldName :: String -> String -> String
  , sumTypeFieldName :: String
  }

defaultDeriveSerializedEntityOptions :: DeriveSerializedEntityOptions
defaultDeriveSerializedEntityOptions = DeriveSerializedEntityOptions
  { mkFieldName = stripEntityFieldPrefix uncapitalize
  , mkConTag = \_ n -> n
  , mkEntityFieldName = \entity field -> entity ++ (stripEntityFieldPrefix capitalize entity field)
  , sumTypeFieldName = "type"
  }

infixE' :: ExpQ -> Name -> ExpQ -> ExpQ
infixE' e1 f e2 = infixE (Just e1) (varE f) (Just e2)

deriveSerializedEntityWith :: DeriveSerializedEntityOptions -> Name -> Q [Dec]
deriveSerializedEntityWith opts name = do
  info <- reify name
  case info of
    TyConI dec ->
      case dec of
        DataD _ _ tvbs cons _ -> f tvbs cons
        other -> error $ "Unsupported type: " ++ show other
    other -> error $ "Invalid argument: " ++ show other

  where f :: [TyVarBndr] -> [Con] -> DecsQ
        f _tvbs cons = do
          let colName = map Char.toLower $ nameBase name
              keyTypeName = mkName $ nameBase name ++ "Id"
              fields = filterDuplicateFields $ (mkName "Id", (ConT keyTypeName), "_id") : (map (\(n, t) -> (n, t, formatFieldName n)) $ foldl' (++) [] $ map conFields cons)

          let keyTypeDec = TySynD keyTypeName [] (ConT ''Bson.ObjectId)
          keyDec <- tySynInstD ''Key $ pure $ TySynEqn [ConT name] (ConT keyTypeName)
          entityFieldsDec <- mkEntityFieldsDec fields
          fieldNameBody <- mkFieldNameBody fields
          toBSONDocBody <- mkToBSONDocBody cons
          fromBSONDocBody <- mkFromBSONDocBody cons

          serializedEntityDec <- instanceD (pure []) (conT ''SerializedEntity `appT` conT name)
            [ pure keyDec
            , pure entityFieldsDec
            , funD 'collectionName [ clause [wildP] (normalB $ stringE colName) [] ]
            , funD 'idField [ clause [] (normalB $ conE (formatEntityFieldName (mkName "Id"))) [] ]
            , funD 'fieldName fieldNameBody
            , funD 'toBSONDoc toBSONDocBody
            , funD 'fromBSONDoc fromBSONDocBody
            ]
          serializedValueDecs <- [d|
            instance SerializedValue $(conT name) where
              toBSON = Bson.Doc . toBSONDoc
              fromBSON (Bson.Doc doc) = fromBSONDoc doc
              fromBSON _              = fail $(stringE $ "Invalid " ++ nameBase name)
            |]

          return $ [ keyTypeDec, serializedEntityDec ] ++ serializedValueDecs

        entityName = nameBase name

        formatFieldName :: Name -> String
        formatFieldName = mkFieldName opts entityName . nameBase

        formatConName :: Name -> String
        formatConName = mkConTag opts entityName . nameBase

        formatEntityFieldName :: Name -> Name
        formatEntityFieldName = mkName . mkEntityFieldName opts entityName. nameBase

        conFields :: Con -> [(Name, Type)]
        conFields (RecC _ fields) = map (\(fName, _, fType) -> (fName, fType)) fields
        conFields _ = []

        filterDuplicateFields :: [(Name, Type, String)] -> [(Name, Type, String)]
        filterDuplicateFields = reverse . snd . foldl' (\(fieldTypes, fields) field@(fName, fType, _) ->
          let mType = Map.lookup fName fieldTypes
           in case mType of
                Nothing -> (Map.insert fName fType fieldTypes, field : fields)
                Just prevType -> if prevType /= fType
                                 then error $ "Duplicate field " ++ nameBase fName ++ " with different types"
                                 else (fieldTypes, fields)
          ) (Map.empty, [])

        mkEntityFieldsDec :: [(Name, Type, String)] -> Q Dec
        mkEntityFieldsDec fields = do
          let tv = mkName "typ"
              fieldCons = map (\(fName, fType, _) -> do
                                ForallC []
                                        [EqualP (VarT tv) fType]
                                        (NormalC (formatEntityFieldName fName) [])
                              ) fields
          return $ DataInstD [] ''EntityField [ConT name, VarT tv] fieldCons []

        mkFieldNameBody :: [(Name, Type, String)] -> Q [ClauseQ]
        mkFieldNameBody fields = do
          let fieldClauses = map (\(fName, _, fSerializedName) ->
                                    clause [conP (formatEntityFieldName fName) []]
                                           (normalB $ stringE fSerializedName)
                                           []
                                 ) fields
          return fieldClauses

        mkToBSONDocBody :: [Con] -> Q [ClauseQ]
        mkToBSONDocBody [con] = conToBSONDoc con False
        mkToBSONDocBody cons = fmap (foldl' (++) [] ) $ mapM (flip conToBSONDoc True) cons

        conToBSONDoc :: Con -> Bool -> Q [ClauseQ]
        conToBSONDoc (RecC cname ts) sumType = do
          let argNames = map (\(fName, _, _) -> mkName ("a_" ++ nameBase fName)) ts
          return [ clause [conP cname (map varP argNames)]
                     ( normalB $ listE $
                       maybe [] (:[]) typeFieldE ++
                       map (\((field, _, _),argName) ->
                             [|$(stringE $ formatFieldName field) =: $(varE argName)|]
                           ) (zip ts argNames)
                     ) []
                 ]
          where typeFieldE = if sumType
                             then Just [|$(stringE (sumTypeFieldName opts)) =: ($(stringE $ formatConName cname) :: Text) |]
                             else Nothing

        conToBSONDoc (NormalC cname []) _ = do
          return [ clause [conP cname []] (normalB $ (listE [])) [] ]
        -- TODO: implement serializing normal constructors with args

        conToBSONDoc con _ = fail $ "Unsupported data constructor type: " ++ conName con

        conName (NormalC n _)  = nameBase n
        conName (RecC n _)     = nameBase n
        conName (InfixC _ n _) = nameBase n
        conName (ForallC _ _ con) = conName con

        mkFromBSONDocBody :: [Con] -> Q [ClauseQ]
        mkFromBSONDocBody [RecC cname ts] = do
          let doc = mkName "doc"
              lookups = map (\(field, _, _) -> [|$(varE doc) .: $(stringE $ formatFieldName field)|]) ts
          return [ clause [varP doc]
                     (normalB $
                        case lookups of
                          [] -> conE cname
                          [x] -> infixE' (conE cname) '(<$>) x
                          (x:xs) -> foldl' (\i e -> [|$i <*> $e|])
                                           [|$(conE cname) <$> $x|]
                                           xs
                     ) []
                 ]

        mkFromBSONDocBody cons = do
          let doc = mkName "doc"
          e <- newName "e"
          t <- newName "t"
          let typeLookup = [|$(varE doc) .: $(stringE (sumTypeFieldName opts))|]
          return [ clause [varP doc]
                     (normalB $ caseE typeLookup $
                        (flip map cons $ \con ->
                          case con of
                            RecC cname _ -> match (conP 'Right [litP (StringL $ formatConName cname)])
                                                     (normalB $ recFromBSONDoc con doc)
                                                     []
                            NormalC cname [] -> match (conP 'Right [litP (StringL $ formatConName cname)])
                                                     (normalB $ [|return $(conE cname)|])
                                                     []
                            _ -> fail $ "Unsupported data constructor type: " ++ conName con
                        ) ++
                        [ (match (conP 'Right [varP t])
                                 (normalB $ varE 'fail `appE` (
                                     infixE' (stringE $ "Unknown " ++ (nameBase name) ++ " type: ") '(++) (varE t)
                                   )) [])
                        , (match (conP 'Left [varP e])
                                 (normalB $ [|fail $(varE e)|]) [])
                        ]
                     ) []
                 ]

          where recFromBSONDoc :: Con -> Name -> ExpQ
                recFromBSONDoc (RecC cname ts) doc = do
                  let lookups = map (\(field, _, _) -> infixE' (varE doc) '(.:) (stringE $ formatFieldName field)) ts
                  case lookups of
                    [] -> conE cname
                    [x] -> infixE' (conE cname) '(<$>) x
                    (x:xs) -> foldl' (\i e -> infixE' i '(<*>) e)
                                     (infixE' (conE cname) '(<$>) x)
                                     xs

                recFromBSONDoc (NormalC cname []) _ = conE cname
                -- TODO: implement deserializing normal constructors with args

                recFromBSONDoc _ _ = error "Non-record type where record type expected"


deriveSerializedEntity :: Name -> Q [Dec]
deriveSerializedEntity = deriveSerializedEntityWith defaultDeriveSerializedEntityOptions


deriveSerializedValue :: Name -> Q [Dec]
deriveSerializedValue name = [d|
    instance SerializedValue $(conT name) where
      toBSON = Bson.String . T.pack . show
      fromBSON (Bson.String t) = readEither . T.unpack $ t
      fromBSON x = fail $ $( stringE $ "Invalid " ++ nameBase name ++ " value: ") ++ show x
  |]
