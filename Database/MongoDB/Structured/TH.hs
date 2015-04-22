{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

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
  { mkFieldName :: String -> String -> String        -- | entity name -> field name -> String
  , mkConTag :: String -> String -> String           -- | entity name -> con name -> String
  , mkEntityFieldName :: String -> String -> String  -- | entity name -> field name -> String
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
deriveSerializedEntityWith DeriveSerializedEntityOptions{..} name = do
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
              fields = filterDuplicateFields $ ("Id", (ConT keyTypeName), "_id") : (
                foldl' (++) [] $
                  map (\con -> map (\(n, t) -> (n, t, mkFieldName entityName n)) $ conFields con) cons
                )

          let keyTypeDec = TySynD keyTypeName [] (ConT ''Bson.ObjectId)
#if MIN_VERSION_template_haskell(2, 9, 0)
          keyDec <- tySynInstD ''Key $ pure $ TySynEqn [ConT name] (ConT keyTypeName)
#else
          keyDec <- tySynInstD ''Key [pure $ ConT name] (pure $ ConT keyTypeName)
#endif
          entityFieldsDec <- mkEntityFieldsDec fields
          fieldNameBody <- mkFieldNameBody fields
          toBSONDocBody <- mkToBSONDocBody cons
          fromBSONDocBody <- mkFromBSONDocBody cons

          serializedEntityDec <- instanceD (pure []) (conT ''SerializedEntity `appT` conT name)
            [ pure keyDec
            , pure entityFieldsDec
            , funD 'collectionName [ clause [wildP] (normalB $ stringE colName) [] ]
            , funD 'idField [ clause [] (normalB $ conE (mkName $ mkEntityFieldName entityName "Id")) [] ]
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

        conName (NormalC n _)  = nameBase n
        conName (RecC n _)     = nameBase n
        conName (InfixC _ n _) = nameBase n
        conName (ForallC _ _ con) = conName con

        conFields :: Con -> [(String, Type)]
        conFields (NormalC cname fieldTypes) =
          let fieldPrefix = case nameBase cname of
                              (s:ss) -> (Char.toLower s : ss) ++ "_"
                              [] -> []
           in map (\(fIdx, (_, fType)) -> (fieldPrefix ++ show fIdx, fType)) $ zip ([0..] :: [Int]) fieldTypes
        conFields (RecC _ ts) = map (\(fName, _, fType) -> (nameBase fName, fType)) ts
        conFields (ForallC _ _ con) = conFields con
        conFields con = fail $ "Unsupported data constructor type: " ++ conName con

        filterDuplicateFields :: [(String, Type, String)] -> [(String, Type, String)]
        filterDuplicateFields = reverse . snd . foldl' (\(fieldTypes, fields) field@(fName, fType, _) ->
          let mType = Map.lookup fName fieldTypes
           in case mType of
                Nothing -> (Map.insert fName fType fieldTypes, field : fields)
                Just _ -> (fieldTypes, fields)
          ) (Map.empty, [])

        mkEntityFieldsDec :: [(String, Type, String)] -> Q Dec
        mkEntityFieldsDec fields = do
          let tv = mkName "typ"
              fieldCons = map (\(fName, fType) -> do
                                ForallC []
                                        [EqualP (VarT tv) fType]
                                        (NormalC (mkName fName) [])
                              ) $ filter ((/="").fst)
                                $ map (\(fName, fType, _) -> (mkEntityFieldName entityName fName, fType)) fields
          return $ DataInstD [] ''EntityField [ConT name, VarT tv] fieldCons []

        mkFieldNameBody :: [(String, Type, String)] -> Q [ClauseQ]
        mkFieldNameBody fields = do
          let fieldClauses = map (\(fName, _, fSerializedName) ->
                                    clause [conP (mkName $ mkEntityFieldName entityName fName) []]
                                           (normalB $ stringE fSerializedName)
                                           []
                                 ) fields
          return fieldClauses

        mkToBSONDocBody :: [Con] -> Q [ClauseQ]
        mkToBSONDocBody [con] = conToBSONDoc con False
        mkToBSONDocBody cons = fmap (foldl' (++) [] ) $ mapM (flip conToBSONDoc True) cons

        conToBSONDoc :: Con -> Bool -> Q [ClauseQ]
        conToBSONDoc con sumType = do
          let argNames = map (\(fName, _) -> mkName ("a_" ++ fName)) fields

          return [ clause [conP (mkName cname) (map varP argNames)]
                     ( normalB $ listE $
                       maybe [] (:[]) typeFieldE ++
                       map (\((field, _),argName) ->
                             [|$(stringE $ mkFieldName entityName field) =: $(varE argName)|]
                           ) (zip fields argNames)
                     ) []
                 ]
          where cname = conName con
                fields = conFields con
                typeFieldE = if sumType
                             then Just [|$(stringE sumTypeFieldName) =: ($(stringE $ mkConTag entityName cname) :: Text) |]
                             else Nothing

        mkFromBSONDocBody :: [Con] -> Q [ClauseQ]
        mkFromBSONDocBody [con] = do
          let doc = mkName "doc"
              cname = mkName $ conName con
              lookups = map (\(field, _) -> [|$(varE doc) .: $(stringE $ mkFieldName entityName field)|]) (conFields con)
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
          let typeLookup = [|$(varE doc) .: $(stringE sumTypeFieldName)|]
          return [ clause [varP doc]
                     (normalB $ caseE typeLookup $
                        (flip map cons $ \con ->
                          match (conP 'Right [litP (StringL $ mkConTag entityName (conName con))])
                                (normalB $ recFromBSONDoc con doc)
                                []
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
                recFromBSONDoc con doc = do
                  let cname = mkName $ conName con
                      lookups = map (\(field, _) -> infixE' (varE doc) '(.:) (stringE $ mkFieldName entityName field))
                                    (conFields con)
                  case lookups of
                    [] -> conE cname
                    [x] -> infixE' (conE cname) '(<$>) x
                    (x:xs) -> foldl' (\i e -> infixE' i '(<*>) e)
                                     (infixE' (conE cname) '(<$>) x)
                                     xs


deriveSerializedEntity :: Name -> Q [Dec]
deriveSerializedEntity = deriveSerializedEntityWith defaultDeriveSerializedEntityOptions


deriveSerializedValue :: Name -> Q [Dec]
deriveSerializedValue name = [d|
    instance SerializedValue $(conT name) where
      toBSON = Bson.String . T.pack . show
      fromBSON (Bson.String t) = readEither . T.unpack $ t
      fromBSON x = fail $ $( stringE $ "Invalid " ++ nameBase name ++ " value: ") ++ show x
  |]
