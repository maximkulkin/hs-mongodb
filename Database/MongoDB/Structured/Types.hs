{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Database.MongoDB.Structured.Types where

import Control.Applicative ((<$>), (<*>))
import qualified Data.Bson as Bson
import qualified Database.MongoDB as MongoDB
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime)


type Parser rec = Either String rec

runParser :: Parser rec -> Either String rec
runParser = id

class SerializedValue a where
  toBSON :: a -> Bson.Value
  fromBSON :: Bson.Value -> Parser a

bsonVal :: Bson.Val a => a -> Bson.Value
bsonVal = Bson.val

bsonCast :: Bson.Val a => String -> Bson.Value -> Parser a
bsonCast typeName = maybe (fail $ "Invalid " ++ typeName) return . Bson.cast' 

instance SerializedValue Int       where { toBSON = bsonVal ; fromBSON = bsonCast "Int" }
instance SerializedValue Int32     where { toBSON = bsonVal ; fromBSON = bsonCast "Int32" }
instance SerializedValue Int64     where { toBSON = bsonVal ; fromBSON = bsonCast "Int64" }
instance SerializedValue Integer   where { toBSON = bsonVal ; fromBSON = bsonCast "Integer" }
instance SerializedValue Double    where { toBSON = bsonVal ; fromBSON = bsonCast "Double" }
instance SerializedValue Float     where { toBSON = bsonVal ; fromBSON = bsonCast "Float" }
instance SerializedValue Text      where { toBSON = bsonVal ; fromBSON = bsonCast "Text" }
instance SerializedValue POSIXTime where { toBSON = bsonVal ; fromBSON = bsonCast "POSIX time" }
instance SerializedValue UTCTime   where { toBSON = bsonVal ; fromBSON = bsonCast "UTC time" }
instance SerializedValue Bson.ObjectId where { toBSON = bsonVal ; fromBSON = bsonCast "Object ID" }

instance SerializedValue a => SerializedValue [a] where
  toBSON = Bson.Array . map toBSON
  fromBSON (Bson.Array arr) = mapM fromBSON arr
  fromBSON x = fail $ "Expected array, got: " ++ show x

instance SerializedValue a => SerializedValue (Maybe a) where
  toBSON = maybe Bson.Null toBSON
  fromBSON Bson.Null = return Nothing
  fromBSON x = Just <$> fromBSON x


class (SerializedValue (Key rec), Eq (Key rec), Ord (Key rec), Show (Key rec)) => SerializedEntity rec where
  type Key rec
  data EntityField rec :: * -> *

  idField :: EntityField rec (Key rec)
  collectionName :: rec -> MongoDB.Collection
  fieldName :: forall typ. EntityField rec typ -> Bson.Label

  toBSONDoc :: rec -> Bson.Document
  fromBSONDoc :: Bson.Document -> Parser rec


data Entity rec = SerializedEntity rec =>
  Entity { entityKey :: Key rec
         , entityVal :: rec
         }

deriving instance (SerializedEntity rec, Eq (Key rec), Eq rec) => Eq (Entity rec)
deriving instance (SerializedEntity rec, Ord (Key rec), Ord rec) => Ord (Entity rec)
deriving instance (SerializedEntity rec, Show (Key rec), Show rec) => Show (Entity rec)
deriving instance (SerializedEntity rec, Read (Key rec), Read rec) => Read (Entity rec)


entityToBSONDoc :: forall rec. SerializedEntity rec => Entity rec -> Bson.Document
entityToBSONDoc (Entity recId record) = (fieldName (idField :: EntityField rec (Key rec)) =: recId) : toBSONDoc record

entityFromBSONDoc :: forall rec. SerializedEntity rec => Bson.Document -> Parser (Entity rec)
entityFromBSONDoc doc = Entity <$> doc .: fieldName (idField :: EntityField rec (Key rec)) <*> fromBSONDoc doc


infix 5 =:, .:
(.:) :: SerializedValue a => Bson.Document -> Bson.Label -> Parser a
doc .: name = maybe (fail $ "Field " ++ T.unpack name ++ " was not found") return (Bson.look name doc) >>= fromBSON

(=:) :: (SerializedValue a) => Bson.Label -> a -> Bson.Field
name =: value = name Bson.:= toBSON value
