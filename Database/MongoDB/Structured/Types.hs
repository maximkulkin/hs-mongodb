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

class SerializedValue a where
  toBSON :: a -> Bson.Value
  fromBSON :: Bson.Value -> Maybe a

instance SerializedValue Char where
  toBSON = Bson.val
  fromBSON = Bson.cast'

instance SerializedValue String where
  toBSON = Bson.val
  fromBSON = Bson.cast'

instance SerializedValue Int where
  toBSON = Bson.val
  fromBSON = Bson.cast'

instance SerializedValue Bson.ObjectId where
  toBSON = Bson.val
  fromBSON = Bson.cast'

-- instance SerializedValue a => SerializedValue [a] where
--   toBSON = Bson.Array . map toBSON
--   fromBSON (Bson.Array arr) = mapM fromBSON arr
--   fromBSON _ = Nothing


class (SerializedValue (Key rec), Eq (Key rec), Ord (Key rec), Show (Key rec)) => SerializedEntity rec where
  type Key rec
  data EntityField rec :: * -> *

  idField :: EntityField rec (Key rec)
  collectionName :: rec -> MongoDB.Collection
  fieldName :: forall typ. EntityField rec typ -> Bson.Label

  toBSONDoc :: rec -> Bson.Document
  fromBSONDoc :: Bson.Document -> Maybe rec


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

entityFromBSONDoc :: forall rec. SerializedEntity rec => Bson.Document -> Maybe (Entity rec)
entityFromBSONDoc doc = Entity <$> doc .: fieldName (idField :: EntityField rec (Key rec)) <*> fromBSONDoc doc


infix 5 =:, .:
(.:) :: (SerializedValue a) => Bson.Document -> Bson.Label -> Maybe a
doc .: name = Bson.look name doc >>= fromBSON

(=:) :: (SerializedValue a) => Bson.Label -> a -> Bson.Field
name =: value = name Bson.:= toBSON value
