{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Database.MongoDB.Structured.Query
  ( Filter
  , Update
  , (!.)
  -- Filter operators
  , filter
  , (==.)
  , (!=.)
  , (<.)
  , (<=.)
  , (>.)
  , (>=.)
  , (->.)
  , (!->.)
  , (||.)
  , (&&.)

  , Query(..)
  , select
  , asc
  , desc

  -- Update operators
  , update
  , (=~)
  , (+=~)
  , (-=~)
  , (*=~)
  , (/=~)

  , Cursor
  , nextBatch
  , next
  , nextN
  , rest
  , closeCursor
  , isCursorClosed

  -- Operations
  , find
  , findOne
  , count
  , insert
  , insertMany
  , save
  , modify
  , delete
  , deleteOne
  ) where

import Prelude hiding (Ordering(..), filter, mapM)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.List (foldl', groupBy)
import Data.Traversable (mapM)
import qualified Data.Text as T
import Data.Word (Word32)
import qualified Data.Bson as Bson
import qualified Database.MongoDB as MongoDB

import Database.MongoDB.Structured.Types


data OrderExp rec = forall field typ. Selectable rec field typ => Asc field
                  | forall field typ. Selectable rec field typ => Desc field

asc :: forall rec field typ. Selectable rec field typ => field -> OrderExp rec
asc = Asc

desc :: forall rec field typ. Selectable rec field typ => field -> OrderExp rec
desc = Desc

orderExpToField :: SerializedEntity rec => OrderExp rec -> Bson.Field
orderExpToField (Asc f)  = selectableFieldName f Bson.:= Bson.val (1 :: Int)
orderExpToField (Desc f) = selectableFieldName f Bson.:= Bson.val (-1 :: Int)


class Selectable r f t | f -> r, f -> t where
  selectableFieldName :: f -> Bson.Label

instance SerializedEntity rec => Selectable rec (EntityField rec typ) typ where
  selectableFieldName = fieldName


data Nested rec typ = Nested Bson.Label

instance SerializedEntity rec => Selectable rec (Nested rec typ) typ where
  selectableFieldName (Nested label) = label

(!.) :: (Selectable rec field1 typ1, Selectable typ1 field2 typ2)
     => field1 -> field2 -> Nested rec typ2
f1 !. f2 = Nested $ T.concat [ selectableFieldName f1, ".", selectableFieldName f2 ]
  

data FilterOperator = EQ | NE | GT | GE | LT | LE | IN | NIN deriving (Eq, Show)

data Filter rec = forall field typ . (SerializedValue typ, Selectable rec field typ) =>
                  Filter { filterField :: field
                         , filterOperator :: FilterOperator
                         , filterValue :: Either typ [typ]
                         }
                | FilterAnd [Filter rec]
                | FilterOr  [Filter rec]
                | FilterCustom Bson.Document

filter :: SerializedEntity rec => Bson.Document -> Filter rec
filter = FilterCustom

filterToDocument :: SerializedEntity rec => Filter rec -> Bson.Document
filterToDocument (Filter{..}) =
  case filterOperator of
    EQ  -> [ selectableFieldName filterField Bson.:= valueBson ]
    NE  -> mkFilter "$ne"
    GT  -> mkFilter "$gt"
    GE  -> mkFilter "$gte"
    LT  -> mkFilter "$lt"
    LE  -> mkFilter "$lte"
    IN  -> mkFilter "$in"
    NIN -> mkFilter "$nin"
  where mkFilter op = [ selectableFieldName filterField Bson.:= Bson.Doc [ op Bson.:= valueBson ] ]
        valueBson = case filterValue of
                    Left x -> toBSON x
                    Right xs -> Bson.Array $ map toBSON xs

filterToDocument (FilterAnd filters) = [ "$and" Bson.:= Bson.Array (map (Bson.Doc . filterToDocument) filters) ]
filterToDocument (FilterOr  filters) = [ "$or"  Bson.:= Bson.Array (map (Bson.Doc . filterToDocument) filters) ]
filterToDocument (FilterCustom doc) = doc

filtersToDocument :: SerializedEntity rec => [Filter rec] -> Bson.Document
filtersToDocument = foldl' (\d f -> d ++ (filterToDocument f)) []


(==.) :: (SerializedValue typ, Selectable rec field typ)
      => field -> typ -> Filter rec
field ==. value = Filter field EQ (Left value)

(!=.) :: (SerializedValue typ, Selectable rec field typ)
      => field -> typ -> Filter rec
field !=. value = Filter field NE (Left value)

(<.) :: (SerializedValue typ, Selectable rec field typ)
     => field -> typ -> Filter rec
field <. value = Filter field LT (Left value)

(<=.) :: (SerializedValue typ, Selectable rec field typ)
      => field -> typ -> Filter rec
field <=. value = Filter field LE (Left value)

(>.) :: (SerializedValue typ, Selectable rec field typ)
     => field -> typ -> Filter rec
field >. value = Filter field GT (Left value)

(>=.) :: (SerializedValue typ, Selectable rec field typ)
      => field -> typ -> Filter rec
field >=. value = Filter field GE (Left value)

(->.) :: (SerializedValue typ, Selectable rec field typ)
      => field -> [typ] -> Filter rec
field ->. values = Filter field IN (Right values)

(!->.) :: (SerializedValue typ, Selectable rec field typ)
       => field -> [typ] -> Filter rec
field !->. values = Filter field NIN (Right values)

infix 0 ||., &&.
(||.) :: Filter rec -> Filter rec  -> Filter rec
(FilterOr f1) ||. (FilterOr f2) = FilterOr (f1++f2)
(FilterOr f1) ||. f2 = FilterOr (f2:f1)
f1 ||. (FilterOr f2) = FilterOr (f1:f2)
f1 ||. f2 = FilterOr (f1:f2:[])

(&&.) :: Filter rec -> Filter rec  -> Filter rec
(FilterAnd f1) &&. (FilterAnd f2) = FilterAnd (f1++f2)
(FilterAnd f1) &&. f2 = FilterAnd (f2:f1)
f1 &&. (FilterAnd f2) = FilterAnd (f1:f2)
f1 &&. f2 = FilterAnd (f1:f2:[])


data Query rec = Query { filters :: [Filter rec]
                       , skip :: Word32
                       , limit :: Word32
                       , sort :: [OrderExp rec]
                       }

instance SerializedEntity rec => Show (Query rec) where
  show = show . queryToMongoDBQuery


select :: SerializedEntity rec => [Filter rec] -> Query rec
select filters = Query { filters = filters
                       , skip = 0
                       , limit = 0
                       , sort = []
                       }

queryToMongoDBQuery :: forall rec. SerializedEntity rec => Query rec -> MongoDB.Query
queryToMongoDBQuery (Query{..}) =
  (MongoDB.select (filtersToDocument filters) (collectionName (undefined :: rec)))
      { MongoDB.skip = skip
      , MongoDB.limit = limit
      , MongoDB.sort = map orderExpToField sort
      }


data UpdateOperator = SET | INC | MUL | MIN | MAX deriving (Eq, Show)

data Update rec = forall field typ . (SerializedValue typ, Selectable rec field typ) =>
                  Update { updateField :: field
                         , updateOperator :: UpdateOperator
                         , updateValue :: typ
                         }
                | UpdateCustom Bson.Document


updatesToDocument :: SerializedEntity rec => [Update rec] -> Bson.Document
updatesToDocument updates =
  let opGroups = map (\xs -> (updateOperator $ head xs, xs)) $ groupBy (\a b -> updateOperator a == updateOperator b) updates
   in foldl' (\x g -> x ++ uncurry groupToDocument g) [] opGroups
  where groupToDocument :: SerializedEntity rec => UpdateOperator -> [Update rec] -> Bson.Document
        groupToDocument SET = mkUpdate "$set"
        groupToDocument INC = mkUpdate "$inc"
        groupToDocument MUL = mkUpdate "$mul"
        groupToDocument MIN = mkUpdate "$min"
        groupToDocument MAX = mkUpdate "$max"

        mkUpdate :: SerializedEntity rec => Bson.Label -> [Update rec] -> Bson.Document
        mkUpdate op opUpdates = [ op Bson.:= Bson.Doc (concat (map (\(Update{..}) -> [ selectableFieldName updateField Bson.:= toBSON updateValue ]) opUpdates)) ]


update :: SerializedEntity rec => Bson.Document -> Update rec
update = UpdateCustom

(=~) :: (SerializedValue typ, Selectable rec field typ)
     => field -> typ -> Update rec
field =~ value = Update field SET value

(+=~) :: (SerializedValue typ, Selectable rec field typ)
      => field -> typ -> Update rec
field +=~ value = Update field INC value

(-=~) :: (SerializedValue typ, Num typ, Selectable rec field typ)
      => field -> typ -> Update rec
field -=~ value = Update field INC (negate value)

(*=~) :: (SerializedValue typ, Selectable rec field typ)
      => field -> typ -> Update rec
field *=~ value = Update field MUL value

(/=~) :: (SerializedValue typ, Fractional typ, Selectable rec field typ)
      => field -> typ -> Update rec
field /=~ value = Update field MUL (1.0 / value)


entityToBSONDoc :: forall rec. SerializedEntity rec => Entity rec -> Bson.Document
entityToBSONDoc (Entity recId record) = (fieldName (idField :: EntityField rec (Key rec)) =: recId) : toBSONDoc record

entityFromBSONDoc :: forall rec. SerializedEntity rec => Bson.Document -> Parser (Entity rec)
entityFromBSONDoc doc = Entity <$> doc .: fieldName (idField :: EntityField rec (Key rec)) <*> fromBSONDoc doc


newtype SerializedEntity rec => Cursor rec = Cursor { rawCursor :: MongoDB.Cursor }

nextBatch :: (MonadIO m, MonadBaseControl IO m, SerializedEntity rec)
          => Cursor rec -> MongoDB.Action m [Entity rec]
nextBatch (Cursor{..}) = MongoDB.nextBatch rawCursor >>= either fail return . runParser . mapM entityFromBSONDoc

next :: (MonadIO m, MonadBaseControl IO m, SerializedEntity rec)
     => Cursor rec -> MongoDB.Action m (Maybe (Entity rec))
next (Cursor{..}) = MongoDB.next rawCursor >>= mapM (either fail return . runParser . entityFromBSONDoc)

nextN :: (MonadIO m, MonadBaseControl IO m, SerializedEntity rec)
      => Int -> Cursor rec -> MongoDB.Action m [Entity rec]
nextN n (Cursor{..}) = MongoDB.nextN n rawCursor >>= either fail return . runParser . mapM entityFromBSONDoc

rest :: (MonadIO m, MonadBaseControl IO m, SerializedEntity rec) => Cursor rec -> MongoDB.Action m [Entity rec]
rest (Cursor{..}) = MongoDB.rest rawCursor >>= either fail return . runParser . mapM entityFromBSONDoc

closeCursor :: (MonadIO m, MonadBaseControl IO m)  => Cursor rec -> MongoDB.Action m ()
closeCursor (Cursor{..}) = MongoDB.closeCursor rawCursor

isCursorClosed :: (MonadIO m, MonadBaseControl IO m)  => Cursor rec -> MongoDB.Action m Bool
isCursorClosed (Cursor{..}) = MongoDB.isCursorClosed rawCursor


-- | 
find :: (MonadIO m, MonadBaseControl IO m, SerializedEntity rec) => Query rec -> MongoDB.Action m (Cursor rec)
find query = MongoDB.find (queryToMongoDBQuery query) >>= return . Cursor

-- | 
findOne :: (MonadIO m, SerializedEntity rec) => Query rec -> MongoDB.Action m (Maybe rec)
findOne query = MongoDB.findOne (queryToMongoDBQuery query) >>= mapM (either fail return . runParser . fromBSONDoc)

-- |
count :: (MonadIO m, MonadBaseControl IO m, SerializedEntity rec) => Query rec -> MongoDB.Action m Int
count query = MongoDB.count (queryToMongoDBQuery query)

-- | 
insertMany :: forall m rec. (MonadIO m, SerializedEntity rec) => [rec] -> MongoDB.Action m ()
insertMany records = MongoDB.insertMany_ collection documents
  where collection = collectionName (undefined :: rec)
        documents = map toBSONDoc records

-- | 
insert :: (MonadIO m, SerializedEntity rec) => rec -> MongoDB.Action m ()
insert record = insertMany [record]

-- |
save :: forall m rec. (MonadIO m, SerializedEntity rec) => Entity rec -> MongoDB.Action m ()
save = MongoDB.save (collectionName (undefined :: rec)) . entityToBSONDoc

-- |
modify :: forall m rec. (MonadIO m, SerializedEntity rec) => [Filter rec] -> [Update rec] -> MongoDB.Action m ()
modify filters updates = MongoDB.modify (MongoDB.select (filtersToDocument filters) (collectionName (undefined :: rec))) (updatesToDocument updates)

-- |
delete :: forall m rec. (MonadIO m, SerializedEntity rec) => [Filter rec] -> MongoDB.Action m ()
delete filters = MongoDB.delete $ MongoDB.Select (filtersToDocument filters) (collectionName (undefined :: rec)) 

-- |
deleteOne :: forall m rec. (MonadIO m, SerializedEntity rec) => [Filter rec] -> MongoDB.Action m ()
deleteOne filters = MongoDB.deleteOne $ MongoDB.Select (filtersToDocument filters) (collectionName (undefined :: rec)) 
