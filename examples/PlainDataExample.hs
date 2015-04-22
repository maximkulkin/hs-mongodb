{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module PlainDataExample where

import Data.Text (Text)
import Database.MongoDB.Structured.TH

data NormalConstructors =
    Foo1 Int String
  | Foo2 Int Double
  deriving (Show, Eq)
$(deriveSerializedEntityWith
    (defaultDeriveSerializedEntityOptions
      { mkFieldName = \e f -> case f of
          "foo1_0" -> "foo1Int"
          "foo1_1" -> "foo1String"
          "foo2_0" -> "foo2Int"
          "foo2_1" -> "foo2Double"
          _ -> fail $ "Unknown field: " ++ f
      , mkEntityFieldName = \e f -> case f of
          "Id" -> e ++ "Id"
          "Type" -> e ++ "Type"
          "foo1_0" -> "Foo1Int"
          "foo1_1" -> "Foo1String"
          "foo2_0" -> "Foo2Int"
          "foo2_1" -> "Foo2Double"
          _ -> fail $ "Unknown field: " ++ f
      })
  ''NormalConstructors)

