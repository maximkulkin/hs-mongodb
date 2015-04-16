{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module SumTypeExample where

import Data.Text (Text)
import Database.MongoDB.Structured.TH

data SumTypeWithIdenticalyNamedFields =
    Foo1
    { fooInt :: Int
    , fooText :: String
    }
  | Foo2
    { fooInt :: Int
    , fooDouble :: Double
    } deriving (Show, Eq)
$(deriveSerializedEntity ''SumTypeWithIdenticalyNamedFields)

