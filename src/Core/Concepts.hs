{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Core.Concepts where

import           BasicPrelude

import qualified Data.Aeson        as JSON
import qualified JSONSchema.Draft4 as D4



-- schema :: IO Text
-- schema = toS . encodePretty <$> (generate arbitrary :: IO D4.Schema)

type ItemKey = Text
type ItemEncoding = Text
type Tag = Text
type Hour = Int64
type Count = Int64
type SchemaName = Text
type SchemaSpace = Text

newtype SchemaDefinition = SchemaDefinition D4.Schema
  deriving (JSON.ToJSON, JSON.FromJSON, Show)

data CounterType = PostCount deriving (Eq, Show, Read)

data Counter src = Counter
  { counterType :: CounterType
  , counterSource :: src
  , counterItemKey :: ItemKey
  , counterHour :: Hour
  , counterValue :: Count
  }
