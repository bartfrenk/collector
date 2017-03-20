{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Core.Concepts where

import           BasicPrelude

import qualified Data.Aeson        as JSON
import           Data.String.Conv
import           GHC.Generics
import qualified JSONSchema.Draft4 as D4
import           Text.Parsec       hiding (space)

type ItemKey = Text
type ItemEncoding = Text
type Tag = Text
type Hour = Int64
type Count = Int64
type SchemaName = Text
type SchemaSpace = Text

readSchemaKey :: Text -> Maybe (SchemaSpace, SchemaName)
readSchemaKey s = case parse (tuple <* eof) "" s of
                    Left _ -> Nothing
                    Right x -> Just x
  where tuple = liftM2 (,) segment (char '/' >> segment)
        segment = toS `fmap` many (noneOf ['/'])


writeSchemaKey :: SchemaSpace -> SchemaName -> Text
writeSchemaKey space name = toS $ space <> "/" <> name

newtype SchemaDefinition = SchemaDefinition D4.Schema
  deriving (JSON.ToJSON, JSON.FromJSON, Show)

data CounterType = PostCount deriving (Eq, Show, Read, Generic)

instance JSON.ToJSON CounterType
instance JSON.FromJSON CounterType

data Counter sourceId = Counter
  { counterType    :: CounterType
  , counterSource  :: sourceId
  , counterItemKey :: ItemKey
  , counterHour    :: Hour
  , counterValue   :: Count
  } deriving (Eq, Show, Read, Generic)

instance JSON.ToJSON sourceId => JSON.ToJSON (Counter sourceId)
instance JSON.FromJSON sourceId => JSON.FromJSON (Counter sourceId)

data SourceAttributes = SourceAttributes
  { schemaKey :: Text
  , label     :: Text
  } deriving (Eq, Show, Read, Generic)

instance JSON.ToJSON SourceAttributes
instance JSON.FromJSON SourceAttributes
