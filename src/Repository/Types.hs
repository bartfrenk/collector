{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Repository.Types where

import           BasicPrelude

import qualified Data.Aeson             as JSON
import           Data.Bifunctor
import           Data.String.Conv
import           Database.Persist.Class (PersistField (..))
import           Database.Persist.Sql
import           Database.Persist.TH
import           Database.Persist.Types (PersistValue (..))
import qualified JSONSchema.Draft4      as D4

newtype SchemaSpec = SchemaSpec D4.Schema

type BlobData = Text

type Tag = Text

type TagList = [Tag]

type ExtId = Text

type ScriptCode = Text

-- TODO: decide width of Int
type Hour = Int

-- TODO: decide width of Int
type Count = Int

data CounterType = PostCount deriving (Eq, Show, Read)

instance PersistField SchemaSpec where
  toPersistValue (SchemaSpec schema) = PersistByteString $ toS $ JSON.encode schema
  fromPersistValue (PersistByteString s) =
    SchemaSpec <$> bimap toS id (JSON.eitherDecodeStrict s)
  fromPersistValue x = Left $ "Expected schema, received " <> tshow x

instance PersistFieldSql SchemaSpec where
  sqlType _ = SqlBlob

derivePersistField "CounterType"
