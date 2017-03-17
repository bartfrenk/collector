{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Persistent.Instances where

import           BasicPrelude

import qualified Data.Aeson             as JSON
import           Data.Bifunctor
import           Data.String.Conv
import           Database.Persist.Class (PersistField (..))
import           Database.Persist.Sql
import           Database.Persist.TH
import           Database.Persist.Types (PersistValue (..))

import           Core.Concepts

instance PersistField SchemaDefinition where
  toPersistValue (SchemaDefinition schema) =
    PersistText $ toS $ JSON.encode schema
  fromPersistValue (PersistText s) =
    SchemaDefinition <$> bimap toS id (JSON.eitherDecodeStrict (toS s))
  fromPersistValue x = Left $ "Expected schema, received " <> tshow x

instance PersistFieldSql SchemaDefinition where
  sqlType _ = SqlString

derivePersistField "CounterType"
