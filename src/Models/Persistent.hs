{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models.Persistent where

import BasicPrelude
import Database.Persist.TH
import Database.Persist.Sql (Entity(..))
import Data.Time.Clock

import Models.Types

share [mkPersist sqlSettings { mpsGenerateLenses = True },
       mkMigrate "migrateAll"] [persistLowerCase|

Schema
  label Text
  spec SchemaSpec

Script
  label Text
  code ScriptCode

Group
  schema SchemaId

Blob
  updated UTCTime default=CURRENT_TIME
  group GroupId
  extId ExtId
  UniqueExtId group extId
  data BlobData
  tags TagList
  active Bool
  deriving Show

Counter
  type CounterType
  group GroupId
  blobExtId ExtId
  hour Hour
  DataPoint group blobExtId hour
  value Count
  deriving Show

|]

type BlobE = Entity Blob

type CounterE = Entity Counter
