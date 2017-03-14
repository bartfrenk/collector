{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Repository.Models ( module Repository.Models
                         , module Repository.Types
                         ) where

import           BasicPrelude
import           Data.Time.Clock
import           Database.Persist.Sql (Entity (..))
import           Database.Persist.TH

import           Repository.Types

share [mkPersist sqlSettings { mpsGenerateLenses = True },
       mkMigrate "migrateAll"] [persistLowerCase|

Schema
  data SchemaSpec

Script
  label Text
  code ScriptCode

Source
  schema SchemaId

Blob
  modified UTCTime default=CURRENT_TIME
  source SourceId
  extId ExtId
  UniqueExtId source extId
  data BlobData
  tags TagList
  active Bool
  deriving Show

Counter
  type CounterType
  source SourceId
  blobExtId ExtId
  hour Hour
  DataPoint source blobExtId hour
  value Count
  deriving Show
|]

type BlobE = Entity Blob

type CounterE = Entity Counter
