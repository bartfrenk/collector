{-# LANGUAGE DeriveGeneric              #-}
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

module Persistent.Models where

import           BasicPrelude
import           Data.Time.Clock
import           Database.Persist.Sql (Entity (..))
import           Database.Persist.TH
import           GHC.Generics

import qualified Core.Concepts as C
import           Persistent.Instances   ()

share [mkPersist sqlSettings,
       mkMigrate "migrateAll"] [persistLowerCase|

Schema
  space C.SchemaSpace
  name C.SchemaName
  UniqueNameInSpace space name
  def C.SchemaDefinition
  deriving Show

Script
  label Text
  code Text
  deriving Show

Source
  label Text
  schema SchemaId
  deriving Show

Item json
  source SourceId
  key C.ItemKey
  UniqueKeyInSource source key
  tags [C.Tag]
  active Bool
  contents C.ItemEncoding
  modified UTCTime default=CURRENT_TIMESTAMP
  deriving Show
  deriving Generic

Counter
  type C.CounterType
  source SourceId
  itemKey C.ItemKey
  hour C.Count
  DataPoint source itemKey hour
  value C.Count
  deriving Show
|]

type ItemE = Entity Item

type CounterE = Entity Counter
