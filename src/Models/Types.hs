{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.Types where

import BasicPrelude
import Database.Persist.TH
import GHC.Generics


type SchemaSpec = Text

type BlobData = Text

type Tag = Text

type TagList = [Tag]

type ExtId = Text

type ScriptCode = Text

-- TODO: decide width of Int
type Hour = Int

-- TODO: decide width of Int
type Count = Int

data CounterType = PostCount deriving (Eq, Show, Read, Generic)

derivePersistField "CounterType"
