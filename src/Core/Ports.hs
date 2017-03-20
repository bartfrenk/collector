{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

module Core.Ports where

import           BasicPrelude


import           Data.Map

import           Core.Concepts

type CombinedRepository m = (MetricRepository m,
                             SourceRepository m)

class Monad m => MetricRepository m where
  type MetricSourceId m :: *
  registerCounts :: [Counter (MetricSourceId m)] -> m ()
  listMostCounted :: CounterType -> MetricSourceId m -> m [Counter (MetricSourceId m)]

-- class Monad m => SchemaRepository m where
--   putSchema :: SchemaSpace -> SchemaName -> SchemaDefinition -> m ()
--   getSchema :: SchemaSpace -> SchemaName -> m (Maybe SchemaDefinition)
--   listSchemas :: SchemaSpace -> m (Map SchemaName SchemaDefinition)
--   listSpaces :: m [SchemaSpace]

class Monad m => SourceRepository m where
  type SourceId m :: *

  createSource :: SourceAttributes -> m (SourceId m)
  getSource :: SourceId m -> m (Maybe SourceAttributes)

  putSchema :: SchemaSpace -> SchemaName -> SchemaDefinition -> m ()
  getSchema :: SchemaSpace -> SchemaName -> m (Maybe SchemaDefinition)
  listSchemas :: SchemaSpace -> m (Map SchemaName SchemaDefinition)
  listSpaces :: m [SchemaSpace]

data SourceRepoException sourceId
  = UnknownSchema SchemaSpace SchemaName
  | UnknownSource sourceId
  | InvalidSchema Text
  deriving (Typeable, Show)

instance (Typeable sourceId, Show sourceId)
  => Exception (SourceRepoException sourceId)

