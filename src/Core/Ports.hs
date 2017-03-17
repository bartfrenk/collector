{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies    #-}

module Core.Ports where

import           Data.Map

import           Core.Concepts

type CombinedRepository m = (MetricRepository m,
                             SchemaRepository m)

class Monad m => MetricRepository m where
  type MetricSourceId m :: *
  registerCounts :: [Counter (MetricSourceId m)] -> m ()
  listMostCounted :: CounterType -> MetricSourceId m -> m [Counter (MetricSourceId m)]

class Monad m => SchemaRepository m where
  putSchema :: SchemaSpace -> SchemaName -> SchemaDefinition -> m ()
  getSchema :: SchemaSpace -> SchemaName -> m (Maybe SchemaDefinition)
  listSchemas :: SchemaSpace -> m (Map SchemaName SchemaDefinition)
  listSpaces :: m [SchemaSpace]
