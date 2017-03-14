{-# LANGUAGE ConstraintKinds #-}

module Repository.Ports where

import Repository.Models

type GeneralStore m = (BlobStore m,
                       MetricStore m)

class Monad m => BlobStore m where
  mergeBlobs :: [Blob] -> m ()
  deactivateBlobs :: [BlobId] -> m ()

class Monad m => MetricStore m where
  countPosts :: [Counter] -> m ()
  fetchMostCounted :: CounterType -> m [(SourceId, ExtId, Count)]

class Monad m => SchemaStore m where
  putSchema :: SchemaSpec -> m ()
  getSchema :: SourceId -> m SchemaSpec
