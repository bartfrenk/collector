{-# LANGUAGE ConstraintKinds #-}

module Store.Class where

import Models.Persistent
import Models.Types

type GeneralStore m = (BlobStore m,
                       CountStore m)

class Monad m => BlobStore m where
  mergeBlobs :: [Blob] -> m ()
  deactivateBlobs :: [BlobId] -> m ()

class Monad m => CountStore m where
  countPosts :: [Counter] -> m ()
  fetchMostCounted :: CounterType -> m [(GroupId, ExtId, Count)]
