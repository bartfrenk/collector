{-# LANGUAGE NoImplicitPrelude #-}

module Handlers where

import BasicPrelude

import Store.Class
import Models.Persistent


postBlobs :: (MonadIO m, GeneralStore m) => [Blob] -> m Int
postBlobs = undefined

listBlobs :: (MonadIO m, GeneralStore m) => Int -> Int -> m [Blob]
listBlobs = undefined

deactivateBlobs :: (MonadIO m, BlobStore m) => [BlobId] -> m Int
deactivateBlobs = undefined

