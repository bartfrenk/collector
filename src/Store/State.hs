{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Store.State where

import Database.Persist
import Control.Lens
import Control.Monad.State
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set

import Models.Persistent
import Store.Class

data GeneralState = GeneralState
  { _blobs :: Map BlobId Blob
  , _counters :: [CounterE]} deriving (Show)

makeLenses ''GeneralState

-- instance BlobStore (State GeneralState) where
--   mergeBlobs bs =
--     let by = 
--         upd b = 
--   deactivateBlobs ids =
--     let upd = Map.adjust (set blobActive False)
--     in modify $ over blobs (\m -> foldr upd m ids)


instance CountStore (State GeneralState) where
  countPosts = undefined
  fetchMostCounted = undefined

