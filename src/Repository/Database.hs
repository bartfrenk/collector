{-# LANGUAGE IncoherentInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Store.Database where

import           BasicPrelude         hiding (groupBy)
import           Database.Esqueleto   hiding ((+=.), (=.))
import           Database.Persist.Sql hiding ((==.))

import           Repository.Models    as M
import           Repository.Ports

instance MonadIO m => BlobStore (SqlPersistT m) where
  mergeBlobs = mapM_ (`upsert` [])
  deactivateBlobs ids =
    updateWhere [BlobId /<-. ids] [ BlobActive =. False ]

instance MonadIO m => CountStore (SqlPersistT m) where
  countPosts = mapM_ upsertCounter
    where upsertCounter :: MonadIO m => Counter -> SqlPersistT m ()
          upsertCounter c@Counter{..} = do
            _ <- upsert c [CounterValue +=. _counterValue]
            return ()
  fetchMostCounted cty = do
    rows <- query
    return (unwrap `fmap` rows)
    where query =
            select $ from $ \c -> do
            where_ (c ^. CounterType ==. val cty)
            groupBy (c ^. CounterGroup, c ^. CounterBlobExtId)
            let sum' = sum_ (c ^. CounterValue)
            orderBy [desc sum']
            return (c ^. CounterGroup, c ^. CounterBlobExtId, sum')
          unwrap (Value groupId, Value extId, Value (Just cnt)) =
            (groupId, extId, cnt)
          unwrap _ = undefined
