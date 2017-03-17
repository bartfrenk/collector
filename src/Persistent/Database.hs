{-# OPTIONS_GHC -fno-warn-orphans #-} -- TODO: wrap SqlPersistT in newtype
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Persistent.Database (module Persistent.Database, migrateAll) where

import           BasicPrelude         hiding (groupBy)
import           Control.Monad        (void)
import           Database.Esqueleto   hiding ((+=.), (=.))
import           Database.Persist.Sql hiding ((==.))

import qualified Core.Concepts        as C
import qualified Core.Ports           as P
import           Persistent.Models

-- instance MonadIO m =>  (SqlPersistT m) where
--   mergeBlobs = mapM_ (`upsert` [])
--   deactivateBlobs ids =
--     updateWhere [BlobId /<-. ids] [ BlobActive =. False ]


instance MonadIO m => P.MetricRepository (SqlPersistT m) where
  type MetricSourceId (SqlPersistT m) = Key Source
  registerCounts = mapM_ upsertCounter
    where upsertCounter :: MonadIO m => C.Counter SourceId -> SqlPersistT m ()
          upsertCounter c@C.Counter{..} = do
            _ <- upsert (iso c) [CounterValue +=. counterValue]
            return ()
          iso :: C.Counter SourceId -> Counter
          iso C.Counter {..} = Counter
            { counterType = counterType
            , counterSource = counterSource
            , counterItemKey = counterItemKey
            , counterHour = counterHour
            , counterValue = counterValue
            }
  listMostCounted cty _ = do
    rows <- query
    return (unwrap `fmap` rows)
    where query =
            select $ from $ \c -> do
            where_ (c ^. CounterType ==. val cty)
            groupBy (c ^. CounterSource, c ^. CounterItemKey)
            let sum' = sum_ (c ^. CounterValue)
            orderBy [desc sum']
            return (c ^. CounterSource, c ^. CounterItemKey, sum')
          unwrap (Value sourceId_, Value itemKey, Value (Just cnt)) =
            C.Counter cty sourceId_ itemKey cnt 0
          unwrap _ = undefined

instance MonadIO m => P.SchemaRepository (SqlPersistT m) where
  putSchema space name def = void $ upsert (Schema space name def) []
  getSchema space name = do
    def <- getBy $ UniqueNameInSpace space name
    return $ schemaDef . entityVal <$> def
  listSchemas = undefined
  listSpaces = undefined
