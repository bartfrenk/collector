{-# OPTIONS_GHC -fno-warn-orphans #-} -- TODO: wrap SqlPersistT in newtype
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Persistent.Database (migrateAll, SourceId) where

import           BasicPrelude         hiding (groupBy, insert)
import           Control.Monad        (void)
import           Control.Monad.Base
import Data.Maybe
import           Database.Esqueleto   hiding ((+=.), (=.))
import           Database.Persist.Sql hiding ((==.))

import qualified Core.Concepts        as C
import qualified Core.Ports           as P
import           Persistent.Models

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

instance (MonadIO m, MonadBase IO m) => P.SourceRepository (SqlPersistT m) where

  type SourceId (SqlPersistT m) = Key Source

  createSource C.SourceAttributes {..} =
    case C.readSchemaKey schemaKey of
      Nothing -> let exc :: P.SourceRepoException (Key Source)
                     exc = P.InvalidSchema schemaKey
                 in throwIO exc
      Just (space, name) -> do
        schema' <- getBy $ UniqueNameInSpace space name
        case schema' of
          Nothing -> let exc :: P.SourceRepoException (Key Source)
                         exc = P.UnknownSchema space name
                     in throwIO exc
          Just schema -> insert $ Source label (entityKey schema)

  getSource sourceId = do
    source' <- get sourceId
    case source' of
      Nothing -> return Nothing
      Just source -> do
        schema <- fromJust <$> get (sourceSchema source)
        let key = C.writeSchemaKey (schemaSpace schema) (schemaName schema)
        return $ Just $ C.SourceAttributes (sourceLabel source) key

  putSchema space name def = void $ upsert (Schema space name def) []
  getSchema space name = do
    schema <- getBy $ UniqueNameInSpace space name
    return $ schemaDef . entityVal <$> schema
  listSchemas = undefined
  listSpaces = undefined
