{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Web.Server where

import           Data.String.Conv
import           Servant
import           Web.API

import           Control.Monad.Except
import           Data.Aeson.Encode.Pretty (encodePretty)

import           Core.Concepts
import           Core.Ports


handleGetSchema :: (SchemaRepository m, MonadError ServantErr m)
                => SchemaSpace -> SchemaName -> m SchemaDefinition
handleGetSchema space name = do
  schema <- getSchema space name
  case schema of
    Just def -> return def
    Nothing  -> throwError err404

handlePutSchema :: SchemaRepository m
                => SchemaSpace -> SchemaName -> SchemaDefinition -> m NoContent
handlePutSchema space name def = putSchema space name def >> return NoContent

handlePostSource :: (CombinedRepository m, MonadIO m) => SchemaDefinition -> m (MetricSourceId m)
handlePostSource def = do
  liftIO $ putStrLn $ toS $ encodePretty def
  undefined

schemaServer :: (SchemaRepository m, MonadError ServantErr m)
             => ServerT SchemaAPI m
schemaServer space name = handleGetSchema space name :<|> handlePutSchema space name

collectorServer :: (SchemaRepository m, MonadError ServantErr m)
                => ServerT CollectorAPI m
collectorServer = schemaServer

