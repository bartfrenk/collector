{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Web.Handlers where

import           Control.Monad.Except
import           Servant

import           Core.Concepts
import qualified Core.Ports           as P


postSource :: (P.SourceRepository m, MonadError ServantErr m)
           => SourceAttributes -> m (P.SourceId m)
postSource = P.createSource

getSource :: (P.SourceRepository m, MonadError ServantErr m)
          => P.SourceId m -> m SourceAttributes
getSource sourceId = do
  source' <- P.getSource sourceId
  case source' of
    Just source -> return source
    Nothing -> throwError err404

getSchema :: (P.SourceRepository m, MonadError ServantErr m)
          => SchemaSpace -> SchemaName -> m SchemaDefinition
getSchema space name = do
  schema' <- P.getSchema space name
  case schema' of
    Just schema -> return schema
    Nothing  -> throwError err404

putSchema :: P.SourceRepository m
          => SchemaSpace -> SchemaName -> SchemaDefinition -> m NoContent
putSchema space name def = P.putSchema space name def >> return NoContent

