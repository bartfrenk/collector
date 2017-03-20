{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Web.Server where

import           Control.Monad.Except
import           Servant

import           Core.Ports
import           Web.API
import qualified Web.Handlers as H


schemaServer :: (SourceRepository m, MonadError ServantErr m)
             => ServerT SchemaAPI m
schemaServer space name =
  H.getSchema space name :<|>
  H.putSchema space name

sourceServer :: (SourceRepository m, MonadError ServantErr m)
             => ServerT (SourceAPI (SourceId m)) m
sourceServer =
  H.postSource :<|>
  H.getSource

collectorServer :: (SourceRepository m, MonadError ServantErr m)
                => ServerT (CollectorAPI (SourceId m)) m
collectorServer =
  sourceServer :<|>
  schemaServer

