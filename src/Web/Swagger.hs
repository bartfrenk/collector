{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Web.Swagger where

import           Control.Lens
import qualified Data.Aeson                 as JSON
import           Data.Aeson.Types           (camelTo2)
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Swagger
import           Servant
import           Servant.Swagger

import           Core.Concepts
import           Web.API

-- type DocumentedAPI = CollectorAPI :<|> SwaggerAPI

-- documentedAPI :: Proxy DocumentedAPI
-- documentedAPI = Proxy

-- type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

-- instance ToParamSchema SourceId where
--   toParamSchema _ = mempty
--     & type_ .~ SwaggerInteger

-- instance ToSchema SourceId where
--   declareNamedSchema _ = return $ NamedSchema Nothing mempty

-- modifier :: String -> String
-- modifier = drop 1 . dropWhile (/= '_') . camelTo2 '_'

-- prefixSchemaOptions :: SchemaOptions
-- prefixSchemaOptions = defaultSchemaOptions { fieldLabelModifier = modifier }

-- instance ToSchema Blob where
--   declareNamedSchema = genericDeclareNamedSchema prefixSchemaOptions

-- instance ToSchema SchemaSpec where
--   declareNamedSchema _ = return $ NamedSchema (Just "JSON schema v4") mempty

-- swagger :: Swagger
-- swagger = toSwagger collectorAPI
--   & info.title .~ "Collector API"
--   & info.version .~ "0.1"
--   & info.description ?~ "Public API for the collector service"

-- writeSwagger :: IO ()
-- writeSwagger = BL8.writeFile "swagger.json" (JSON.encode $ toSwagger collectorAPI)
