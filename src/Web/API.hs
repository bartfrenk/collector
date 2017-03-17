{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Web.API where

import           Servant

import           Core.Concepts


collectorAPI :: Proxy CollectorAPI
collectorAPI = Proxy

type CollectorAPI = "schemas" :> SchemaAPI

  --"sources" :> SourceAPI sourceId

type WithPaging = QueryParam "offset" Int :> QueryParam "limit"

type SourceAPI sourceId =
  ReqBody '[JSON] SchemaDefinition :> Post '[JSON] sourceId :<|>
  Capture "sourceId" sourceId :>
    "schema" :> (
      Get '[JSON] SchemaDefinition :<|>
      Post :> ReqBody
      )

type SchemaAPI = Capture "space" SchemaSpace :> Capture "name" SchemaName :> (
  Get '[JSON] SchemaDefinition :<|>
  ReqBody '[JSON] SchemaDefinition :> Put '[JSON] NoContent)


    -- :<|>
    -- "items" :> ReqBody '[JSON] [] :> Post '[JSON] NoContent :<|>
    -- "items" :> Get '[JSON] [])

  -- "blobs" :> (
  -- ReqBody '[JSON] [Blob] :> Post '[JSON] Int :<|>
  -- Paging :> Get '[JSON] [Blob] :<|>
  -- "deactivate" :> ReqBody '[JSON] [BlobId] :> Post '[JSON] Int)

