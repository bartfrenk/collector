{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Web.API where

import           Servant

import           Core.Concepts


collectorAPI :: Proxy (CollectorAPI sourceId)
collectorAPI = Proxy

type CollectorAPI sourceId
  = "sources" :> SourceAPI sourceId :<|>
    "schemas" :> SchemaAPI

type SourceAPI sourceId =
  ReqBody '[JSON] SourceAttributes :> Post '[JSON] sourceId :<|>
  Capture "sourceId" sourceId :> Get '[JSON] SourceAttributes

type SchemaAPI = Capture "space" SchemaSpace :> Capture "name" SchemaName :> (
  Get '[JSON] SchemaDefinition :<|>
  ReqBody '[JSON] SchemaDefinition :> Put '[JSON] NoContent)

type WithPaging = QueryParam "offset" Int :> QueryParam "limit"

    -- :<|>
    -- "items" :> ReqBody '[JSON] [] :> Post '[JSON] NoContent :<|>
    -- "items" :> Get '[JSON] [])

  -- "blobs" :> (
  -- ReqBody '[JSON] [Blob] :> Post '[JSON] Int :<|>
  -- Paging :> Get '[JSON] [Blob] :<|>
  -- "deactivate" :> ReqBody '[JSON] [BlobId] :> Post '[JSON] Int)

