{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Web.API where

import qualified JSONSchema.Draft4 as D4
import           Servant

import           Repository.Models

type API = "sources" :> Capture "sourceId" SourceId :> PerGroupAPI

type Paging = QueryParam "offset" Int :> QueryParam "limit"

type PerGroupAPI = "schema" :> Get '[JSON] D4.Schema


  -- "blobs" :> (
  -- ReqBody '[JSON] [Blob] :> Post '[JSON] Int :<|>
  -- Paging :> Get '[JSON] [Blob] :<|>
  -- "deactivate" :> ReqBody '[JSON] [BlobId] :> Post '[JSON] Int)

