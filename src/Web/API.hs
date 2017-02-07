{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Web.API where

import Servant

import Models.Persistent

type API = "groups" :> Capture "groupId" GroupId :> PerGroupAPI

type Paging = QueryParam "offset" Int :> QueryParam "limit"

type PerGroupAPI = "blobs" :> (
  ReqBody '[JSON] [Blob] :> Post '[JSON] Int :<|>
  Paging :> Get '[JSON] [Blob] :<|>
  "deactivate" :> ReqBody '[JSON] [BlobId] :> Post '[JSON] Int)

