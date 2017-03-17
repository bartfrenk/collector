{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where


import           BasicPrelude


import           Control.Monad.Except
import           Control.Monad.Logger        hiding (runLoggingT)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Database.Persist.Postgresql hiding (migrate)
import           Network.Wai                 (Middleware)
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Middleware.Cors
import           Servant

import           Persistent.Database         (migrateAll)
import           Web.API
import           Web.Server                  (collectorServer)


type BackendT m a = SqlPersistT (LoggingT m) a

customCors :: Middleware
customCors = cors (const $ Just customCorsResourcePolicy)
  where customCorsResourcePolicy =
          simpleCorsResourcePolicy
          { corsRequestHeaders = simpleHeaders }

runBackend :: (MonadIO m, MonadBaseControl IO m)
           => ConnectionString -> LogLevel -> BackendT m a -> m a
runBackend str _ = runLoggingT . runDatabase
  where runDatabase = withPostgresqlPool str 10 . runSqlPool
        runLoggingT = runStderrLoggingT

postgres :: ConnectionString
postgres = "host=localhost \
           \port=15432 \
           \user=collector \
           \password=changeit \
           \dbname=collector"

server :: ConnectionString -> LogLevel -> Server CollectorAPI
server str level = enter (Nat translate) collectorServer
  where translate :: BackendT (ExceptT ServantErr IO) a -> Handler a
        translate = runBackend str level

port :: Int
port = 5000


main :: IO ()
main = do
  migrate postgres LevelDebug
  putStrLn $ "Starting collector on port " <> tshow port <> "..."
  run port $ mw $ serve collectorAPI (server postgres LevelDebug)
  where mw = customCors

migrate :: ConnectionString -> LogLevel -> IO ()
migrate str level = runBackend str level (runMigration migrateAll)

