{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Models where

import Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)
import Database.Persist.Sql (SqlPersistT, runMigration, runSqlPool)
import Database.Persist.TH
       (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Data.Time.Clock (UTCTime)

import Say
import Config (Config, configPool)
import Data.Text (Text)



share
    [ mkPersist sqlSettings
    , mkMigrate "migrateAll"
    ] [persistLowerCase|
User json
    name Text
    email Text
    deriving Show Eq

Item json
    uid Text
    bsgId Text
    name Text
    shortName Text
    price Int
    basePrice Int
    avg24hPrice Int
    avg7daysPrice Int
    traderName Text
    traderPrice Int
    traderPriceCur Text
    link Text
    wikiLink Text
    updated UTCTime
|]

doMigrations :: SqlPersistT IO ()
doMigrations = do
    liftIO $ say "in doMigrations, running?"
    runMigration migrateAll
    liftIO $ say "already run"

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks configPool
    liftIO $ runSqlPool query pool
