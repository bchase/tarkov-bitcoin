{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Item where

import Control.Monad.Except (MonadIO)
import Control.Monad.Logger (logDebugNS)
import Database.Persist.Postgresql
import Servant

import Config (AppT(..))
import Control.Monad.Metrics (increment)
import Models (Item, runDb)



type ItemAPI =
       "items" :> Get '[JSON] [Entity Item]


itemApi :: Proxy ItemAPI
itemApi = Proxy

itemServer :: MonadIO m => ServerT ItemAPI (AppT m)
itemServer = allItems

allItems :: MonadIO m => AppT m [Entity Item]
allItems = do
    increment "allItems"
    logDebugNS "web" "allItems"
    runDb (selectList [] [])
