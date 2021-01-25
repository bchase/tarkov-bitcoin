{-# LANGUAGE OverloadedStrings #-}

module Task.UpdatePrices
  ( run
  ) where

import Control.Monad (join, void)
import Control.Exception (throwIO)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Data.Aeson
import Data.Maybe
import Data.ByteString.Lazy (ByteString)
import Data.String.Conv
import Database.Persist.Postgresql ((==.), insert, deleteWhere)
import Lens.Micro
import Network.Wreq
import Safe (headMay)
import System.Environment (getEnv)

import Config (App, AppT(..), Config(..))
import Init (withConfig)
import Models


run :: IO ()
run = do
  -- apiKey <- getEnv "TARKOV_MARKET_API_KEY" # TODO
  let apiKey = "asdf"

  items <- fetchItems apiKey
  mapM_ createOrUpdateItem items


createOrUpdateItem :: Item -> IO ()
createOrUpdateItem item =
  withConfig $ \cfg -> do
    runAppToIO cfg $ do
      void $ runDb $ do
        deleteWhere [ ItemUid ==. itemUid item ]
        insert item


itemUids :: [String]
itemUids =
  [ btc
  , gpu
  , mfuel
  , efuel
  ]


fetchItems :: String -> IO [Item]
fetchItems apiKey = catMaybes <$> (flip mapM) itemUids (fetchItem apiKey)

fetchItem :: String -> String -> IO (Maybe Item)
fetchItem apiKey uid = do
  let opts = defaults & header "x-api-key" .~ [toS apiKey]
  r <- getWith opts (priceItem uid)
  let body = r ^. responseBody
  pure $ parse body


parse :: ByteString -> Maybe Item
parse = join . fmap headMay . decode


priceItem :: String -> String
priceItem _ = "http://localhost:8081/items"
-- priceItem uid = "https://tarkov-market.com/api/v1/item?uid=" ++ uid


btc :: String
btc = "5ac84494-465a-424a-b36e-fe22869ba5ec"

-- TODO
gpu :: String
gpu   = ""
mfuel :: String
mfuel = ""
efuel :: String
efuel = ""


--- TODO dup
runAppToIO :: Config -> App a -> IO a
runAppToIO config app = do
  result <- runExceptT $ runReaderT (runApp app) config
  either throwIO pure result
