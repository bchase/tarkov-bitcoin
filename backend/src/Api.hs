{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api (app) where

import Control.Monad.Reader (runReaderT)
import Servant
       ( (:<|>)((:<|>))
       , Proxy(Proxy)
       , Raw
       , Server
       , serve
       , serveDirectoryFileServer
       )
import Servant.Server

import Api.Item (ItemAPI, itemApi, itemServer)
import Config (AppT(..), Config(..))



-- | This is the function we export to run our 'ItemAPI'. Given
-- a 'Config', we return a WAI 'Application' which any WAI compliant server
-- can run.
itemApp :: Config -> Application
itemApp cfg = serve itemApi (appToServer cfg)

-- | This functions tells Servant how to run the 'App' monad with our
-- 'server' function.
appToServer :: Config -> Server ItemAPI
appToServer cfg = hoistServer itemApi (convertApp cfg) itemServer

-- | This function converts our @'AppT' m@ monad into the @ExceptT ServantErr
-- m@ monad that Servant's 'enter' function needs in order to run the
-- application.
convertApp :: Config -> AppT IO a -> Handler a
convertApp cfg appt = Handler $ runReaderT (runApp appt) cfg

-- | Since we also want to provide a minimal front end, we need to give
-- Servant a way to serve a directory with HTML and JavaScript. This
-- function creates a WAI application that just serves the files out of the
-- given directory.
files :: Server Raw
files = serveDirectoryFileServer "assets"

-- | Just like a normal API type, we can use the ':<|>' combinator to unify
-- two different APIs and applications. This is a powerful tool for code
-- reuse and abstraction! We need to put the 'Raw' endpoint last, since it
-- always succeeds.
type AppAPI = ItemAPI :<|> Raw

appApi :: Proxy AppAPI
appApi = Proxy

-- | Finally, this function takes a configuration and runs our 'ItemAPI'
-- alongside the 'Raw' endpoint that serves all of our files.
app :: Config -> Application
app cfg =
    serve appApi (appToServer cfg :<|> files)
