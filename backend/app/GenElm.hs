{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import Servant.Elm (DefineElm (DefineElm), Proxy (Proxy), defElmImports, defElmOptions, generateElmModuleWith)

import Api.Item (ItemAPI)
import Models (Item)



main :: IO ()
main =
  generateElmModuleWith
    defElmOptions
    [ "Api" ]
    defElmImports
    "elm"
    [ DefineElm (Proxy :: Proxy Item)
    ]
    (Proxy :: Proxy ItemAPI)
