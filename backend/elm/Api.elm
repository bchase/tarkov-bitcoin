module Api exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set
import Http
import String
import Url.Builder

type alias Item  =
   { itemUid: String
   , itemBsgId: String
   , itemName: String
   , itemShortName: String
   , itemPrice: Int
   , itemBasePrice: Int
   , itemAvg24hPrice: Int
   , itemAvg7daysPrice: Int
   , itemTraderName: String
   , itemTraderPrice: Int
   , itemTraderPriceCur: String
   , itemLink: String
   , itemWikiLink: String
   , itemUpdated: Posix
   }

jsonDecItem : Json.Decode.Decoder ( Item )
jsonDecItem =
   Json.Decode.succeed (\pitemUid pitemBsgId pitemName pitemShortName pitemPrice pitemBasePrice pitemAvg24hPrice pitemAvg7daysPrice pitemTraderName pitemTraderPrice pitemTraderPriceCur pitemLink pitemWikiLink pitemUpdated -> {itemUid = pitemUid, itemBsgId = pitemBsgId, itemName = pitemName, itemShortName = pitemShortName, itemPrice = pitemPrice, itemBasePrice = pitemBasePrice, itemAvg24hPrice = pitemAvg24hPrice, itemAvg7daysPrice = pitemAvg7daysPrice, itemTraderName = pitemTraderName, itemTraderPrice = pitemTraderPrice, itemTraderPriceCur = pitemTraderPriceCur, itemLink = pitemLink, itemWikiLink = pitemWikiLink, itemUpdated = pitemUpdated})
   |> required "itemUid" (Json.Decode.string)
   |> required "itemBsgId" (Json.Decode.string)
   |> required "itemName" (Json.Decode.string)
   |> required "itemShortName" (Json.Decode.string)
   |> required "itemPrice" (Json.Decode.int)
   |> required "itemBasePrice" (Json.Decode.int)
   |> required "itemAvg24hPrice" (Json.Decode.int)
   |> required "itemAvg7daysPrice" (Json.Decode.int)
   |> required "itemTraderName" (Json.Decode.string)
   |> required "itemTraderPrice" (Json.Decode.int)
   |> required "itemTraderPriceCur" (Json.Decode.string)
   |> required "itemLink" (Json.Decode.string)
   |> required "itemWikiLink" (Json.Decode.string)
   |> required "itemUpdated" (jsonDecPosix)

jsonEncItem : Item -> Value
jsonEncItem  val =
   Json.Encode.object
   [ ("itemUid", Json.Encode.string val.itemUid)
   , ("itemBsgId", Json.Encode.string val.itemBsgId)
   , ("itemName", Json.Encode.string val.itemName)
   , ("itemShortName", Json.Encode.string val.itemShortName)
   , ("itemPrice", Json.Encode.int val.itemPrice)
   , ("itemBasePrice", Json.Encode.int val.itemBasePrice)
   , ("itemAvg24hPrice", Json.Encode.int val.itemAvg24hPrice)
   , ("itemAvg7daysPrice", Json.Encode.int val.itemAvg7daysPrice)
   , ("itemTraderName", Json.Encode.string val.itemTraderName)
   , ("itemTraderPrice", Json.Encode.int val.itemTraderPrice)
   , ("itemTraderPriceCur", Json.Encode.string val.itemTraderPriceCur)
   , ("itemLink", Json.Encode.string val.itemLink)
   , ("itemWikiLink", Json.Encode.string val.itemWikiLink)
   , ("itemUpdated", jsonEncPosix val.itemUpdated)
   ]


getItems : (Result Http.Error  ((List (Entity Item)))  -> msg) -> Cmd msg
getItems toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ "items"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.list ((jsonDecEntity jsonDecItem)))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }
