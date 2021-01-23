module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- derived entirely from: https://escapefromtarkov.gamepedia.com/Hideout#Bitcoin_Farm
--
-- TODO
--   functionality
--     [ ] payback time
--     [ ] per hour / per day toggle
--     [ ] rouble cost text inputs
--     [ ] fetch live prices + autopopulate
--   other
--     [ ] comma separate roubles, e.g. "12,345"
--     [ ] display explanations from wiki
--     [ ] README


bitcoinRoubles : Int
bitcoinRoubles =
    542000


gpuRoubles : Int
gpuRoubles =
    570000


expeditionaryFuelRoubles : Int
expeditionaryFuelRoubles =
    120000


metalFuelRoubles : Int
metalFuelRoubles =
    170000



--


fuelUnitPowerTime : Bool -> Float
fuelUnitPowerTime solar =
    -- Every 1 fuel resource powers the Hideout for 14 minutes 27 seconds
    -- 28 minutes 54 seconds with solar power module
    let
        m =
            if solar then
                2

            else
                1
    in
    (14 / 60 + 27 / 3600) * m


fuelRoublesPerHour : Bool -> Fuel -> Int
fuelRoublesPerHour solar fuel =
    let
        tpu =
            fuelUnitPowerTime solar

        totalFuelCost =
            fuelItemCost fuel

        units =
            fuelUnits fuel

        totalFuelTime =
            tpu * toFloat units
    in
    ceiling <| toFloat totalFuelCost / totalFuelTime


roublesPerHour : Int -> Int
roublesPerHour count =
    round <| bitcoinPerHour count * toFloat bitcoinRoubles


bitcoinPerHour : Int -> Float
bitcoinPerHour count =
    0.04137931 + (toFloat count - 1) / 49 * 0.10386397


timePerBitcoin : Int -> Float
timePerBitcoin count =
    1 / bitcoinPerHour count


gpuCost : Int -> Int
gpuCost count =
    gpuRoubles * count


paybackDaysForGpus : Int -> Float
paybackDaysForGpus count =
    let
        cost =
            toFloat <| gpuCost count

        roublesPerDay =
            toFloat bitcoinRoubles * bitcoinPerHour count * 24
    in
    cost / roublesPerDay


type Fuel
    = Metal
    | Expeditionary


fuelUnits : Fuel -> Int
fuelUnits fuel =
    case fuel of
        Metal ->
            100

        Expeditionary ->
            60


fuelItemCost : Fuel -> Int
fuelItemCost fuel =
    case fuel of
        Metal ->
            metalFuelRoubles

        Expeditionary ->
            expeditionaryFuelRoubles



--- init / subs ---


type alias Model =
    { solar : Bool
    , fuel : Fuel
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    do [] <| initModel flags


initModel : Flags -> Model
initModel flags =
    { solar = False
    , fuel = Metal
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []



--- update ---


type Msg
    = NoOp
    | SetFuel Fuel
    | SetSolar Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            pure model

        SetSolar solar ->
            pure { model | solar = solar }

        SetFuel fuel ->
            pure { model | fuel = fuel }



--- view ---


view : Model -> Html Msg
view ({ solar, fuel } as model) =
    div []
        [ div []
            [ input
                [ type_ "checkbox"
                , checked solar
                , onCheck SetSolar
                ]
                []
            , label [] [ text "Solar" ]
            ]
        , hr [] []
        , div []
            [ div
                []
                [ input
                    [ type_ "radio"
                    , checked <| fuel == Metal
                    , onClick <| SetFuel Metal
                    ]
                    []
                , label [] [ text "Metal Fuel Tank" ]
                , br [] []
                ]
            , div
                []
                [ input
                    [ type_ "radio"
                    , checked <| fuel == Expeditionary
                    , onClick <| SetFuel Expeditionary
                    ]
                    []
                , label [] [ text "Expeditionary Fuel Tank" ]
                , br [] []
                ]
            ]
        , hr [] []
        , span []
            [ strong [] [ text "Roubles/hour for fuel: " ]
            , span [] [ text <| (String.fromInt <| fuelRoublesPerHour solar fuel) ++ "₽" ]
            ]
        , hr [] []
        , table []
            [ thead []
                [ tr []
                    [ th [] [ text "GPUs" ]
                    , th [] [ text "time per 0.2btc" ]
                    , th [] [ text "roubles per hour (less fuel)" ]
                    ]
                ]
            , tbody [] <| List.map (row model) (List.range 1 50)
            ]
        ]


row : Model -> Int -> Html Msg
row { solar, fuel } count =
    let
        rph =
            roublesPerHour count

        fph =
            fuelRoublesPerHour solar fuel

        rphlf =
            rph - fph

        ( hour, min ) =
            count
                |> timePerBitcoin
                |> hourMin
                |> Tuple.mapBoth String.fromInt String.fromInt

        payback =
            paybackDaysForGpus count
    in
    tr []
        [ td [] [ text <| String.fromInt count ]
        , td [] [ text <| hour ++ "h" ++ min ++ "m" ]
        , td [] [ text <| String.fromInt rph ++ "₽  (" ++ String.fromInt rphlf ++ "₽" ++ ")" ]

        -- , td [] [ text <| String.fromFloat payback ]
        ]



--- main ---


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



--- helpers ---


hourMin : Float -> ( Int, Int )
hourMin t =
    let
        h =
            floor t

        rem =
            abs <| t - toFloat h

        m =
            round <| 60 * rem
    in
    ( h, m )



--- platform helpers ---


pure : Model -> ( Model, Cmd Msg )
pure =
    do []


do : List (Cmd Msg) -> Model -> ( Model, Cmd Msg )
do cs model =
    ( model, Cmd.batch cs )
