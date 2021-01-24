module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- derived entirely from: https://escapefromtarkov.gamepedia.com/Hideout#Bitcoin_Farm
--
-- TODO
--   functionality
--     [X] rouble cost text inputs
--     [ ] payback time
--     [ ] per hour / per day toggle
--     [ ] fetch live prices + autopopulate
--   other
--     [ ] comma separate roubles, e.g. "12,345"
--     [ ] display explanations from wiki
--     [ ] README


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


fuelRoublesPerHour : Model -> Int
fuelRoublesPerHour ({ solar, fuel } as model) =
    let
        tpu =
            fuelUnitPowerTime solar

        totalFuelCost =
            fuelItemCost model fuel

        units =
            fuelUnits fuel

        totalFuelTime =
            tpu * toFloat units
    in
    ceiling <| toFloat totalFuelCost / totalFuelTime


roublesPerHour : Model -> Int -> Int
roublesPerHour { roublesBitcoin } count =
    round <| bitcoinPerHour count * toFloat roublesBitcoin


bitcoinPerHour : Int -> Float
bitcoinPerHour count =
    0.04137931 + (toFloat count - 1) / 49 * 0.10386397


timePerBitcoin : Int -> Float
timePerBitcoin count =
    1 / bitcoinPerHour count


paybackDaysForGpus : Model -> Int -> Float
paybackDaysForGpus { roublesGpu, roublesBitcoin } count =
    let
        cost =
            toFloat <| roublesGpu * count

        roublesPerDay =
            toFloat roublesBitcoin * bitcoinPerHour count * 24
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


fuelItemCost : Model -> Fuel -> Int
fuelItemCost { roublesMetalFuel, roublesExpeditionaryFuel } fuel =
    case fuel of
        Metal ->
            roublesMetalFuel

        Expeditionary ->
            roublesExpeditionaryFuel


type Item
    = Gpu
    | Bitcoin
    | MFuel
    | EFuel


items : List Item
items =
    let
        ensure totality =
            case totality of
                Gpu ->
                    ()

                Bitcoin ->
                    ()

                MFuel ->
                    ()

                EFuel ->
                    ()
    in
    [ Gpu
    , Bitcoin
    , MFuel
    , EFuel
    ]



--- init / subs ---


type alias Model =
    { solar : Bool
    , fuel : Fuel
    , roublesGpu : Int
    , roublesBitcoin : Int
    , roublesMetalFuel : Int
    , roublesExpeditionaryFuel : Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    do [] <| initModel flags


initModel : Flags -> Model
initModel flags =
    { solar = False
    , fuel = Metal
    , roublesGpu = 570000
    , roublesBitcoin = 542000
    , roublesMetalFuel = 170000
    , roublesExpeditionaryFuel = 120000
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
    | AdjustPrice Item Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            pure model

        SetSolar solar ->
            pure { model | solar = solar }

        SetFuel fuel ->
            pure { model | fuel = fuel }

        AdjustPrice item int ->
            pure <| adjustPrice model item int


adjustPrice : Model -> Item -> Int -> Model
adjustPrice model item int =
    let
        { roublesGpu, roublesBitcoin, roublesMetalFuel, roublesExpeditionaryFuel } =
            model
    in
    case item of
        Gpu ->
            { model | roublesGpu = roublesGpu + int }

        Bitcoin ->
            { model | roublesBitcoin = roublesBitcoin + int }

        MFuel ->
            { model | roublesMetalFuel = roublesMetalFuel + int }

        EFuel ->
            { model | roublesExpeditionaryFuel = roublesExpeditionaryFuel + int }



--- view ---


view : Model -> Html Msg
view ({ solar, fuel } as model) =
    div []
        [ div [] <| List.map (viewItemPriceInput model) items
        , hr [] []
        , div []
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
            , span [] [ text <| (String.fromInt <| fuelRoublesPerHour model) ++ "₽" ]
            ]
        , hr [] []
        , table [ style "border-collapse" "collapse" ]
            [ thead []
                [ tr [ cellBorder ]
                    [ th [ cellBorder ] [ text "GPUs" ]
                    , th [ cellBorder ] [ text "time per 0.2 BTC" ]
                    , th [ cellBorder ] [ text "roubles per hour (less fuel)" ]
                    ]
                ]
            , tbody [] <| List.map (row model) (List.range 1 50)
            ]
        ]


cellBorder : Attribute Msg
cellBorder =
    style "border" "1px solid black"


viewItemPriceInput : Model -> Item -> Html Msg
viewItemPriceInput model item =
    let
        ( name, roubles ) =
            case item of
                Gpu ->
                    ( "GPU", model.roublesGpu )

                Bitcoin ->
                    ( "Bitcoin (0.2 BTC)", model.roublesBitcoin )

                MFuel ->
                    ( "Metal Fuel Tank", model.roublesMetalFuel )

                EFuel ->
                    ( "Expeditionary Fuel Tank", model.roublesExpeditionaryFuel )
    in
    case ( item, model.fuel ) of
        ( EFuel, Metal ) ->
            text ""

        ( MFuel, Expeditionary ) ->
            text ""

        _ ->
            div []
                [ button [ onClick <| AdjustPrice item 10000 ] [ text "+10k" ]
                , button [ onClick <| AdjustPrice item 1000 ] [ text "+1k" ]
                , button [ onClick <| AdjustPrice item -1000 ] [ text "-1k" ]
                , button [ onClick <| AdjustPrice item -10000 ] [ text "-10k" ]
                , input [ disabled True, style "text-align" "right", value <| String.fromInt roubles ] []
                , text "₽ "
                , label [] [ text name ]
                ]


row : Model -> Int -> Html Msg
row ({ solar, fuel } as model) count =
    let
        rph =
            roublesPerHour model count

        fph =
            fuelRoublesPerHour model

        rphlf =
            rph - fph

        ( hour, min ) =
            count
                |> timePerBitcoin
                |> hourMin
                |> Tuple.mapBoth String.fromInt String.fromInt

        -- payback =
        --     paybackDaysForGpus model count
    in
    tr []
        [ td [ cellBorder ] [ text <| String.fromInt count ]
        , td [ cellBorder ] [ text <| hour ++ "h" ++ min ++ "m" ]
        , td [ cellBorder ] [ text <| String.fromInt rph ++ "₽  (" ++ String.fromInt rphlf ++ "₽" ++ ")" ]

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
