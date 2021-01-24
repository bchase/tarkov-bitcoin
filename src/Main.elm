module Main exposing (main)

import Browser
import FormatNumber
import FormatNumber.Locales exposing (Decimals(..), usLocale)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- derived entirely from: https://escapefromtarkov.gamepedia.com/Hideout#Bitcoin_Farm


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


paybackDaysForGpus : Model -> Maybe Int -> Int -> Float
paybackDaysForGpus { roublesGpu, roublesBitcoin } mfph count =
    let
        gpuCost =
            toFloat <| roublesGpu * count

        daysForFuel =
            case mfph of
                Nothing ->
                    0

                Just fph ->
                    (toFloat fph * 24 * daysForGpus) / roublesPerDay

        roublesPerDay =
            toFloat roublesBitcoin * bitcoinPerHour count * 24

        daysForGpus =
            gpuCost / roublesPerDay
    in
    daysForGpus + daysForFuel


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
    , displayPer : TimeUnit
    }


type TimeUnit
    = Hour
    | Day


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
    , displayPer = Hour
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
    | DisplayPer TimeUnit


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

        DisplayPer unit ->
            pure { model | displayPer = unit }


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
view ({ solar, fuel, displayPer } as model) =
    let
        per =
            case displayPer of
                Hour ->
                    "hour"

                Day ->
                    "day"
    in
    div [ style "margin" "30px" ]
        [ h1 [] [ text "Tarkov Bitcoin Farm Calculator" ]
        , span []
            [ text "Entirely based on and thanks to: "
            , a
                [ href "https://escapefromtarkov.gamepedia.com/Hideout#Bitcoin_Farm"
                ]
                [ text "Hideout - The Official Escape from Tarkov Wiki" ]
            ]
        , br [] []
        , br [] []
        , hr [] []
        , h2 [] [ text "Costs" ]
        , div [] <| List.map (viewItemPriceInput model) items
        , br [] []
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
        , br [] []
        , div []
            [ input
                [ type_ "checkbox"
                , checked solar
                , onCheck SetSolar
                ]
                []
            , label [] [ text "Solar" ]
            ]
        , br [] []
        , span []
            [ strong [] [ text "Roubles/hour for fuel: " ]
            , span [] [ text <| (format <| fuelRoublesPerHour model) ++ "₽" ]
            ]
        , br [] []
        , br [] []
        , hr [] []
        , br [] []
        , div []
            [ strong [] [ text "Display roubles per " ]
            , span
                []
                [ input
                    [ type_ "radio"
                    , checked <| displayPer == Hour
                    , onClick <| DisplayPer Hour
                    ]
                    []
                , label [] [ text "hour" ]
                ]
            , span
                []
                [ input
                    [ type_ "radio"
                    , checked <| displayPer == Day
                    , onClick <| DisplayPer Day
                    ]
                    []
                , label [] [ text "day" ]
                ]
            ]
        , br [] []
        , table [ style "border-collapse" "collapse" ]
            [ thead []
                [ tr [ cellBorder ]
                    [ th [ cellBorder ] [ text "GPUs" ]
                    , th [ cellBorder ] [ text "time per 0.2 BTC" ]
                    , th [ cellBorder ] [ text <| "roubles per " ++ per ++ " (less fuel)" ]
                    , th [ cellBorder ] [ text "pay-back-time in days (plus fuel)" ]
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
                , input [ disabled True, style "text-align" "right", value <| format roubles ] []
                , text "₽ "
                , label [] [ text name ]
                ]


row : Model -> Int -> Html Msg
row ({ solar, fuel, displayPer } as model) count =
    let
        rph =
            roublesPerHour model count

        fph =
            fuelRoublesPerHour model

        roubles =
            case displayPer of
                Hour ->
                    rph

                Day ->
                    rph * 24

        roublesLessFuel =
            roubles - fph

        ( hour, min ) =
            count
                |> timePerBitcoin
                |> hourMin
                |> Tuple.mapBoth String.fromInt String.fromInt

        payback =
            format_ 2 <| paybackDaysForGpus model Nothing count

        paybackPlusFuel =
            format_ 2 <| paybackDaysForGpus model (Just fph) count
    in
    tr []
        [ td [ cellBorder ] [ text <| String.fromInt count ]
        , td [ cellBorder ] [ text <| hour ++ "h" ++ min ++ "m" ]
        , td [ cellBorder ] [ text <| format roubles ++ "₽  (" ++ format roublesLessFuel ++ "₽" ++ ")" ]
        , td [ cellBorder ] [ text <| payback ++ " (" ++ paybackPlusFuel ++ ")" ]
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


format : Int -> String
format =
    format_ 0 << toFloat


format_ : Int -> Float -> String
format_ dec =
    FormatNumber.format { usLocale | decimals = Exact dec }



--- platform helpers ---


pure : Model -> ( Model, Cmd Msg )
pure =
    do []


do : List (Cmd Msg) -> Model -> ( Model, Cmd Msg )
do cs model =
    ( model, Cmd.batch cs )
