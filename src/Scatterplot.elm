module Scatterplot exposing (main)

import Browser
import Html exposing (Html, button, div, input, label, option, select)
import Html.Attributes as HA exposing (value, selected, type_, checked)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder)
import Svg exposing (..)
import Svg.Attributes as SA


type alias DataPoint =
    { schritte : Int
    , wasserzufuhr : Float
    , schlafdauer : Float
    , kalorienverbrauch : Float
    , gesundheitszustand : String
    , healthScore : Float
    , geschlecht : String
    }


gesundheitszustandDecoder : Decoder String
gesundheitszustandDecoder =
    Decode.oneOf
        [ Decode.string
        , Decode.int |> Decode.map String.fromInt
        ]


dataPointDecoder : Decoder DataPoint
dataPointDecoder =
    Decode.map7 DataPoint
        (Decode.field "schritte" Decode.int)
        (Decode.field "wasserzufuhr" Decode.float)
        (Decode.field "schlafdauer" Decode.float)
        (Decode.field "kalorienverbrauch" Decode.float)
        (Decode.field "gesundheitszustand" gesundheitszustandDecoder)
        (Decode.field "healthScore" Decode.float)
        (Decode.field "geschlecht" Decode.string)


type alias Model =
    { dataPoints : List DataPoint
    , error : Maybe String
    , showPlot : Bool
    , xAxis : String
    , yAxis : String
    , showMen : Bool
    , showWomen : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { dataPoints = []
      , error = Nothing
      , showPlot = True
      , xAxis = "schritte"  -- "wasserzufuhr" entfernt, daher "schritte" als Standard
      , yAxis = "wasserzufuhr"
      , showMen = True
      , showWomen = True
      }
    , loadData
    )


loadData : Cmd Msg
loadData =
    Http.get
        { url = "/data/health_activity_data.json"
        , expect = Http.expectJson DataLoaded (Decode.list dataPointDecoder)
        }


type Msg
    = DataLoaded (Result Http.Error (List DataPoint))
    | TogglePlot
    | SetXAxis String
    | SetYAxis String
    | ToggleMen
    | ToggleWomen


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DataLoaded result ->
            case result of
                Ok dataPoints ->
                    ( { model | dataPoints = dataPoints, error = Nothing }, Cmd.none )

                Err httpError ->
                    ( { model | error = Just ("Fehler beim Laden der Daten: " ++ httpErrorToString httpError) }, Cmd.none )

        TogglePlot ->
            ( { model | showPlot = not model.showPlot }, Cmd.none )

        SetXAxis axis ->
            ( { model | xAxis = axis }, Cmd.none )

        SetYAxis axis ->
            ( { model | yAxis = axis }, Cmd.none )

        ToggleMen ->
            ( { model | showMen = not model.showMen }, Cmd.none )

        ToggleWomen ->
            ( { model | showWomen = not model.showWomen }, Cmd.none )


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl msg -> "Bad URL: " ++ msg
        Http.Timeout -> "Timeout"
        Http.NetworkError -> "Netzwerkfehler"
        Http.BadStatus status -> "HTTP Fehler: Status " ++ String.fromInt status
        Http.BadBody msg -> "Ungültiger Body: " ++ msg


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ div [ HA.style "font-size" "20px", HA.style "font-weight" "bold" ] [ Html.text "X-Achse: " ]
            , select [ onInput SetXAxis ]
                (axisOptions model.xAxis)
            ]
        , div []
            [ div [ HA.style "font-size" "20px", HA.style "font-weight" "bold" ] [ Html.text "Y-Achse: " ]
            , select [ onInput SetYAxis ]
                (axisOptions model.yAxis)
            ]
        , div []
            [ label []
                [ input [ type_ "checkbox", checked model.showMen, onClick ToggleMen ] []
                , Html.text " Männer"
                ]
            , label [ HA.style "margin-left" "10px" ]
                [ input [ type_ "checkbox", checked model.showWomen, onClick ToggleWomen ] []
                , Html.text " Frauen"
                ]
            ]
        , button [ onClick TogglePlot ] [ Html.text (if model.showPlot then "Plot verbergen" else "Plot anzeigen") ]
        , case model.error of
            Just errMsg ->
                div [ HA.style "color" "red" ] [ Html.text ("Fehler: " ++ errMsg) ]

            Nothing ->
                if model.showPlot then
                    scatterPlot model
                else
                    div [] [ Html.text "Plot ist verborgen." ]
        , div [] [ Html.text ("Anzahl Datenpunkte: " ++ String.fromInt (List.length model.dataPoints)) ]
        ]


axisOptions : String -> List (Html Msg)
axisOptions selectedAxis =
    [ option [ value "schritte", selected (selectedAxis == "schritte") ] [ Html.text "Schritte pro Tag" ]
    -- Wasserzufuhr und Health Score entfernt:
    -- , option [ value "wasserzufuhr", selected (selectedAxis == "wasserzufuhr") ] [ Html.text "Wasserzufuhr (Liter)" ]
    , option [ value "schlafdauer", selected (selectedAxis == "schlafdauer") ] [ Html.text "Schlafdauer" ]
    , option [ value "kalorienverbrauch", selected (selectedAxis == "kalorienverbrauch") ] [ Html.text "Kalorienverbrauch" ]
    , option [ value "gesundheitszustand", selected (selectedAxis == "gesundheitszustand") ] [ Html.text "Gesundheitszustand" ]
    -- , option [ value "healthScore", selected (selectedAxis == "healthScore") ] [ Html.text "Health Score" ]
    ]


getValue : String -> DataPoint -> Float
getValue axis dp =
    case axis of
        "schritte" -> toFloat dp.schritte
        -- "wasserzufuhr" entfernt
        "schlafdauer" -> dp.schlafdauer
        "kalorienverbrauch" -> dp.kalorienverbrauch
        "gesundheitszustand" -> -- Versuch Gesundheitszustand als Zahl zu nehmen (hier 0)
            0
        -- "healthScore" entfernt
        _ -> 0


scale : Float -> Float -> Float -> Float -> Float -> Float
scale domainMin domainMax rangeMin rangeMax value =
    let
        domainRange = domainMax - domainMin
        rangeRange = rangeMax - rangeMin
    in
    if domainRange == 0 then
        rangeMin
    else
        clamp rangeMin rangeMax (((value - domainMin) / domainRange) * rangeRange + rangeMin)


clamp : Float -> Float -> Float -> Float
clamp minVal maxVal val =
    if val < minVal then
        minVal
    else if val > maxVal then
        maxVal
    else
        val


scatterPlot : Model -> Html Msg
scatterPlot model =
    let
        width = 900
        height = 400
        margin = 50

        filteredData =
            List.filter
                (\dp ->
                    (model.showMen && dp.geschlecht == "m") ||
                    (model.showWomen && dp.geschlecht == "w")
                )
                model.dataPoints

        xs = List.map (getValue model.xAxis) filteredData
        ys = List.map (getValue model.yAxis) filteredData

        xMin = List.minimum xs |> Maybe.withDefault 0
        xMax = List.maximum xs |> Maybe.withDefault 100
        yMin = List.minimum ys |> Maybe.withDefault 0
        yMax = List.maximum ys |> Maybe.withDefault 100

        points =
            List.map
                (\dp ->
                    let
                        x = scale xMin xMax margin (toFloat (width - margin)) (getValue model.xAxis dp)
                        y = scale yMin yMax (toFloat (height - margin)) margin (getValue model.yAxis dp)
                        color = if dp.geschlecht == "m" then "blue" else "red"
                    in
                    circle
                        [ SA.cx (String.fromFloat x)
                        , SA.cy (String.fromFloat y)
                        , SA.r "3"
                        , SA.fill color
                        ]
                        []
                )
                filteredData

        xAxisLine =
            line
                [ SA.x1 (String.fromInt margin)
                , SA.y1 (String.fromInt (height - margin))
                , SA.x2 (String.fromInt (width - margin))
                , SA.y2 (String.fromInt (height - margin))
                , SA.stroke "black"
                ]
                []

        yAxisLine =
            line
                [ SA.x1 (String.fromInt margin)
                , SA.y1 (String.fromInt margin)
                , SA.x2 (String.fromInt margin)
                , SA.y2 (String.fromInt (height - margin))
                , SA.stroke "black"
                ]
                []

        xAxisLabel =
            text_
                [ SA.x (String.fromInt (width // 2))
                , SA.y (String.fromInt (height - 10))
                , SA.textAnchor "middle"
                , SA.fontSize "24"
                ]
                [ Svg.text (axisLabel model.xAxis) ]

        yAxisLabel =
            text_
                [ SA.transform ("translate(15," ++ String.fromInt (height // 2) ++ ") rotate(-90)")
                , SA.textAnchor "middle"
                , SA.fontSize "24"
                ]
                [ Svg.text (axisLabel model.yAxis) ]
    in
    Svg.svg
        [ SA.width (String.fromInt width)
        , SA.height (String.fromInt height)
        , SA.style "border:1px solid black"
        ]
        ([ xAxisLine, yAxisLine, xAxisLabel, yAxisLabel ] ++ points)


axisLabel : String -> String
axisLabel axis =
    case axis of
        "schritte" -> "Schritte pro Tag"
        -- "wasserzufuhr" entfernt
        "schlafdauer" -> "Schlafdauer"
        "kalorienverbrauch" -> "Kalorienverbrauch"
        "gesundheitszustand" -> "Gesundheitszustand"
        -- "healthScore" entfernt
        _ -> ""


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
