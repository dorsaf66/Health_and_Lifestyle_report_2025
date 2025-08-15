module Scatterplot exposing (main)

import Browser
import Html exposing (Html, button, div, input, label, option, select)
import Html as Html
import Html.Attributes as HA exposing (value, selected, type_, checked)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder)
import Svg exposing (..)
import Svg as Svg
import Svg.Attributes as SA


-- DATENTYPEN

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
      , xAxis = "schritte"
      , yAxis = "kalorienverbrauch"
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


-- NACHRICHTEN

type Msg
    = DataLoaded (Result Http.Error (List DataPoint))
    | TogglePlot
    | SetXAxis String
    | SetYAxis String
    | ToggleMen
    | ToggleWomen


-- UPDATE

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


-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ div []
            [ div [ HA.style "font-size" "20px", HA.style "font-weight" "bold" ] [ Html.text "X-Achse: " ]
            , select [ onInput SetXAxis ]
                (xAxisOptions model.xAxis)
            ]
        , div []
            [ div [ HA.style "font-size" "20px", HA.style "font-weight" "bold" ] [ Html.text "Y-Achse: " ]
            , select [ onInput SetYAxis ]
                (yAxisOptions model.yAxis)
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



xAxisOptions : String -> List (Html Msg)
xAxisOptions selectedAxis =
    [ option [ value "schritte", selected (selectedAxis == "schritte") ] [ Html.text "Schritte pro Tag" ]
    , option [ value "wasserzufuhr", selected (selectedAxis == "wasserzufuhr") ] [ Html.text "Wasserzufuhr (Liter)" ]
    , option [ value "schlafdauer", selected (selectedAxis == "schlafdauer") ] [ Html.text "Schlafdauer (Stunden)" ]
    ]


yAxisOptions : String -> List (Html Msg)
yAxisOptions selectedAxis =
    [ option [ value "kalorienverbrauch", selected (selectedAxis == "kalorienverbrauch") ] [ Html.text "Kalorienverbrauch" ]
    , option [ value "gesundheitszustand", selected (selectedAxis == "gesundheitszustand") ] [ Html.text "Gesundheitszustand" ]
    , option [ value "healthScore", selected (selectedAxis == "healthScore") ] [ Html.text "Health Score" ]
    ]


-- WERT-UMRECHNUNG

gesundheitszustandToFloat : String -> Float
gesundheitszustandToFloat str =
    case String.toLower str of
        "schlecht" -> 1
        "mittel" -> 2
        "gut" -> 3
        _ -> 0


getValue : String -> DataPoint -> Float
getValue axis dp =
    case axis of
        "schritte" -> toFloat dp.schritte
        "wasserzufuhr" -> dp.wasserzufuhr
        "schlafdauer" -> dp.schlafdauer
        "kalorienverbrauch" -> dp.kalorienverbrauch
        "gesundheitszustand" -> gesundheitszustandToFloat dp.gesundheitszustand
        "healthScore" -> dp.healthScore
        _ -> 0


-- PLOT-ZEICHNUNG

scatterPlot : Model -> Html Msg
scatterPlot model =
    let
        width = 1200
        height = 700
        margin = 70

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
        xMax = List.maximum xs |> Maybe.withDefault 1
        yMin = List.minimum ys |> Maybe.withDefault 0
        yMax = List.maximum ys |> Maybe.withDefault 1

        scaleX val = scale xMin xMax (toFloat margin) (toFloat (width - margin)) val
        scaleY val = scale yMin yMax (toFloat (height - margin)) (toFloat margin) val

        points =
            List.map
                (\dp ->
                    let
                        x = scaleX (getValue model.xAxis dp)
                        y = scaleY (getValue model.yAxis dp)
                        color = if dp.geschlecht == "m" then "blue" else "red"
                    in
                    Svg.circle
                        [ SA.cx (String.fromFloat x)
                        , SA.cy (String.fromFloat y)
                        , SA.r "3"
                        , SA.fill color
                        ]
                        []
                )
                filteredData

        xTicks =
            axisTicks xMin xMax 5
                |> List.map (\val -> tickLabel val (scaleX val) (toFloat (height - margin + 20)) (String.fromFloat (round1 val)))

        yTicks =
            case model.yAxis of
                "gesundheitszustand" ->
                    [ (1, "schlecht"), (2, "mittel"), (3, "gut") ]
                        |> List.map (\(val, lbl) -> tickLabelY (scaleY val) lbl)

                "healthScore" ->
                    List.range 0 10
                        |> List.map (\i -> tickLabelY (scaleY (toFloat (i * 10))) (String.fromInt (i * 10)))

                _ ->
                    axisTicks yMin yMax 5
                        |> List.map (\val -> tickLabelY (scaleY val) (String.fromFloat (round1 val)))

        xAxisLine =
            Svg.line
                [ SA.x1 (String.fromInt margin)
                , SA.y1 (String.fromInt (height - margin))
                , SA.x2 (String.fromInt (width - margin))
                , SA.y2 (String.fromInt (height - margin))
                , SA.stroke "black"
                ]
                []

        yAxisLine =
            Svg.line
                [ SA.x1 (String.fromInt margin)
                , SA.y1 (String.fromInt margin)
                , SA.x2 (String.fromInt margin)
                , SA.y2 (String.fromInt (height - margin))
                , SA.stroke "black"
                ]
                []

        xAxisLabel =
            Svg.text_
                [ SA.x (String.fromInt (width // 2))
                , SA.y (String.fromInt (height - 10))
                , SA.textAnchor "middle"
                , SA.fontSize "20"
                ]
                [ Svg.text (axisLabel model.xAxis) ]

        yAxisLabel =
            Svg.text_
                [ SA.transform ("translate(20," ++ String.fromInt (height // 2) ++ ") rotate(-90)")
                , SA.textAnchor "middle"
                , SA.fontSize "20"
                ]
                [ Svg.text (axisLabel model.yAxis) ]
    in
    Svg.svg
        [ SA.width (String.fromInt width)
        , SA.height (String.fromInt height)
        , SA.style "border:1px solid black"
        ]
        ([ xAxisLine, yAxisLine, xAxisLabel, yAxisLabel ] ++ xTicks ++ yTicks ++ points)


-- HELFER

axisTicks : Float -> Float -> Int -> List Float
axisTicks minVal maxVal count =
    let
        step = (maxVal - minVal) / toFloat count
    in
    List.map (\i -> minVal + step * toFloat i) (List.range 0 count)


tickLabel : Float -> Float -> Float -> String -> Svg.Svg Msg
tickLabel val x y lbl =
    Svg.text_
        [ SA.x (String.fromFloat x)
        , SA.y (String.fromFloat y)
        , SA.textAnchor "middle"
        , SA.fontSize "14"
        ]
        [ Svg.text lbl ]


tickLabelY : Float -> String -> Svg.Svg Msg
tickLabelY y lbl =
    Svg.text_
        [ SA.x "65"
        , SA.y (String.fromFloat y)
        , SA.textAnchor "end"
        , SA.fontSize "14"
        ]
        [ Svg.text lbl ]


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


round1 : Float -> Float
round1 num =
    (toFloat (round (num * 10))) / 10


axisLabel : String -> String
axisLabel axis =
    case axis of
        "schritte" -> "Schritte pro Tag"
        "wasserzufuhr" -> "Wasserzufuhr (Liter)"
        "schlafdauer" -> "Schlafdauer (Stunden)"
        "kalorienverbrauch" -> "Kalorienverbrauch"
        "gesundheitszustand" -> "Gesundheitszustand"
        "healthScore" -> "Health Score"
        _ -> ""


-- MAIN

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
