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

import Csv exposing (parse)
import Http
import Html exposing (Html, button, div, option, select, text)
import Html.Attributes as HtmlAttr
import Html.Events as HtmlEvents
import Svg exposing (..)
import Svg.Attributes as SvgAttr
import String
import Json.Decode exposing (int)

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


type alias Person = 
    { id : Int
    , age : Int
    , gender : String
    , heightCm : Int
    , weightKg : Int
    , bmi : Float
    , dailySteps : Int
    , caloriesIntake : Int
    , hoursOfSleep : Float
    , heartRate : Int
    , bloodPresure : String
    , exerciseHours : Float
    , smoker : String
    , alcoholConsumption : Int
    , diabetic : String
    , heartDisease : String 
    }

rowToPerson : List String -> Maybe Person
rowToPerson row =
    case row of
        idStr :: ageStr :: genderStr :: heightStr :: weightStr :: bmiStr :: dailyStepsStr :: caloriesIntakeStr :: hoursOfSleepStr :: heartRateStr :: bloodPresureStr :: exerciseHoursStr :: smokerStr :: alcoholConsumptionStr :: diabeticStr :: heartDiseaseStr :: [] ->
            case ( String.toInt idStr
                 , String.toInt ageStr
                 , String.toInt heightStr
                 , String.toInt weightStr
                 , String.toFloat bmiStr
                 , String.toInt dailyStepsStr
                 , String.toInt caloriesIntakeStr
                 , String.toFloat hoursOfSleepStr
                 , String.toInt heartRateStr
                 , String.toFloat exerciseHoursStr
                 , String.toInt alcoholConsumptionStr
                 ) of
                ( Just id
                , Just age
                , Just heightCm
                , Just weightKg
                , Just bmi
                , Just dailySteps
                , Just caloriesIntake
                , Just hoursOfSleep
                , Just heartRate
                , Just exerciseHours
                , Just alcoholConsumption
                ) ->
                    Just
                        { id = id
                        , age = age
                        , gender = genderStr
                        , heightCm = heightCm
                        , weightKg = weightKg
                        , bmi = bmi
                        , dailySteps = dailySteps
                        , caloriesIntake = caloriesIntake
                        , hoursOfSleep = hoursOfSleep
                        , heartRate = heartRate
                        , bloodPresure = bloodPresureStr
                        , exerciseHours = exerciseHours
                        , smoker = smokerStr
                        , alcoholConsumption = alcoholConsumption
                        , diabetic = diabeticStr
                        , heartDisease = heartDiseaseStr
                        }
                _ ->
                    Nothing
        _ ->
            Nothing


init : () -> ( Model, Cmd Msg, Cmd Msg )
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
    , loadCsv
    , Cmd.none
    )


loadCsv : Cmd Msg
loadCsv =
    -- Beispiel für HTTP-Request:
    Http.get
        { url = "data/health_activity_data.csv"
        , expect = Http.expectString CsvLoaded
        }



sampleData : List DataPoint
sampleData =
    [ { schritte = 5000, wasserzufuhr = 2, schlafdauer = 7, kalorienverbrauch = 2200, gesundheitszustand = 3, healthScore = 75, stressLevel = 2, altersgruppe = "30-40", geschlecht = "m" }
    , { schritte = 8000, wasserzufuhr = 1.5, schlafdauer = 6, kalorienverbrauch = 2500, gesundheitszustand = 4, healthScore = 80, stressLevel = 3, altersgruppe = "20-30", geschlecht = "w" }
    , { schritte = 4000, wasserzufuhr = 2.5, schlafdauer = 8, kalorienverbrauch = 2000, gesundheitszustand = 2, healthScore = 60, stressLevel = 1, altersgruppe = "40-50", geschlecht = "m" }
    , { schritte = 7000, wasserzufuhr = 2.2, schlafdauer = 7, kalorienverbrauch = 2300, gesundheitszustand = 4, healthScore = 78, stressLevel = 2, altersgruppe = "30-40", geschlecht = "w" }
    , { schritte = 6000, wasserzufuhr = 1.8, schlafdauer = 6.5, kalorienverbrauch = 2100, gesundheitszustand = 3, healthScore = 70, stressLevel = 2, altersgruppe = "40-50", geschlecht = "m" }
    , { schritte = 9000, wasserzufuhr = 1.4, schlafdauer = 5.5, kalorienverbrauch = 2600, gesundheitszustand = 5, healthScore = 85, stressLevel = 3, altersgruppe = "20-30", geschlecht = "w" }
    , { schritte = 3500, wasserzufuhr = 2.8, schlafdauer = 8, kalorienverbrauch = 1900, gesundheitszustand = 2, healthScore = 58, stressLevel = 1, altersgruppe = "50-60", geschlecht = "m" }
    , { schritte = 4500, wasserzufuhr = 2, schlafdauer = 7.5, kalorienverbrauch = 2050, gesundheitszustand = 3, healthScore = 65, stressLevel = 2, altersgruppe = "30-40", geschlecht = "w" }
    , { schritte = 8500, wasserzufuhr = 1.6, schlafdauer = 6, kalorienverbrauch = 2550, gesundheitszustand = 4, healthScore = 82, stressLevel = 3, altersgruppe = "20-30", geschlecht = "m" }
    , { schritte = 4000, wasserzufuhr = 2.4, schlafdauer = 7, kalorienverbrauch = 1950, gesundheitszustand = 2, healthScore = 60, stressLevel = 1, altersgruppe = "40-50", geschlecht = "w" }
    , { schritte = 7800, wasserzufuhr = 1.7, schlafdauer = 6.5, kalorienverbrauch = 2400, gesundheitszustand = 4, healthScore = 77, stressLevel = 2, altersgruppe = "30-40", geschlecht = "m" }
    , { schritte = 5200, wasserzufuhr = 2.1, schlafdauer = 7.2, kalorienverbrauch = 2150, gesundheitszustand = 3, healthScore = 69, stressLevel = 2, altersgruppe = "50-60", geschlecht = "w" }
    , { schritte = 9200, wasserzufuhr = 1.3, schlafdauer = 5.8, kalorienverbrauch = 2700, gesundheitszustand = 5, healthScore = 88, stressLevel = 3, altersgruppe = "20-30", geschlecht = "m" }
    , { schritte = 3000, wasserzufuhr = 2.7, schlafdauer = 8.5, kalorienverbrauch = 1800, gesundheitszustand = 2, healthScore = 55, stressLevel = 1, altersgruppe = "60-70", geschlecht = "w" }
    , { schritte = 6500, wasserzufuhr = 2, schlafdauer = 7, kalorienverbrauch = 2250, gesundheitszustand = 3, healthScore = 72, stressLevel = 2, altersgruppe = "40-50", geschlecht = "m" }
    , { schritte = 8300, wasserzufuhr = 1.5, schlafdauer = 6.2, kalorienverbrauch = 2500, gesundheitszustand = 4, healthScore = 80, stressLevel = 3, altersgruppe = "30-40", geschlecht = "w" }
    , { schritte = 4700, wasserzufuhr = 2.3, schlafdauer = 7.3, kalorienverbrauch = 2100, gesundheitszustand = 3, healthScore = 66, stressLevel = 2, altersgruppe = "50-60", geschlecht = "m" }
    , { schritte = 7800, wasserzufuhr = 1.6, schlafdauer = 6.1, kalorienverbrauch = 2450, gesundheitszustand = 4, healthScore = 79, stressLevel = 3, altersgruppe = "20-30", geschlecht = "w" }
    , { schritte = 3900, wasserzufuhr = 2.5, schlafdauer = 7.8, kalorienverbrauch = 2000, gesundheitszustand = 2, healthScore = 61, stressLevel = 1, altersgruppe = "40-50", geschlecht = "m" }
    , { schritte = 6000, wasserzufuhr = 1.9, schlafdauer = 6.7, kalorienverbrauch = 2200, gesundheitszustand = 3, healthScore = 73, stressLevel = 2, altersgruppe = "30-40", geschlecht = "w" }
    ]


-- NACHRICHTEN

type Msg
    = DataLoaded (Result Http.Error (List DataPoint))
    | TogglePlot
    | SetXAxis String
    | SetYAxis String
    | ToggleMen
    | ToggleWomen


-- UPDATE

type Msg
    = ChangeX String
    | ChangeY String
    | TogglePlot
    | CsvLoaded (Result Http.Error String)


>>>>>>> main
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
        CsvLoaded (Ok csvString) ->
            let
                csv = parse csvString
            in
                case csv of
                    Csv.Ok rows ->
                        let
                            -- Überspringe die Header-Zeile
                            dataRows = List.drop 1 rows
                            persons = List.filterMap rowToPerson dataRows
                        in
                            -- persons ist jetzt eine Liste von Person-Records
                            ( { model | data = persons }, Cmd.none )
                    Csv.Err err ->
                        -- Fehlerbehandlung für CSV-Parsing-Fehler
                        ( model, Cmd.none )

        CsvLoaded (Err httpError) ->
            -- Fehlerbehandlung für HTTP-Fehler
            ( model, Cmd.none )


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

        _ ->
            "gray"

-- MAIN

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
