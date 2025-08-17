module Main exposing (main)

import Browser
import Csv.Parser exposing (parse)
import Http
import Html exposing (Html, button, div, option, select, text)
import Html.Attributes as HtmlAttr
import Html.Events as HtmlEvents
import Svg exposing (..)
import Svg.Attributes as SvgAttr
import String
import Json.Decode exposing (int)

-- MODEL

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

type alias Model =
    { data : List Person
    , selectedX : String
    , selectedY : String
    , showPlot : Bool
    }


rowToPerson : List String -> Maybe Person
rowToPerson row =
    case row of
        idStr :: ageStr :: genderStr :: heightStr :: weightStr :: bmiStr :: dailyStepsStr :: caloriesIntakeStr :: hoursOfSleepStr :: heartRateStr :: bloodPresureStr :: exerciseHoursStr :: smokerStr :: alcoholConsumptionStr :: diabeticStr :: heartDiseaseStr :: [] ->
            Just
                { id = idStr |> String.toInt |> Maybe.withDefault 0
                , age = ageStr |> String.toInt |> Maybe.withDefault 0
                , gender = genderStr
                , heightCm = heightStr |> String.toInt |> Maybe.withDefault 0
                , weightKg = weightStr |> String.toInt |> Maybe.withDefault 0
                , bmi = bmiStr |> String.toFloat |> Maybe.withDefault 0
                , dailySteps = dailyStepsStr |> String.toInt |> Maybe.withDefault 0
                , caloriesIntake = caloriesIntakeStr |> String.toInt |> Maybe.withDefault 0
                , hoursOfSleep = hoursOfSleepStr |> String.toFloat |> Maybe.withDefault 0
                , heartRate = heartRateStr |> String.toInt |> Maybe.withDefault 0
                , bloodPresure = bloodPresureStr
                , exerciseHours = exerciseHoursStr |> String.toFloat |> Maybe.withDefault 0
                , smoker = smokerStr
                , alcoholConsumption = alcoholConsumptionStr |> String.toInt |> Maybe.withDefault 0
                , diabetic = diabeticStr
                , heartDisease = heartDiseaseStr
                }
        _ ->
            Nothing


init : () -> ( Model, Cmd Msg )
init _ =
    ( { data = []
      , selectedX = "Schritte"
      , selectedY = "Kalorienaufnahme"
      , showPlot = True
      }
    , loadCsv
    )


loadCsv : Cmd Msg
loadCsv =
    -- Beispiel für HTTP-Request:
    Http.get
        { url = "data/health_activity_data.csv" 
        , expect = Http.expectString CsvLoaded
        }



sampleData : List Person
sampleData =
    [ { age = 50, alcoholConsumption = 0, bloodPresure = "60/100", bmi = 80.5, caloriesIntake = 500, dailySteps = 1000, diabetic = "no", exerciseHours = 3.6, gender = "f", heartDisease = "no", heartRate = 70, heightCm = 170, hoursOfSleep = 7.5, id = 1, smoker = "no", weightKg = 75}
    , { age = 30, alcoholConsumption = 2, bloodPresure = "60/100", bmi = 120, caloriesIntake = 700, dailySteps = 5000, diabetic = "no", exerciseHours = 5.2, gender = "m", heartDisease = "no", heartRate = 62, heightCm = 190, hoursOfSleep = 7.3, id = 1, smoker = "no", weightKg = 86}
    , { age = 60, alcoholConsumption = 5, bloodPresure = "60/100", bmi = 75, caloriesIntake = 1000, dailySteps = 800, diabetic = "yes", exerciseHours = 0.0, gender = "f", heartDisease = "yes", heartRate = 60, heightCm = 160, hoursOfSleep = 8.1, id = 1, smoker = "no", weightKg = 73}
    ]



-- UPDATE

type Msg
    = ChangeX String
    | ChangeY String
    | TogglePlot
    | CsvLoaded (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeX newX ->
            ( { model | selectedX = newX }, Cmd.none )

        ChangeY newY ->
            ( { model | selectedY = newY }, Cmd.none )

        TogglePlot ->
            ( { model | showPlot = not model.showPlot }, Cmd.none )

        CsvLoaded (Ok csvString) ->
            let
                -- csv = Decode.decodeCsv csvString
                csv = parse { fieldSeparator = ','} csvString
            in
                case csv of
                    Ok rows ->
                        let
                            -- Überspringe die Header-Zeile
                            dataRows = List.drop 1 rows
                            persons = List.filterMap rowToPerson dataRows
                        in
                            -- persons ist jetzt eine Liste von Person-Records
                            ( { model | data = persons }, Cmd.none )
                    Err err ->
                        -- Fehlerbehandlung für CSV-Parsing-Fehler

                        ( { model | data = sampleData }, Cmd.none )

        CsvLoaded (Err httpError) ->
            -- Fehlerbehandlung für HTTP-Fehler
            Debug.log "HTTP-Fehler aufgetreten"     -- wird das angezeigt?
            ( { model | data = sampleData }, Cmd.none )


-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ div []
            [ Html.text "X-Achse: "
            , axisSelectX model.selectedX
            ]
        , div []
            [ Html.text "Y-Achse: "
            , axisSelectY model.selectedY
            ]
        , div [ HtmlAttr.style "margin" "10px 0" ]
            [ button [ HtmlEvents.onClick TogglePlot ]
                [ Html.text (if model.showPlot then "Plot verbergen" else "Plot anzeigen") ]
            ]
        , if model.showPlot then
            scatterPlotView model
          else
            Html.text ""
        ]


axisSelectX : String -> Html Msg
axisSelectX selected =
    select [ HtmlEvents.onInput ChangeX ]
        [ option [ HtmlAttr.value "Schritte", HtmlAttr.selected (selected == "Schritte") ] [ Html.text "Schritte" ] 
        , option [ HtmlAttr.value "Alkoholkonsum (pro Woche)", HtmlAttr.selected (selected == "Alkoholkonsum (pro Woche)") ] [ Html.text "Alkoholkonsum (pro Woche)" ]
        , option [ HtmlAttr.value "Trainingsstunden (pro Woche)", HtmlAttr.selected (selected == "Trainingsstunden (pro Woche)") ] [ Html.text "Trainingsstunden (pro Woche)" ]
        , option [ HtmlAttr.value "Kalorienaufnahme", HtmlAttr.selected (selected == "Kalorienaufnahme") ] [ Html.text "Kalorienaufnahme" ]
        ]


axisSelectY : String -> Html Msg
axisSelectY selected =
    select [ HtmlEvents.onInput ChangeY ]
        [ option [ HtmlAttr.value "Schlafdauer", HtmlAttr.selected (selected == "Schlafdauer") ] [ Html.text "Schlafdauer" ]
        , option [ HtmlAttr.value "BMI", HtmlAttr.selected (selected == "BMI") ] [ Html.text "BMI" ]
        , option [ HtmlAttr.value "Herzfrequenz", HtmlAttr.selected (selected == "Herzfrequenz") ] [ Html.text "Herzfrequenz" ]
        ]


-- Hilfsfunktion zum dynamischen Zugriff auf Werte je Achse

getValueForAxis : Person -> String -> Float
getValueForAxis dp axis =
    case axis of
        "Schritte" ->
            toFloat dp.dailySteps

        "Alkoholkonsum (pro Woche)" ->
            toFloat dp.alcoholConsumption

        "Schlafdauer" ->
            dp.hoursOfSleep

        "Kalorienaufnahme" ->
            toFloat dp.caloriesIntake

        "BMI" ->
            dp.bmi

        "Trainingsstunden (pro Woche)" ->
            dp.exerciseHours

        "Herzfrequenz" ->
            toFloat dp.heartRate

        _ ->
            0


-- SCATTERPLOT SVG

width : Float
width = 600

height : Float
height = 400

padding : Float
padding = 40


scatterPlotView : Model -> Html Msg
scatterPlotView model =
    let
        allXValues =
            List.map (\dp -> getValueForAxis dp model.selectedX) model.data

        allYValues =
            List.map (\dp -> getValueForAxis dp model.selectedY) model.data

        minX =
            List.minimum allXValues |> Maybe.withDefault 0

        maxX =
            List.maximum allXValues |> Maybe.withDefault 1

        minY =
            List.minimum allYValues |> Maybe.withDefault 0

        maxY =
            List.maximum allYValues |> Maybe.withDefault 1

        scaleX v =
            padding + ((v - minX) / (maxX - minX)) * (width - 2 * padding)

        scaleY v =
            height - padding - ((v - minY) / (maxY - minY)) * (height - 2 * padding)

        -- Achsenlinien
        xAxis =
            line
                [ SvgAttr.x1 (String.fromFloat padding)
                , SvgAttr.y1 (String.fromFloat (height - padding))
                , SvgAttr.x2 (String.fromFloat (width - padding))
                , SvgAttr.y2 (String.fromFloat (height - padding))
                , SvgAttr.stroke "black"
                , SvgAttr.strokeWidth "2"
                ]
                []

        yAxis =
            line
                [ SvgAttr.x1 (String.fromFloat padding)
                , SvgAttr.y1 (String.fromFloat padding)
                , SvgAttr.x2 (String.fromFloat padding)
                , SvgAttr.y2 (String.fromFloat (height - padding))
                , SvgAttr.stroke "black"
                , SvgAttr.strokeWidth "2"
                ]
                []

        -- Achsen-Beschriftung
        xLabel =
            text_
                [ SvgAttr.x (String.fromFloat (width / 2))
                , SvgAttr.y (String.fromFloat (height - 5))
                , SvgAttr.textAnchor "middle"
                , SvgAttr.fontSize "14"
                ]
                [ Svg.text model.selectedX ]

        yLabel =
            text_
                [ SvgAttr.x "15"
                , SvgAttr.y (String.fromFloat (height / 2))
                , SvgAttr.textAnchor "middle"
                , SvgAttr.fontSize "14"
                , SvgAttr.transform ("rotate(-90 15 " ++ String.fromFloat (height / 2) ++ ")")
                ]
                [ Svg.text model.selectedY ]

        -- Punkte
        points =
            List.map
                (\dp ->
                    circle
                        [ SvgAttr.cx (String.fromFloat (scaleX (getValueForAxis dp model.selectedX)))
                        , SvgAttr.cy (String.fromFloat (scaleY (getValueForAxis dp model.selectedY)))
                        , SvgAttr.r "6"
                        , SvgAttr.fill (colorByGender dp.gender)
                        , SvgAttr.stroke (colorByStress dp.heartRate)
                        , SvgAttr.strokeWidth "2"
                        , SvgAttr.opacity "0.8"
                        , SvgAttr.title ("Herzfrequenz: " ++ String.fromInt dp.heartRate ++ ", Alter: " ++ String.fromInt dp.age)
                        ]
                        []
                ) 
                model.data
    in
    Svg.svg
        [ SvgAttr.width (String.fromFloat width)
        , SvgAttr.height (String.fromFloat height)
        , SvgAttr.style "border: 1px solid black; background: white;"
        ]
        ( [ xAxis, yAxis, xLabel, yLabel ] ++ points )


-- Farb-Funktionen (Beispiel)

colorByGender : String -> String
colorByGender g =
    case g of
        "m" ->
            "steelblue"

        "w" ->
            "orange"

        _ ->
            "gray"


colorByStress : Int -> String
colorByStress stress =
    case stress of
        1 ->
            "green"

        2 ->
            "yellow"

        3 ->
            "red"

        _ ->
            "gray"

-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }