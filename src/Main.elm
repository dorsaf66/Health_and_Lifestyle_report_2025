module Main exposing (main)

import Browser
import Csv.Parser exposing (parse)
import Http
import Html exposing (Html, button, div, option, select, text, input, label)
import Html.Attributes as HtmlAttr
import Html.Events as HtmlEvents
import Svg exposing (..)
import Svg.Attributes as SvgAttr
import String


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
    , bloodPressure : String
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
    , showMale : Bool
    , showFemale : Bool
    , showSmoker : Bool
    , showDiabetic : Bool
    , showHeartDisease : Bool
    }


-- CSV PARSING

rowToPerson : List String -> Maybe Person
rowToPerson row =
    case row of
        idStr :: ageStr :: genderStr :: heightStr :: weightStr :: bmiStr :: dailyStepsStr :: caloriesIntakeStr :: hoursOfSleepStr :: heartRateStr :: bloodPressureStr :: exerciseHoursStr :: smokerStr :: alcoholConsumptionStr :: diabeticStr :: heartDiseaseStr :: [] ->
            Just
                { id = String.toInt idStr |> Maybe.withDefault 0
                , age = String.toInt ageStr |> Maybe.withDefault 0
                , gender = genderStr
                , heightCm = String.toInt heightStr |> Maybe.withDefault 0
                , weightKg = String.toInt weightStr |> Maybe.withDefault 0
                , bmi = String.toFloat bmiStr |> Maybe.withDefault 0
                , dailySteps = String.toInt dailyStepsStr |> Maybe.withDefault 0
                , caloriesIntake = String.toInt caloriesIntakeStr |> Maybe.withDefault 0
                , hoursOfSleep = String.toFloat hoursOfSleepStr |> Maybe.withDefault 0
                , heartRate = String.toInt heartRateStr |> Maybe.withDefault 0
                , bloodPressure = bloodPressureStr
                , exerciseHours = String.toFloat exerciseHoursStr |> Maybe.withDefault 0
                , smoker = smokerStr
                , alcoholConsumption = String.toInt alcoholConsumptionStr |> Maybe.withDefault 0
                , diabetic = diabeticStr
                , heartDisease = heartDiseaseStr
                }
        _ ->
            Nothing


-- INIT

init : () -> ( Model, Cmd Msg )
init _ =
    ( { data = []
      , selectedX = "Schritte"
      , selectedY = "Kalorienaufnahme"
      , showPlot = True
      , showMale = True
      , showFemale = True
      , showSmoker = False
      , showDiabetic = False
      , showHeartDisease = False
      }
    , loadCsv
    )


loadCsv : Cmd Msg
loadCsv =
    Http.get
        { url = "data/health_activity_data.csv"
        , expect = Http.expectString CsvLoaded
        }


-- UPDATE

type Msg
    = ChangeX String
    | ChangeY String
    | TogglePlot
    | CsvLoaded (Result Http.Error String)
    | ToggleMale Bool
    | ToggleFemale Bool
    | ToggleSmoker Bool
    | ToggleDiabetic Bool
    | ToggleHeartDisease Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeX newX ->
            ( { model | selectedX = newX }, Cmd.none )

        ChangeY newY ->
            ( { model | selectedY = newY }, Cmd.none )

        TogglePlot ->
            ( { model | showPlot = not model.showPlot }, Cmd.none )

        ToggleMale val ->
            ( { model | showMale = val }, Cmd.none )

        ToggleFemale val ->
            ( { model | showFemale = val }, Cmd.none )

        ToggleSmoker val ->
            ( { model | showSmoker = val }, Cmd.none )

        ToggleDiabetic val ->
            ( { model | showDiabetic = val }, Cmd.none )

        ToggleHeartDisease val ->
            ( { model | showHeartDisease = val }, Cmd.none )

        CsvLoaded (Ok csvString) ->
            case parse { fieldSeparator = ',' } csvString of
                Ok rows ->
                    let
                        dataRows = List.drop 1 rows
                        persons = List.filterMap rowToPerson dataRows
                    in
                    ( { model | data = persons }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        CsvLoaded (Err _) ->
            ( model, Cmd.none )


-- VIEW

view : Model -> Html Msg
view model =
    div [ HtmlAttr.style "font-family" "Arial, sans-serif", HtmlAttr.style "margin" "20px" ]
        [ div [ HtmlAttr.style "margin-bottom" "10px" ]
            [ Html.text "X-Achselolo: "
            , axisSelectX model.selectedX
            ]
        , div [ HtmlAttr.style "margin-bottom" "10px" ]
            [ Html.text "Y-Achse: "
            , axisSelectY model.selectedY
            ]
        , div [ HtmlAttr.style "margin" "10px 0" ]
            [ button [ HtmlEvents.onClick TogglePlot ]
                [ Html.text (if model.showPlot then "Plot verbergen" else "Plot anzeigen") ]
            ]
        , div [ HtmlAttr.style "margin-bottom" "10px" ]
            [ labelCheckbox "Männer" model.showMale ToggleMale
            , labelCheckbox "Frauen" model.showFemale ToggleFemale
            ]
        , div [ HtmlAttr.style "margin-bottom" "10px" ]
            [ labelCheckbox "Raucher" model.showSmoker ToggleSmoker
            , labelCheckbox "Diabetiker" model.showDiabetic ToggleDiabetic
            , labelCheckbox "Herzkrankheit" model.showHeartDisease ToggleHeartDisease
            ]
        , if model.showPlot then
            scatterPlotView model
          else
            Html.text ""
        ]


labelCheckbox : String -> Bool -> (Bool -> Msg) -> Html Msg
labelCheckbox labelText checked toMsg =
    label [ HtmlAttr.style "margin-right" "15px" ]
        [ input [ HtmlAttr.type_ "checkbox", HtmlAttr.checked checked, HtmlEvents.onCheck toMsg ] []
        , Html.text (" " ++ labelText)
        ]


axisSelectX : String -> Html Msg
axisSelectX selected =
    select [ HtmlEvents.onInput ChangeX ]
        [ option [ HtmlAttr.value "Schritte", HtmlAttr.selected (selected == "Schritte") ] [ Html.text "Schritte" ]
        , option [ HtmlAttr.value "Alkoholkonsum pro Woche", HtmlAttr.selected (selected == "Alkoholkonsum pro Woche") ] [ Html.text "Alkoholkonsum pro Woche" ]
        , option [ HtmlAttr.value "Schlafdauer", HtmlAttr.selected (selected == "Schlafdauer") ] [ Html.text "Schlafdauer" ]
        , option [ HtmlAttr.value "Herzfrequenz", HtmlAttr.selected (selected == "Herzfrequenz") ] [ Html.text "Herzfrequenz" ]
        ]


axisSelectY : String -> Html Msg
axisSelectY selected =
    select [ HtmlEvents.onInput ChangeY ]
        [ option [ HtmlAttr.value "Kalorienaufnahme", HtmlAttr.selected (selected == "Kalorienaufnahme") ] [ Html.text "Kalorienaufnahme" ]
        , option [ HtmlAttr.value "BMI", HtmlAttr.selected (selected == "BMI") ] [ Html.text "BMI" ]
        , option [ HtmlAttr.value "Trainingsstunden pro Woche", HtmlAttr.selected (selected == "Trainingsstunden pro Woche") ] [ Html.text "Trainingsstunden pro Woche" ]
        ]


-- HILFSFUNKTION

getValueForAxis : Person -> String -> Float
getValueForAxis dp axis =
    case axis of
        "Schritte" -> toFloat dp.dailySteps
        "Alkoholkonsum pro Woche" -> toFloat dp.alcoholConsumption
        "Schlafdauer" -> dp.hoursOfSleep
        "Kalorienaufnahme" -> toFloat dp.caloriesIntake
        "BMI" -> dp.bmi
        "Trainingsstunden pro Woche" -> dp.exerciseHours
        "Herzfrequenz" -> toFloat dp.heartRate
        _ -> 0


-- SCATTERPLOT

width : Float
width = 800

height : Float
height = 600

padding : Float
padding = 50


ticksForAxis : String -> List Float
ticksForAxis axis =
    case axis of
        "Schritte" -> [ 0, 5000, 10000, 15000, 20000 ]
        "Alkoholkonsum pro Woche" -> [ 0, 2, 4, 6, 8, 10 ]
        "Trainingsstunden pro Woche" -> [ 0, 2, 4, 6, 8, 10 ]
        "Kalorienaufnahme" -> [ 0, 1000, 1500, 2000, 2500, 3000, 3500 ]
        "Schlafdauer" -> [ 0, 2, 4, 6, 8, 10 ]
        "BMI" -> [ 0, 15, 20, 25, 30 ]
        "Herzfrequenz" -> [ 0, 40, 80, 100, 140 ]
        _ -> []


scatterPlotView : Model -> Html Msg
scatterPlotView model =
    let
        filteredData =
            List.filter
                (\dp ->
                    ((model.showMale && dp.gender == "Male") || (model.showFemale && dp.gender == "Female"))
                        && (not model.showSmoker || dp.smoker == "Yes")
                        && (not model.showDiabetic || dp.diabetic == "Yes")
                        && (not model.showHeartDisease || dp.heartDisease == "Yes")
                )
                model.data

        xs = List.map (\dp -> getValueForAxis dp model.selectedX) filteredData
        ys = List.map (\dp -> getValueForAxis dp model.selectedY) filteredData

        minX = 0
        maxX = List.maximum xs |> Maybe.withDefault 1
        minY = 0
        maxY = List.maximum ys |> Maybe.withDefault 1

        scaleX x = padding + ((x - minX) / (maxX - minX)) * (width - 2 * padding)
        scaleY y = height - padding - ((y - minY) / (maxY - minY)) * (height - 2 * padding)

        color dp =
            if dp.gender == "Male" then "blue"
            else if dp.gender == "Female" then "red"
            else "gray"

        xTicks = ticksForAxis model.selectedX
        yTicks = ticksForAxis model.selectedY

        tickLineX v =
            Svg.line [ SvgAttr.x1 (String.fromFloat (scaleX v)), SvgAttr.y1 (String.fromFloat (height - padding)), SvgAttr.x2 (String.fromFloat (scaleX v)), SvgAttr.y2 (String.fromFloat (height - padding + 5)), SvgAttr.stroke "black" ] []

        tickLineY v =
            Svg.line [ SvgAttr.x1 (String.fromFloat (padding - 5)), SvgAttr.y1 (String.fromFloat (scaleY v)), SvgAttr.x2 (String.fromFloat padding), SvgAttr.y2 (String.fromFloat (scaleY v)), SvgAttr.stroke "black" ] []

        tickTextX v =
            Svg.text_ [ SvgAttr.x (String.fromFloat (scaleX v)), SvgAttr.y (String.fromFloat (height - padding + 20)), SvgAttr.textAnchor "middle" ] [ Svg.text (String.fromFloat v) ]

        tickTextY v =
            Svg.text_ [ SvgAttr.x (String.fromFloat (padding - 10)), SvgAttr.y (String.fromFloat (scaleY v + 4)), SvgAttr.textAnchor "end" ] [ Svg.text (String.fromFloat v) ]
    in
    Svg.svg [ SvgAttr.width (String.fromFloat width), SvgAttr.height (String.fromFloat height) ]
        (Svg.line [ SvgAttr.x1 (String.fromFloat padding), SvgAttr.y1 (String.fromFloat padding), SvgAttr.x2 (String.fromFloat padding), SvgAttr.y2 (String.fromFloat (height - padding)), SvgAttr.stroke "black" ] []
        :: Svg.line [ SvgAttr.x1 (String.fromFloat padding), SvgAttr.y1 (String.fromFloat (height - padding)), SvgAttr.x2 (String.fromFloat (width - padding)), SvgAttr.y2 (String.fromFloat (height - padding)), SvgAttr.stroke "black" ] []
        :: (List.map tickLineX xTicks ++ List.map tickTextX xTicks)
        ++ (List.map tickLineY yTicks ++ List.map tickTextY yTicks)
        ++ [ -- Achsen Labels
            Svg.text_ [ SvgAttr.x (String.fromFloat (width/2)), SvgAttr.y (String.fromFloat (height - 10)), SvgAttr.textAnchor "middle" ] [ Svg.text model.selectedX ],
            Svg.text_ [ SvgAttr.x "15", SvgAttr.y (String.fromFloat (height/2)), SvgAttr.textAnchor "middle", SvgAttr.transform ("rotate(-90 15," ++ String.fromFloat (height/2) ++ ")") ] [ Svg.text model.selectedY ]
           ]
        ++ List.map
            (\dp ->
                Svg.circle
                    [ SvgAttr.cx (String.fromFloat (scaleX (getValueForAxis dp model.selectedX)))
                    , SvgAttr.cy (String.fromFloat (scaleY (getValueForAxis dp model.selectedY)))
                    , SvgAttr.r "4"
                    , SvgAttr.fill (color dp)
                    ]
                    []
            )
            filteredData
        )


-- MAIN

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
