module Spinnennetz exposing (main)

import Browser
import Html exposing (Html, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Csv.Decode as Decode exposing (Decoder)
import Svg exposing (..)
import Svg.Attributes as SA
import Svg as S
import List.Extra exposing (transpose)
import Debug
import Basics


-- MODEL

type alias Person =
    { id : Int
    , gender : String
    , age : Int
    , sleepDuration : Float
    , sleepQuality : Int
    , physicalActivity : Int
    , stressLevel : Int
    , dailySteps : Int
    }

type AgeGroup
    = Young
    | Middle
    | Old

type Metric
    = SleepDuration
    | SleepQuality
    | Activity
    | Stress
    | Steps

type alias Model =
    { people : List Person
    , error : Maybe String
    , ageFilter : List AgeGroup
    , selectedMetrics : List Metric
    }


-- INIT

init : () -> ( Model, Cmd Msg )
init _ =
    ( { people = [], error = Nothing
      , ageFilter = []
      , selectedMetrics = [ SleepDuration, SleepQuality, Activity, Stress, Steps ]
      }
    , loadCsv
    )


-- MESSAGES

type Msg
    = CsvLoaded (Result Http.Error String)
    | ToggleAgeGroup AgeGroup
    | ToggleMetric Metric


-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CsvLoaded (Ok raw) ->
            case parseCsv raw of
                Ok persons ->
                    ( { model | people = persons }, Cmd.none )

                Err err ->
                    ( { model | error = Just err }, Cmd.none )

        CsvLoaded (Err httpErr) ->
            ( { model | error = Just (Debug.toString httpErr) }, Cmd.none )

        ToggleAgeGroup ag ->
            let
                newFilter =
                    if List.member ag model.ageFilter then
                        List.filter ((/=) ag) model.ageFilter
                    else
                        ag :: model.ageFilter
            in
            ( { model | ageFilter = newFilter }, Cmd.none )

        ToggleMetric m ->
            let
                newMetrics =
                    if List.member m model.selectedMetrics then
                        List.filter ((/=) m) model.selectedMetrics
                    else
                        m :: model.selectedMetrics
            in
            ( { model | selectedMetrics = newMetrics }, Cmd.none )


-- CSV LOADING

loadCsv : Cmd Msg
loadCsv =
    Http.get
        { url = "data/Sleep_health_and_lifestyle_dataset.csv"
        , expect = Http.expectString CsvLoaded
        }


-- CSV DECODER

personDecoder : Decoder Person
personDecoder =
    Decode.map3
        (\id gender rest ->
            { id = id
            , gender = gender
            , age = rest.age
            , sleepDuration = rest.sleepDuration
            , sleepQuality = rest.sleepQuality
            , physicalActivity = rest.physicalActivity
            , stressLevel = rest.stressLevel
            , dailySteps = rest.dailySteps
            }
        )
        (Decode.field "Person ID" Decode.int)
        (Decode.field "Gender" Decode.string)
        (Decode.map3
            (\age sleepDur sleepQual ->
                (\physAct stress steps ->
                    { age = age
                    , sleepDuration = sleepDur
                    , sleepQuality = sleepQual
                    , physicalActivity = physAct
                    , stressLevel = stress
                    , dailySteps = steps
                    }
                )
            )
            (Decode.field "Age" Decode.int)
            (Decode.field "Sleep Duration" Decode.float)
            (Decode.field "Quality of Sleep" Decode.int)
            |> Decode.andThen (\f -> Decode.map2 f (Decode.field "Physical Activity Level" Decode.int) (Decode.field "Stress Level" Decode.int))
            |> Decode.andThen (\f -> Decode.map f (Decode.field "Daily Steps" Decode.int))
        )


parseCsv : String -> Result String (List Person)
parseCsv raw =
    case Decode.decodeCsv Decode.FieldNamesFromFirstRow personDecoder raw of
        Ok persons -> Ok persons
        Err e -> Err (Debug.toString e)


-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ viewControls model
        , case model.error of
            Just err -> Html.text ("Fehler: " ++ err)
            Nothing ->
                if List.isEmpty model.people then
                    Html.text "Lade Daten ..."
                else
                    radarCompare (filteredPeople model) model.selectedMetrics
        ]


-- CONTROLS

viewControls : Model -> Html Msg
viewControls model =
    div []
        [ div []
            [ Html.text "Altersgruppe: "
            , checkboxAge "Jung <30" Young model.ageFilter
            , checkboxAge "Mittel 30–40" Middle model.ageFilter
            , checkboxAge "Alt >=50" Old model.ageFilter
            ]
        , div []
            [ Html.text "Metriken: "
            , checkboxMetric "Sleep Duration" SleepDuration model.selectedMetrics
            , checkboxMetric "Sleep Quality" SleepQuality model.selectedMetrics
            , checkboxMetric "Activity" Activity model.selectedMetrics
            , checkboxMetric "Stress" Stress model.selectedMetrics
            , checkboxMetric "Steps" Steps model.selectedMetrics
            ]
        ]


checkboxAge : String -> AgeGroup -> List AgeGroup -> Html Msg
checkboxAge labelText value selected =
    let
        checked = List.member value selected
    in
    Html.label []
        [ input [ type_ "checkbox", Html.Attributes.checked checked, onClick (ToggleAgeGroup value) ] []
        , Html.text labelText
        ]


checkboxMetric : String -> Metric -> List Metric -> Html Msg
checkboxMetric labelText value selected =
    let
        checked = List.member value selected
    in
    Html.label []
        [ input [ type_ "checkbox", Html.Attributes.checked checked, onClick (ToggleMetric value) ] []
        , Html.text labelText
        ]


-- FILTERING

filteredPeople : Model -> List Person
filteredPeople model =
    List.filter
        (\p ->
            case model.ageFilter of
                [] -> True
                filters ->
                    List.any
                        (\f ->
                            case f of
                                Young -> p.age < 30
                                Middle -> p.age >= 30 && p.age <= 40
                                Old -> p.age >= 50
                        )
                        filters
        )
        model.people


-- RADAR CHART

normalize : List Float -> List Float
normalize xs =
    let
        minVal = List.minimum xs |> Maybe.withDefault 0
        maxVal = List.maximum xs |> Maybe.withDefault 1
        range = maxVal - minVal
    in
    if range == 0 then
        List.map (\_ -> 0.5) xs
    else
        List.map (\x -> (x - minVal) / range) xs


metricsForPerson : Person -> List Metric -> List Float
metricsForPerson p metrics =
    List.map
        (\m ->
            case m of
                SleepDuration -> p.sleepDuration
                SleepQuality -> toFloat p.sleepQuality
                Activity -> toFloat p.physicalActivity
                Stress -> toFloat p.stressLevel
                Steps -> toFloat p.dailySteps
        )
        metrics


radarCompare : List Person -> List Metric -> Html msg
radarCompare persons selectedMetrics =
    let
        rawValuesLists =
            List.map (\p -> metricsForPerson p selectedMetrics) persons

        columns = transpose rawValuesLists
        normalizedColumns = List.map normalize columns
        normalizedRows = transpose normalizedColumns

        radarDataList =
            List.map2 (\p vals -> { label = "Person " ++ String.fromInt p.id, values = vals }) persons normalizedRows
    in
    S.svg [ SA.width "500", SA.height "500" ]
        (List.concat
            [ [ S.circle [ SA.cx "250", SA.cy "250", SA.r "200", SA.fill "none", SA.stroke "lightgray" ] [] ]
            , List.map (\d -> radarPolygon d.values selectedMetrics) radarDataList
            , axisLines 250 250 200 (List.length selectedMetrics) (List.map metricToString selectedMetrics)
            ]
        )


radarPolygon : List Float -> List Metric -> Svg msg
radarPolygon values metrics =
    let
        n = List.length values
        angles = List.map (\i -> 2 * Basics.pi * toFloat i / toFloat n) (List.range 0 (n-1))
        pointsStr =
            List.map2
                (\angle value ->
                    let
                        r = 200 * value
                        x = 250 + r * cos angle
                        y = 250 - r * sin angle
                    in
                    String.fromFloat x ++ "," ++ String.fromFloat y
                )
                angles
                values
    in
    S.polygon
        [ SA.points (String.join " " pointsStr)
        , SA.fill "none"
        , SA.fillOpacity "0.3"
        , SA.stroke "black"
        ]
        []


axisLines : Float -> Float -> Float -> Int -> List String -> List (Svg msg)
axisLines cx cy r n labels =
    let
        angles = List.map (\i -> 2 * Basics.pi * toFloat i / toFloat n) (List.range 0 (n-1))
        colors = List.map colorForMetric labels
    in
    List.map2
        (\angle labelText ->
            let
                xPos = cx + r * cos angle
                yPos = cy - r * sin angle
            in
            S.g []
                [ S.line [ SA.x1 (String.fromFloat cx), SA.y1 (String.fromFloat cy), SA.x2 (String.fromFloat xPos), SA.y2 (String.fromFloat yPos), SA.stroke (colorForMetric labelText) ] []
                , S.text_ [ SA.x (String.fromFloat (xPos + 5)), SA.y (String.fromFloat yPos), SA.fontSize "12px" ] [ Html.text labelText ]
                ]
        )
        angles
        labels


metricToString : Metric -> String
metricToString m =
    case m of
        SleepDuration -> "Sleep Duration"
        SleepQuality -> "Sleep Quality"
        Activity -> "Activity"
        Stress -> "Stress"
        Steps -> "Steps"


colorForMetric : String -> String
colorForMetric label =
    case label of
        "Sleep Duration" -> "red"
        "Sleep Quality" -> "blue"
        "Activity" -> "green"
        "Stress" -> "orange"
        "Steps" -> "purple"
        _ -> "gray"


-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
