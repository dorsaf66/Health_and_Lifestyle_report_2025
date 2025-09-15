module Baum exposing (main)

import Browser
import Html exposing (Html, div)
import Http
import String
import Svg exposing (Svg, svg, circle, line, text_, g, defs, marker, path)
import Svg.Attributes as SA


-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


-- MODEL

type alias Person =
    { id : Int
    , gender : String
    , age : Int
    , occupation : String
    , sleepDuration : Float
    , qualitySleep : Int
    , physicalActivity : Int
    , stressLevel : Int
    , bmiCategory : String
    , systolic : Int
    , diastolic : Int
    , heartRate : Int
    , dailySteps : Int
    , sleepDisorder : String
    }


type alias Model =
    { people : List Person
    , error : Maybe String
    }


initModel : Model
initModel =
    { people = []
    , error = Nothing }


-- INIT

init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, loadCsv )


-- UPDATE

type Msg
    = CsvLoaded (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CsvLoaded (Ok raw) ->
            ( { model | people = parseCsv raw, error = Just "CSV geladen" }, Cmd.none )

        CsvLoaded (Err e) ->
            ( { model | error = Just ("HTTP Error: " ++ Debug.toString e) }, Cmd.none )


-- HTTP

loadCsv : Cmd Msg
loadCsv =
    Http.get
        { url = "data/Sleep_health_and_lifestyle_dataset.csv"
        , expect = Http.expectString CsvLoaded
        }


-- CSV PARSER

parseCsv : String -> List Person
parseCsv raw =
    raw
        |> String.trim
        |> String.split "\n"
        |> List.drop 1
        |> List.filterMap parseRow


parseRow : String -> Maybe Person
parseRow row =
    case String.split "," row |> List.map String.trim of
        idStr :: gender :: ageStr :: occ :: sleepDurStr :: qSleepStr :: physActStr :: stressStr :: bmi :: bp :: hrStr :: stepsStr :: sleepDis :: [] ->
            let
                ( sys, dia ) = parseBp bp
            in
            Just
                { id = String.toInt idStr |> Maybe.withDefault -1
                , gender = gender
                , age = String.toInt ageStr |> Maybe.withDefault 0
                , occupation = occ
                , sleepDuration = String.toFloat sleepDurStr |> Maybe.withDefault 0
                , qualitySleep = String.toInt qSleepStr |> Maybe.withDefault 0
                , physicalActivity = String.toInt physActStr |> Maybe.withDefault 0
                , stressLevel = String.toInt stressStr |> Maybe.withDefault 0
                , bmiCategory = bmi
                , systolic = sys
                , diastolic = dia
                , heartRate = String.toInt hrStr |> Maybe.withDefault 0
                , dailySteps = String.toInt stepsStr |> Maybe.withDefault 0
                , sleepDisorder = sleepDis
                }

        _ ->
            Nothing


parseBp : String -> ( Int, Int )
parseBp s =
    case String.split "/" (String.trim s) of
        a :: b :: _ ->
            ( String.toInt a |> Maybe.withDefault 0
            , String.toInt b |> Maybe.withDefault 0
            )

        _ ->
            ( 0, 0 )


-- STATISTICS

type alias Metrics =
    { sleepPct : Float
    , qualityPct : Float
    , physPct : Float
    , stepsPct : Float
    , sleepDisorderPct : Float
    }


computeMetricsForGender : String -> List Person -> Metrics
computeMetricsForGender gender people =
    let
        group = List.filter (\p -> String.toLower p.gender == String.toLower gender) people
        count = max 1 (List.length group)

        avgInt f =
            group |> List.map f |> List.foldl (+) 0 |> (\s -> toFloat s / toFloat count)

        avgFloat f =
            group |> List.map f |> List.foldl (+) 0.0 |> (\s -> s / toFloat count)

        avgSleep = avgFloat .sleepDuration
        avgQuality = avgInt .qualitySleep
        avgPhys = avgInt .physicalActivity
        avgSteps = avgInt .dailySteps

        countDisordered =
            group
                |> List.filter (\p -> String.toLower p.sleepDisorder /= "none" && p.sleepDisorder /= "")
                |> List.length

        sleepDisorderPct = (toFloat countDisordered / toFloat count) * 100
    in
    { sleepPct = clamp 0 100 (avgSleep / 10 * 100)
    , qualityPct = clamp 0 100 (avgQuality / 10 * 100)
    , physPct = clamp 0 100 avgPhys
    , stepsPct = clamp 0 100 (avgSteps / 20000 * 100)
    , sleepDisorderPct = sleepDisorderPct
    }


clamp : Float -> Float -> Float -> Float
clamp lo hi v =
    if v < lo then lo else if v > hi then hi else v


-- VIEW

type alias MetricNode =
    { x : Float, y : Float, title : String, pct : Float }


view : Model -> Html Msg
view model =
    case model.error of
        Nothing -> div [] [ Html.text "Loading..." ]
        Just _ -> svgTree model.people


svgTree : List Person -> Html Msg
svgTree people =
    let
        width = 1200
        height = 700

        rootX = toFloat (width // 2)
        rootY = 60
        leftX = 300
        rightX = 900
        genderY = 200
        metricY = 420
        metricSpacing = 90
        nodeRadius = 36

        metrics =
            [ ("Sleep<br>Duration", .sleepPct)
            , ("Quality<br>of Sleep", .qualityPct)
            , ("Physical<br>Activity", .physPct)
            , ("Daily<br>Steps", .stepsPct)
            , ("Sleep<br>Disorder", .sleepDisorderPct)
            ]

        leftMetrics = computeMetricsForGender "Male" people
        rightMetrics = computeMetricsForGender "Female" people

        totalPeople = List.length people
        maleCount = List.length (List.filter (\p -> String.toLower p.gender == "male") people)
        femaleCount = List.length (List.filter (\p -> String.toLower p.gender == "female") people)

        malePct = (toFloat maleCount / toFloat totalPeople) * 100
        femalePct = (toFloat femaleCount / toFloat totalPeople) * 100

        metricNodes side baseX =
            List.indexedMap
                (\i (title, getter) ->
                    { x = toFloat baseX + toFloat i * toFloat metricSpacing - (toFloat (List.length metrics - 1) * toFloat metricSpacing / 2)
                    , y = toFloat metricY
                    , title = title
                    , pct = getter (if side == "left" then leftMetrics else rightMetrics)
                    }
                )
                metrics

        leftMetricNodes = metricNodes "left" leftX
        rightMetricNodes = metricNodes "right" rightX

        round1 v = ((v * 10 |> round |> toFloat) / 10)

        drawNode n =
            g []
                [ circle
                    [ SA.cx (String.fromFloat n.x)
                    , SA.cy (String.fromFloat n.y)
                    , SA.r (String.fromInt nodeRadius)
                    , SA.fill "DeepSkyBlue"
                    ] []
                , text_
                    [ SA.x (String.fromFloat n.x)
                    , SA.y (String.fromFloat (n.y - 8))
                    , SA.textAnchor "middle"
                    , SA.fontSize "10"
                    , SA.fill "white"
                    ]
                    [ Html.text (String.join "\n" (String.split "<br>" n.title)) ]
                , text_
                    [ SA.x (String.fromFloat n.x)
                    , SA.y (String.fromFloat (n.y + 18))
                    , SA.textAnchor "middle"
                    , SA.fontSize "10"
                    , SA.fill "DarkRed"
                    , SA.fontWeight "bold"
                    ]
                    [ Html.text (String.fromFloat (round1 n.pct) ++ "%") ]
                ]

        lineAttrs x1 y1 x2 y2 =
            [ SA.x1 (String.fromFloat x1)
            , SA.y1 (String.fromFloat y1)
            , SA.x2 (String.fromFloat x2)
            , SA.y2 (String.fromFloat y2)
            , SA.stroke "DarkRed"
            , SA.strokeWidth "2"
            , SA.markerEnd "url(#arrow)"
            ]

        rootNode = drawNode { x = rootX, y = rootY, title = "People", pct = 100 }

        maleNode =
            drawNode
                { x = toFloat leftX
                , y = toFloat genderY
                , title = "Male"
                , pct = round1 malePct
                }

        femaleNode =
            drawNode
                { x = toFloat rightX
                , y = toFloat genderY
                , title = "Female"
                , pct = round1 femalePct
                }

        lines =
            [ line (lineAttrs rootX rootY (toFloat leftX) (toFloat genderY)) []
            , line (lineAttrs rootX rootY (toFloat rightX) (toFloat genderY)) []
            ]
                ++ List.map (\n -> line (lineAttrs (toFloat leftX) (toFloat (genderY + nodeRadius)) n.x (n.y - toFloat nodeRadius)) []) leftMetricNodes
                ++ List.map (\n -> line (lineAttrs (toFloat rightX) (toFloat (genderY + nodeRadius)) n.x (n.y - toFloat nodeRadius)) []) rightMetricNodes

        allNodes = [rootNode, maleNode, femaleNode] ++ List.map drawNode leftMetricNodes ++ List.map drawNode rightMetricNodes

        arrowDef =
            defs []
                [ marker
                    [ SA.id "arrow"
                    , SA.markerWidth "10"
                    , SA.markerHeight "10"
                    , SA.refX "0"
                    , SA.refY "3"
                    , SA.orient "auto"
                    , SA.markerUnits "strokeWidth"
                    ]
                    [ path [ SA.d "M0,0 L0,6 L9,3 z", SA.fill "DarkRed" ] [] ]
                ]
    in
    svg
        [ SA.width (String.fromInt width)
        , SA.height (String.fromInt height)
        ]
        (arrowDef :: (lines ++ allNodes))
