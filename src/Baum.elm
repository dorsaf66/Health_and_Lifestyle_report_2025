port module Baum exposing (..)

import Browser
import Csv.Parser exposing (parse)
import Html exposing (Html, div)
import Html.Attributes as HtmlAttr
import Svg exposing (..)
import Svg.Attributes as SvgAttr
import Svg.Events
import Json.Decode as Decode
import Http
import String
import Basics

-- PORTS
port receiveMatrix : (List (List Float) -> msg) -> Sub msg

-- MODEL
type alias Person =
    { id : Int
    , gender : String
    , age : Int
    , occupation : String
    , sleepDuration : Float
    , sleepQuality : Int
    , physicalActivityLevel : Int
    , stressLevel : Int
    , bmi : String
    , bloodPressure : String
    , heartRate : Int
    , dailySteps : Int
    , sleepDisorder : String
    }

type alias Node =
    { id : Int
    , label : String
    , x : Float
    , y : Float
    , fx : Maybe Float
    , fy : Maybe Float
    }

type alias Edge =
    { from : Int
    , to : Int
    , weight : Float
    }

type alias Model =
    { data : List Person
    , correlationMatrix : List (List Float)
    , nodes : List Node
    , edges : List Edge
    }

-- CSV PARSING
rowToPerson : List String -> Maybe Person
rowToPerson row =
    case row of
        idStr :: genderStr :: ageStr :: occupationStr :: sleepDurationStr :: sleepQualityStr :: physicalActivityLevelStr :: stressLevelStr :: bmiStr :: bloodPressureStr :: heartRateStr :: dailyStepsStr :: sleepDisorderStr :: [] ->
            Just
                { id = String.toInt idStr |> Maybe.withDefault 0
                , gender = genderStr
                , age = String.toInt ageStr |> Maybe.withDefault 0
                , occupation = occupationStr
                , sleepDuration = String.toFloat sleepDurationStr |> Maybe.withDefault 0
                , sleepQuality = String.toInt sleepQualityStr |> Maybe.withDefault 0
                , physicalActivityLevel = String.toInt physicalActivityLevelStr |> Maybe.withDefault 0
                , stressLevel = String.toInt stressLevelStr |> Maybe.withDefault 0
                , bmi = bmiStr
                , bloodPressure = bloodPressureStr
                , heartRate = String.toInt heartRateStr |> Maybe.withDefault 0
                , dailySteps = String.toInt dailyStepsStr |> Maybe.withDefault 0
                , sleepDisorder = sleepDisorderStr
                }

        _ ->
            Nothing

-- INIT
init : () -> ( Model, Cmd Msg )
init _ =
    ( { data = []
      , correlationMatrix = []
      , nodes = []
      , edges = []
      }
    , loadCsv
    )

loadCsv : Cmd Msg
loadCsv =
    Http.get
        { url = "data/Sleep_health_and_lifestyle_dataset.csv"
        , expect = Http.expectString CsvLoaded
        }

-- UPDATE
type Msg
    = CsvLoaded (Result Http.Error String)
    | MatrixReceived (List (List Float))
    | DragNode Int Float Float
    | ReleaseNode Int

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        MatrixReceived matrix ->
            let
                (nodes, edges) = buildGraph matrix
            in
            ( { model | correlationMatrix = matrix, nodes = nodes, edges = edges }, Cmd.none )

        DragNode nodeId x y ->
            let
                nodesUpdated =
                    List.map (\n -> if n.id == nodeId then { n | fx = Just x, fy = Just y, x = x, y = y } else n) model.nodes
            in
            ( { model | nodes = nodesUpdated }, Cmd.none )

        ReleaseNode nodeId ->
            let
                nodesUpdated =
                    List.map (\n -> if n.id == nodeId then { n | fx = Nothing, fy = Nothing } else n) model.nodes
            in
            ( { model | nodes = nodesUpdated }, Cmd.none )

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveMatrix MatrixReceived

-- GRAPH CREATION
buildGraph : List (List Float) -> (List Node, List Edge)
buildGraph matrix =
    let
        keys =
            [ "Gender", "Age", "Sleep Duration", "Quality of Sleep"
            , "Physical Activity Level", "Stress Level", "BMI Category"
            , "Heart Rate", "Daily Steps", "Sleep Disorder"
            ]

        svgWidth = 1200
        svgHeight = 600
        n = List.length keys
        spacing = 150
        rowHeight = 150

        labelOverride label =
            case label of
                "Physical Activity Level" -> "Good"
                "Quality of Sleep" -> "Poor"
                "Stress Level" -> "High"
                _ -> label

        nodes =
            List.indexedMap
                (\i label ->
                    let
                        x = svgWidth / 2 + (toFloat ((Basics.modBy 5 i) - 2) * spacing)
                        y = svgHeight / 2 + (toFloat (i // 5) * rowHeight)
                    in
                    { id = i
                    , label = labelOverride label
                    , x = x
                    , y = y
                    , fx = Nothing
                    , fy = Nothing
                    }
                )
                keys

        edges =
            List.concatMap
                (\(row, i) ->
                    List.concatMap
                        (\(value, j) ->
                            if i /= j then
                                [ { from = i, to = j, weight = value } ]
                            else
                                []
                        )
                        (List.indexedMap (\j value -> (value, j)) row)
                )
                (List.indexedMap (\i row -> (row, i)) matrix)
    in
    (nodes, edges)

-- VIEW
graphView : Model -> Html Msg
graphView model =
    let
        svgWidth = 1200
        svgHeight = 600

        nodeElements =
            List.concatMap
                (\n ->
                    [ Svg.circle
                        [ SvgAttr.cx (String.fromFloat n.x)
                        , SvgAttr.cy (String.fromFloat n.y)
                        , SvgAttr.r "35"
                        , SvgAttr.fill "#69b3a2"
                        , Svg.Events.on "mousedown"
                            (Decode.map2 (\x y -> DragNode n.id x y)
                                (Decode.field "clientX" Decode.float)
                                (Decode.field "clientY" Decode.float)
                            )
                        , Svg.Events.on "mouseup" (Decode.succeed (ReleaseNode n.id))
                        ]
                        []
                    , Svg.text_
                        [ SvgAttr.x (String.fromFloat n.x)
                        , SvgAttr.y (String.fromFloat (n.y + 5))
                        , SvgAttr.textAnchor "middle"
                        , SvgAttr.fontSize "14"
                        , SvgAttr.fontWeight "bold"
                        , SvgAttr.fill "white"
                        ]
                        [ Svg.text n.label ]
                    ]
                )
                model.nodes

        edgeElements =
            List.map
                (\edge ->
                    let
                        fromNode = List.head <| List.filter (\n -> n.id == edge.from) model.nodes
                        toNode = List.head <| List.filter (\n -> n.id == edge.to) model.nodes
                    in
                    case (fromNode, toNode) of
                        (Just fn, Just tn) ->
                            Svg.line
                                [ SvgAttr.x1 (String.fromFloat fn.x)
                                , SvgAttr.y1 (String.fromFloat fn.y)
                                , SvgAttr.x2 (String.fromFloat tn.x)
                                , SvgAttr.y2 (String.fromFloat tn.y)
                                , SvgAttr.stroke "#8a4343ff"
                                , SvgAttr.strokeWidth (String.fromFloat (abs edge.weight * 8))
                                ]
                                []

                        _ -> Svg.line [] []
                )
                model.edges
    in
    div
        [ HtmlAttr.style "margin" "0 auto"
        , HtmlAttr.style "width" "1200px"
        , HtmlAttr.style "text-align" "center"
        ]
        [ Svg.svg
            [ SvgAttr.width (String.fromFloat svgWidth)
            , SvgAttr.height (String.fromFloat svgHeight)
            ]
            (edgeElements ++ nodeElements)
        ]

-- MAIN
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = graphView
        , subscriptions = subscriptions
        }