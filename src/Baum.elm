port module Baum exposing (..)

import Browser
import Svg exposing (..)
import Svg.Attributes as SvgAttr
import Svg.Events
import Json.Decode as Decode
import Html exposing (Html, div)
import Html.Attributes as HtmlAttr
import Basics

-- PORTS
port receiveMatrix : (List (List Float) -> msg) -> Sub msg

-- MODEL
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
    { nodes : List Node
    , edges : List Edge
    }

-- INIT
init : () -> ( Model, Cmd Msg )
init _ =
    ( { nodes = [], edges = [] }
    , Cmd.none
    )

-- MSG
type Msg
    = MatrixReceived (List (List Float))
    | DragNode Int Float Float
    | ReleaseNode Int

-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MatrixReceived matrix ->
            let
                (nodes, edges) = buildGraph matrix
            in
            ( { model | nodes = nodes, edges = edges }, Cmd.none )

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

-- GRAPH CREATION (Baum-Anordnung)
buildGraph : List (List Float) -> (List Node, List Edge)
buildGraph matrix =
    let
        keys =
            [ "Gender", "Age", "Sleep Duration", "Quality of Sleep"
            , "Physical Activity Level", "Stress Level", "BMI Category"
            , "Heart Rate", "Daily Steps", "Sleep Disorder"
            ]

        svgWidth = 1200
        levelHeight = 100

        -- Baum-artige Anordnung: Wurzel oben, Kinder darunter
        nodes =
            List.indexedMap
                (\i label ->
                    let
                        level = i // 2
                        xOffset = if Basics.modBy 2 i == 0 then -150 else 150
                        x = svgWidth / 2 + toFloat xOffset
                        y = toFloat level * levelHeight + 50
                    in
                    { id = i
                    , label = label
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

-- HELPER: Label splitten für mehrzeilige Anzeige
splitLabel : String -> List String
splitLabel label =
    String.split " " label

-- VIEW
graphView : Model -> Html Msg
graphView model =
    let
        svgWidth = 1200
        svgHeight = 600
        marginTop = 2

        nodeElements =
            List.concatMap
                (\n ->
                    [ Svg.circle
                        [ SvgAttr.cx (String.fromFloat n.x)
                        , SvgAttr.cy (String.fromFloat (n.y + marginTop))
                        , SvgAttr.r "40"
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
                        , SvgAttr.y (String.fromFloat (n.y + marginTop - 10))
                        , SvgAttr.textAnchor "middle"
                        , SvgAttr.fontSize "12"
                        , SvgAttr.fontWeight "bold"
                        , SvgAttr.fill "white"
                        ]
                        (List.indexedMap (\i word ->
                            Svg.tspan
                                [ SvgAttr.x (String.fromFloat n.x)
                                , SvgAttr.dy (if i == 0 then "0" else "15")
                                ]
                                [ Svg.text word ]
                        ) (splitLabel n.label))
                    ]
                )
                model.nodes

        edgeElements =
            List.map
                (\edge ->
                    let
                        fromNode = List.head <| List.filter (\n -> n.id == edge.from) model.nodes
                        toNode = List.head <| List.filter (\n -> n.id == edge.to) model.nodes
                        strokeWidth = max 0.5 (abs edge.weight * 8)
                    in
                    case (fromNode, toNode) of
                        (Just fn, Just tn) ->
                            Svg.line
                                [ SvgAttr.x1 (String.fromFloat fn.x)
                                , SvgAttr.y1 (String.fromFloat (fn.y + marginTop))
                                , SvgAttr.x2 (String.fromFloat tn.x)
                                , SvgAttr.y2 (String.fromFloat (tn.y + marginTop))
                                , SvgAttr.stroke "#8a4343ff"
                                , SvgAttr.strokeWidth (String.fromFloat strokeWidth)
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
