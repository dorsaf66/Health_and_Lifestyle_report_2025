module Uebung exposing (..)


import Browser
import Browser.Events
import Color
import Force
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Html
import Html.Events exposing (on)
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as Decode
import Time
import TypedSvg exposing (circle, g, line, rect, svg, title)
import TypedSvg.Attributes exposing (alignmentBaseline, class, fill, markerEnd, stroke, textAnchor, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, fontSize, height, r, strokeWidth, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Attribute, Svg, text)
import TypedSvg.Types exposing (AlignmentBaseline (..), AnchorAlignment (..), Paint(..))
import TypedSvg exposing (text_)


w : Float
w =
    700


h : Float
h =
    350


type Msg
    = DragStart NodeId ( Float, Float )
    | DragAt ( Float, Float )
    | DragEnd ( Float, Float )
    | Tick Time.Posix


type alias Model =
    { drag : Maybe Drag
    , graph : Graph Entity ()
    , simulation : Force.State NodeId
    }


type alias Drag =
    { start : ( Float, Float )
    , current : ( Float, Float )
    , index : NodeId
    }


type alias Entity =
    Force.Entity NodeId { value : String }


initializeNode : NodeContext String () -> NodeContext Entity ()
initializeNode ctx =
    { node = { label = Force.entity ctx.node.id ctx.node.label, id = ctx.node.id }
    , incoming = ctx.incoming
    , outgoing = ctx.outgoing
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        graph =
            Graph.mapContexts initializeNode philosophenGraph

        link { from, to } =
            ( from, to )

        forces =
            [ Force.links <| List.map link <| Graph.edges graph
            , Force.manyBodyStrength -50 <| List.map .id <| Graph.nodes graph       -- , Force.manyBody <| List.map .id <| Graph.nodes graph
            , Force.center (w / 2) (h / 2)
            ]
    in
    ( Model Nothing graph (Force.simulation forces), Cmd.none )


updateNode : ( Float, Float ) -> NodeContext Entity () -> NodeContext Entity ()
updateNode ( x, y ) nodeCtx =
    let
        nodeValue =
            nodeCtx.node.label
    in
    updateContextWithValue nodeCtx { nodeValue | x = x, y = y }


updateContextWithValue : NodeContext Entity () -> Entity -> NodeContext Entity ()
updateContextWithValue nodeCtx value =
    let
        node =
            nodeCtx.node
    in
    { nodeCtx | node = { node | label = value } }


updateGraphWithList : Graph Entity () -> List Entity -> Graph Entity ()
updateGraphWithList =
    let
        graphUpdater value =
            Maybe.map (\ctx -> updateContextWithValue ctx value)
    in
    List.foldr (\node graph -> Graph.update node.id (graphUpdater node) graph)


update : Msg -> Model -> Model
update msg ({ drag, graph, simulation } as model) =
    case msg of
        Tick t ->
            let
                ( newState, list ) =
                    Force.tick simulation <| List.map .label <| Graph.nodes graph
            in
            case drag of
                Nothing ->
                    Model drag (updateGraphWithList graph list) newState

                Just { current, index } ->
                    Model drag
                        (Graph.update index
                            (Maybe.map (updateNode current))
                            (updateGraphWithList graph list)
                        )
                        newState

        DragStart index xy ->
            Model (Just (Drag xy xy index)) graph simulation

        DragAt xy ->
            case drag of
                Just { start, index } ->
                    Model (Just (Drag start xy index))
                        (Graph.update index (Maybe.map (updateNode xy)) graph)
                        (Force.reheat simulation)

                Nothing ->
                    Model Nothing graph simulation

        DragEnd xy ->
            case drag of
                Just { start, index } ->
                    Model Nothing
                        (Graph.update index (Maybe.map (updateNode xy)) graph)
                        simulation

                Nothing ->
                    Model Nothing graph simulation


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            -- This allows us to save resources, as if the simulation is done, there is no point in subscribing
            -- to the rAF.
            if Force.isCompleted model.simulation then
                Sub.none

            else
                Browser.Events.onAnimationFrame Tick

        Just _ ->
            Sub.batch
                [ Browser.Events.onMouseMove (Decode.map (.clientPos >> DragAt) Mouse.eventDecoder)
                , Browser.Events.onMouseUp (Decode.map (.clientPos >> DragEnd) Mouse.eventDecoder)
                , Browser.Events.onAnimationFrame Tick
                ]


onMouseDown : NodeId -> Attribute Msg
onMouseDown index =
    Mouse.onDown (.clientPos >> DragStart index)


linkElement : Graph Entity () -> Edge () -> Svg msg
linkElement graph edge =
    let
        source =
            Maybe.withDefault (Force.entity 0 "") <| Maybe.map (.node >> .label) <| Graph.get edge.from graph

        target =
            Maybe.withDefault (Force.entity 0 "") <| Maybe.map (.node >> .label) <| Graph.get edge.to graph
    in
    line
        [ strokeWidth 1
        , stroke <| Paint <| Color.rgb255 170 170 170
        , x1 source.x
        , y1 source.y
        , x2 target.x
        , y2 target.y
        , markerEnd "url(#arrowhead)"
        ]
        []


nodeElement : { a | id : NodeId, label : { b | x : Float, y : Float, value : String } } -> Svg Msg
nodeElement node =
    if node.id < 12 then
        circle
            [ r 10
            , fill <| Paint Color.green
            , stroke <| Paint <| Color.black
            , strokeWidth 0.5
            , onMouseDown node.id
            , cx node.label.x
            , cy node.label.y
            ]
            [ ]
    else
        rect
            [ x ( node.label.x - 14 )
            , y ( node.label.y - 7 )
            , width 28
            , height 14
            , onMouseDown node.id
            , fill <| Paint Color.brown
            , stroke <| Paint Color.black
            , strokeWidth 0.5
            ]
            [  ]


nodeName : { a | id : NodeId, label : { b | x : Float, y : Float, value : String } } -> Svg Msg
nodeName node =
    text_
        [ x node.label.x
        , y node.label.y
        , alignmentBaseline AlignmentCentral
        , textAnchor AnchorMiddle
        , fontSize 10
        , onMouseDown node.id
        ]
        [ text node.label.value ]

view : Model -> Svg Msg
view model =
    svg [ viewBox 0 0 w h ]
        ([ Graph.edges model.graph
            |> List.map (linkElement model.graph)
            |> g [ class [ "links" ] ] 
        , Graph.nodes model.graph
            |> List.map nodeElement
            |> g [ class [ "nodes" ] ]
        , Graph.nodes model.graph
            |> List.map nodeName
            |> g [ class [ "identifier"] ] 
        ]
        ++ [TypedSvg.defs [] [ TypedSvg.marker
                [ TypedSvg.Attributes.id "arrowhead"
                , TypedSvg.Attributes.markerWidth <| TypedSvg.Types.Px 12
                , TypedSvg.Attributes.markerHeight <| TypedSvg.Types.Px 8
                , TypedSvg.Attributes.refX "20"      
                , TypedSvg.Attributes.refY "3"      
                , TypedSvg.Attributes.orient "auto"
                ]
                [ TypedSvg.polygon [ TypedSvg.Attributes.points [(0, 0), (9, 3), (0, 6) ] ] []   -- (0, 0), (10, 3.5), (0, 7)
                ]
            ]
        ])


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        }

philosophenGraph : Graph String ()
philosophenGraph =
    Graph.fromNodeLabelsAndEdgePairs
        [ "ea1" -- 0
        , "ri1" -- 1
        , "th1" -- 2
        , "hu1" -- 3
        , "ea2" -- 4
        , "ri2" -- 5
        , "th2" -- 6
        , "hu2" -- 7
        , "ea3" -- 8
        , "ri3" -- 9
        , "th3" -- 10
        , "hu3" -- 11
        , "acq1" -- 12
        , "rel1" -- 13
        , "bec1" -- 14
        , "acq3" -- 15
        , "acq2" -- 16
        , "rel2" -- 17
        , "bec2" -- 18
        , "rel3" -- 19
        , "bec3" -- 20 
        ]
        [ ( 0, 13 )
        , ( 1, 12)
        , ( 1, 15 )
        , ( 2, 14 )
        , ( 3, 12 )
        , ( 4, 17 )
        , ( 5, 12 )
        , ( 5, 16 )
        , ( 6, 18 )
        , ( 7, 16 )
        , ( 8, 19 )
        , ( 9, 16 )
        , ( 9, 15 )
        , ( 10, 20 )
        , ( 11, 15 )
        , ( 12, 0 )
        , ( 13, 1 )
        , ( 13, 2 )
        , ( 14, 3 )
        , ( 15, 8 )
        , ( 16, 4 )
        , ( 17, 5 )
        , ( 17, 6 )
        , ( 18, 7 )
        , ( 19, 9 )
        , ( 19, 10 )
        ,  (20, 11 )
        ]