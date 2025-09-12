module Test exposing (..)

--import Color
import Dict
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Statistics
import Tree exposing (Tree)
import TreeLayout
import TypedSvg exposing (circle, g, line, path, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, d, fill, fontFamily, fontSize, stroke, strokeWidth, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))


w : Float
w =
    300


h : Float
h =
    200


padding : Float
padding =
    60


defaultExtent : ( number, number1 )
defaultExtent =
    ( 0, 100 )


treePlot : Float -> List ( String, Maybe String ) -> Svg msg
treePlot minDist tree =
    let
        layout : Dict.Dict String { x : Float, y : Float }
        layout =
            TreeLayout.treeLayout minDist tree

        xValues : List Float
        xValues =
            List.map ( \l -> l.x ) ( Dict.values layout )

        yValues : List Float
        yValues =
            List.map ( \l -> l.y ) ( Dict.values layout )
        
        xyValues : List (Float, Float)
        xyValues = 
            List.map2 Tuple.pair xValues yValues
                |> List.map (\t -> (Tuple.first t + 80, (Tuple.second t * 30)))

        rootCoords : {x : Float, y : Float}
        rootCoords =
            Maybe.withDefault {x=0, y=0} (Dict.get "root" layout)

    in
    svg [ viewBox 10 50 w h, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        [ style []
            [ TypedSvg.Core.text """
            .point circle { stroke: rgba(100, 100, 100,1); fill: rgba(100, 100, 100,1); }
            .point line { stroke: rgba(100, 100, 100,1); fill: rgba(100, 100, 100,1); }
            .point text { display: none; }
            .point:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgba(118, 214, 78,1); }
            .point:hover text { display: inline; }
          """ ]
        , g
            [ transform [ Translate padding padding ] ] <|
            -- Knoten zeichnen
            List.map
                (\t -> 
                    circle
                        [ cx (Tuple.first t)
                        , cy (Tuple.second t)
                        , height 4
                        , r 5
                        , width 4
                        ]
                        []
                )
                xyValues
            -- Kanten zeichnen
            ++ List.map
                (\t -> 
                    line
                        [ x1 ((Maybe.withDefault {x=0, y=0} (Dict.get (Tuple.first t) layout)).x + 80)
                        , y1 ((Maybe.withDefault {x=0, y=0} (Dict.get (Tuple.first t) layout)).y * 30)
                        , x2 ((Maybe.withDefault rootCoords (Dict.get (Maybe.withDefault "" (Tuple.second t)) layout)).x + 80)
                        , y2 ((Maybe.withDefault rootCoords (Dict.get (Maybe.withDefault "" (Tuple.second t)) layout)).y * 30)
                        , stroke (Paint Color.darkGrey)
                        ]
                        []
                ) tree 
            -- Beschriftung
            ++ List.map
                (\t ->
                    text_ 
                        [x ((Maybe.withDefault {x=0, y=0} (Dict.get (Tuple.first t) layout)).x + 76)
                        , y ((Maybe.withDefault {x=0, y=0} (Dict.get (Tuple.first t) layout)).y * 30 - 5)
                        , fontFamily [ "Helvetica", "sans-serif" ]
                        , fontSize (Px 4)
                        ]
                        [ Html.text  (Tuple.first t)
                        ]
                )
                tree
        ] 

main : Html msg
main =
    let
        binaerTree =
            Tree.tree "root"
                [ Tree.tree "E1.1"
                    [ Tree.tree "E2.1" 
                        [ Tree.tree "E3.1" 
                            [ Tree.tree "E4.1" []
                            ]
                        ]
                    , Tree.tree "E2.2" 
                        [ Tree.tree "E3.2" 
                            [ Tree.tree "E4.2" []
                            ]
                        , Tree.tree "E3.3" []
                        ]
                    ]
                , Tree.tree "E1.2" 
                    [ Tree.tree "E2.3" []
                    , Tree.tree "E2.4" 
                        [ Tree.tree "E3.4" []
                        ]
                    ]
                ]
        
        generalTree =
            Tree.tree "root"
                [ Tree.tree "E1.1" 
                    [ Tree.tree "E2.1" []
                    , Tree.tree "E2.2" []
                    , Tree.tree "E2.3" []
                    , Tree.tree "E2.4" []
                    , Tree.tree "E2.5" []
                    , Tree.tree "E2.6" []
                    , Tree.tree "E2.7" 
                        [ Tree.tree "E3.1" []
                        ]
                    ]
                , Tree.tree "E1.2" []
                , Tree.tree "E1.3" []
                , Tree.tree "E1.4" 
                    [ Tree.tree "E2.8" []
                    ]
                , Tree.tree "E1.5" []
                , Tree.tree "E1.6" 
                    [ Tree.tree "E2.9" 
                        [ Tree.tree "E3.2" []
                        , Tree.tree "E3.3" []
                        , Tree.tree "E3.4" []
                        , Tree.tree "E3.5" []
                        , Tree.tree "E3.6" []
                        , Tree.tree "E3.7" []
                        , Tree.tree "E3.8" []
                        ]
                    ]
                ]

        convertedBinaerTree : List ( String, Maybe String )
        convertedBinaerTree =
            binaerTree
                |> Tree.map (\v -> ( v, Nothing ))
                |> convert
                |> Tree.flatten

        convertedGeneralTree : List ( String, Maybe String )
        convertedGeneralTree =
            generalTree
                |> Tree.map (\v -> ( v, Nothing ))
                |> convert
                |> Tree.flatten

        layoutBinaerTree : Dict.Dict String { x : Float, y : Float }
        layoutBinaerTree =
            TreeLayout.treeLayout 40 convertedBinaerTree
            
    in
    div []
        [ treePlot 40 convertedBinaerTree
        , treePlot 18 convertedGeneralTree
        , Html.div [] [ Html.text "Converted binaerTree (Child, Maybe Parent)" ]
        , Html.ul [] <|
            List.map
                (\( child, parent ) ->
                    Html.li []
                        [ Html.text <|
                            "(  "
                                ++ child
                                ++ ", "
                                ++ Maybe.withDefault "Nothing" parent
                                ++ ")"
                        ]
                )
                convertedBinaerTree
        , Html.div [] [ Html.text "Tree Layout" ]
        , Html.ul [] <|
            List.map
                (\( node, { x, y } ) ->
                    Html.li []
                        [ Html.text <|
                            "("
                                ++ node
                                ++ ", x="
                                ++ String.fromFloat x
                                ++ ",y="
                                ++ String.fromFloat y
                                ++ ")"
                        ]
                )
            <| 
                Dict.toList layoutBinaerTree
        ]

convert : Tree ( String, Maybe String ) -> Tree ( String, Maybe String )
convert t =
    let
        ( currentLabel, _ ) =
            Tree.label t
    in
    Tree.mapChildren
        (\listChildren ->
            listChildren
                |> List.map
                    (\c ->
                        convert c
                            |> Tree.mapLabel (\( a, _ ) -> ( a, Just currentLabel ))
                    )
        )