module Mehrdimensionale exposing (main)

import Browser
import Html exposing (Html, div)
import Html as Html
import Http
import String
import Svg exposing (Svg, polyline, svg, text_, line, circle)
import Svg.Attributes as SA
import List.Extra exposing (unique, elemIndex)


-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, loadCsv )


-- MODEL

type alias Person =
    { id : Int
    , age : Int
    , occupation : String
    , bmiCat : Int
    , heartRate : Int
    , systolic : Int
    , stress : Int
    , sleepDuration : Float
    , sleepDisorder : String
    }


type alias Model =
    { people : List Person
    , error : Maybe String
    }


initModel : Model
initModel =
    { people = [], error = Nothing }


-- UPDATE

type Msg
    = CsvLoaded (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CsvLoaded (Ok raw) ->
            let
                parsed = parseCsv raw
            in
            ( { model
                | people = parsed
                , error = Just ("Parsed rows: " ++ String.fromInt (List.length parsed))
              }
            , Cmd.none
            )

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
    let
        lines =
            raw
                |> String.trim
                |> String.split "\n"
                |> List.map String.trim
    in
    case lines of
        [] -> []
        header :: rows -> rows |> List.filterMap parseRow


parseRow : String -> Maybe Person
parseRow row =
    case String.split "," row |> List.map String.trim of
        idStr :: gender :: ageStr :: occ :: sleepDurStr :: qSleep :: physAct :: stressStr :: bmi :: bp :: hrStr :: steps :: sleepDis :: [] ->
            let
                bmiNum =
                    case String.toLower bmi of
                        "normal" -> 1
                        "overweight" -> 2
                        "obese" -> 3
                        _ -> 0

                ( sys, _ ) = parseBp bp

                sleepDur =
                    case String.toFloat sleepDurStr of
                        Just v -> v
                        Nothing -> 0

                stress =
                    case String.toInt stressStr of
                        Just v -> v
                        Nothing -> 0
            in
            Just
                { id = String.toInt idStr |> Maybe.withDefault -1
                , age = String.toInt ageStr |> Maybe.withDefault 0
                , occupation = occ
                , bmiCat = bmiNum
                , heartRate = String.toInt hrStr |> Maybe.withDefault 0
                , systolic = sys
                , stress = stress
                , sleepDuration = sleepDur
                , sleepDisorder = sleepDis
                }

        _ -> Nothing


parseBp : String -> ( Int, Int )
parseBp s =
    case String.split "/" (String.trim s) of
        a :: b :: _ ->
            ( String.toInt a |> Maybe.withDefault 0
            , String.toInt b |> Maybe.withDefault 0
            )
        _ -> (0, 0)


-- VIEW

view : Model -> Html Msg
view model =
    case model.error of
        Just err ->
            div []
                [ Html.text ("Status: " ++ err)
                , svgParallel model.people
                ]

        Nothing ->
            div []
                [ Html.text "Loading..." ]


-- ACHSEN RECORD

type alias Axis =
    { name : String
    , getValue : Person -> Float
    , min : Float
    , max : Float
    , tickLabels : List String
    }


-- PARALLEL KOORDINATEN PLOT

svgParallel : List Person -> Svg Msg
svgParallel people =
    let
        -- Kategorien auf Zahlen mappen
        occupations = people |> List.map .occupation |> unique
        sleepDisorders = people |> List.map .sleepDisorder |> unique

        axisIndex str list =
            list |> elemIndex str |> Maybe.withDefault 0 |> toFloat

        axes : List Axis
        axes =
            [ { name = "Occupation"
              , getValue = (\p -> axisIndex p.occupation occupations)
              , min = 0
              , max = toFloat (List.length occupations - 1)
              , tickLabels = occupations
              }
            , { name = "Sleep Disorder"
              , getValue = (\p -> axisIndex p.sleepDisorder sleepDisorders)
              , min = 0
              , max = toFloat (List.length sleepDisorders - 1)
              , tickLabels = sleepDisorders
              }
            , { name = "Stress"
              , getValue = (\p -> toFloat p.stress)
              , min = 0
              , max = 10
              , tickLabels = List.map String.fromInt (List.range 0 10)
              }
            , { name = "Blood Pressure"
              , getValue = (\p -> toFloat p.systolic)
              , min = 90
              , max = 160
              , tickLabels = List.map String.fromInt [90,100,110,120,130,140,150,160]
              }
            ]

        w = List.length axes * 150
        h = 500

        scaleY minV maxV val =
            h - ((val - minV) / (maxV - minV) * toFloat h)

        axisX idx =
            50 + idx * 150

        color occ =
            case occ of
                "Doctor" -> "blue"
                "Software Engineer" -> "green"
                "Sales Representative" -> "red"
                _ -> "gray"

        -- Linien
        personLine p =
            let
                points =
                    axes
                        |> List.indexedMap
                            (\i axis ->
                                let val = axis.getValue p in
                                ( axisX i, scaleY axis.min axis.max val )
                            )
            in
            polyline
                [ SA.points (points |> List.map (\(x,y) -> String.fromInt x ++ "," ++ String.fromFloat y) |> String.join " ")
                , SA.fill "none"
                , SA.stroke (color p.occupation)
                , SA.strokeWidth "1.5"
                , SA.strokeOpacity "0.6"
                ]
                []

        -- Punkte auf Achsen
        personPoints p =
            axes
                |> List.indexedMap
                    (\i axis ->
                        let val = axis.getValue p in
                        circle
                            [ SA.cx (String.fromInt (axisX i))
                            , SA.cy (String.fromFloat (scaleY axis.min axis.max val))
                            , SA.r "4"
                            , SA.fill (color p.occupation)
                            , SA.stroke "black"
                            , SA.strokeWidth "0.5"
                            ]
                            []
                    )

        -- Achsenlinien
        axisLines =
            axes
                |> List.indexedMap
                    (\i _ ->
                        line
                            [ SA.x1 (String.fromInt (axisX i))
                            , SA.y1 "0"
                            , SA.x2 (String.fromInt (axisX i))
                            , SA.y2 (String.fromInt h)
                            , SA.stroke "black"
                            , SA.strokeWidth "1"
                            ]
                            []
                    )

        -- Achsenticks und Labels
        axisTicks =
            axes
                |> List.indexedMap
                    (\i axis ->
                        axis.tickLabels
                            |> List.indexedMap
                                (\j label ->
                                    let
                                        val = axis.min + (toFloat j / toFloat (List.length axis.tickLabels - 1)) * (axis.max - axis.min)
                                        y = scaleY axis.min axis.max val
                                        tickLine =
                                            line
                                                [ SA.x1 (String.fromInt (axisX i - 5))
                                                , SA.y1 (String.fromFloat y)
                                                , SA.x2 (String.fromInt (axisX i + 5))
                                                , SA.y2 (String.fromFloat y)
                                                , SA.stroke "black"
                                                , SA.strokeWidth "1"
                                                ]
                                                []
                                        tickLabel =
                                            text_
                                                [ SA.x (String.fromInt (axisX i - 10))
                                                , SA.y (String.fromFloat (y + 4))
                                                , SA.fontSize "12"
                                                , SA.textAnchor "end"
                                                ]
                                                [ Html.text label ]
                                    in
                                    [ tickLine, tickLabel ]
                                )
                            |> List.concat
                    )
                |> List.concat

        -- Achsenbeschriftung unten
        axisLabels =
            axes
                |> List.indexedMap
                    (\i axis ->
                        text_
                            [ SA.x (String.fromInt (axisX i))
                            , SA.y (String.fromInt (h + 30))
                            , SA.fontSize "14"
                            , SA.textAnchor "middle"
                            ]
                            [ Html.text axis.name ]
                    )
    in
    svg
        [ SA.width (String.fromInt w)
        , SA.height (String.fromInt (h + 50))
        ]
        (axisLines ++ axisTicks ++ List.concatMap personPoints people ++ List.map personLine people ++ axisLabels)
