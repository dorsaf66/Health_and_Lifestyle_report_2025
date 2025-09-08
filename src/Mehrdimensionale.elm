module Mehrdimensionale exposing (main)

import Browser
import Html exposing (Html, div)
import Html as Html
import Http
import String
import Svg exposing (Svg, svg, text_, line, circle, polyline)
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


-- MODEL

type alias Person =
    { id : Int
    , age : Int
    , occupation : String
    , bmi : Int
    , heartRate : Int
    , systolic : Int
    , stressLevel : Int
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
                    String.toFloat sleepDurStr |> Maybe.withDefault 0

                stress =
                    String.toInt stressStr |> Maybe.withDefault 0

                age   =
                    String.toInt ageStr |> Maybe.withDefault 0
            in
            Just
                { id = String.toInt idStr |> Maybe.withDefault -1
                , age = String.toInt ageStr |> Maybe.withDefault 0
                , occupation = occ
                , bmi = bmiNum
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
                [ Html.text (" " ++ err)
                , Html.text "TITLE HIER"
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


-- PARALLEL KOORDINATENPLOT

svgParallel : List Person -> Svg Msg
svgParallel people =
    let
        paddingTop = 50
        paddingLeft = 150

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
            , { name = "Age"
              , getValue = (\p -> toFloat p.age)
              , min = 27
              , max = 60
              , tickLabels = List.map String.fromInt (List.range 27 60)
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
            ]

        -- Basisabstand + Extra-Gaps
        baseSpacing = 120
        extraBetween01 = 70   -- Occupation → Age (kleiner!)
        extraBetween12 = 70   -- Age → Sleep Disorder (kleiner!)
        extraBetween23 = 40   -- Sleep Disorder → Stress (noch kleiner!)

        axisX idx =
            let
                extras =
                    (if idx >= 1 then extraBetween01 else 0)
                        + (if idx >= 2 then extraBetween12 else 0)
                        + (if idx >= 3 then extraBetween23 else 0)
            in
            paddingLeft + idx * baseSpacing + extras

        w = List.length axes * 150 + paddingLeft
        h = 500

        clamp low high v =
            if v < low then low else if v > high then high else v

        scaleY minV maxV val =
            let
                ratio = clamp 0 1 ((val - minV) / (maxV - minV))
            in
            paddingTop + (h - ratio * toFloat h)

        color occ =
            case occ of
                "Doctor" -> "Navy"
                "Software Engineer" -> "Teal"
                "Sales Representative" -> "Salmon"
                "Accountant" -> "Yellow"
                "Nurse" -> "Violet"
                "Lawyer" -> "Brown"
                "Teacher" -> "Tomato"
                "Engineer" -> "Peru"
                "Scientist" -> "Peru"
                "Salesperson" -> "SlateBlue"
                "Manager" -> "Orange"
                _ -> "Gray"

        personLine p =
            let
                points =
                    axes
                        |> List.indexedMap
                            (\i axis ->
                                ( toFloat (axisX i)
                                , scaleY axis.min axis.max (axis.getValue p)
                                )
                            )
            in
            polyline
                [ SA.points
                    (points
                        |> List.map (\(x, y) -> String.fromFloat x ++ "," ++ String.fromFloat y)
                        |> String.join " "
                    )
                , SA.fill "none"
                , SA.stroke (color p.occupation)
                , SA.strokeWidth "1.5"
                , SA.strokeOpacity "0.6"
                ]
                []

        personPoints p =
            axes
                |> List.indexedMap
                    (\i axis ->
                        circle
                            [ SA.cx (String.fromInt (axisX i))
                            , SA.cy (String.fromFloat (scaleY axis.min axis.max (axis.getValue p)))
                            , SA.r "4"
                            , SA.fill (color p.occupation)
                            , SA.stroke "black"
                            , SA.strokeWidth "0.5"
                            ]
                            []
                    )
    in
    svg
        [ SA.width (String.fromInt w)
        , SA.height (String.fromInt (paddingTop + h + 50))
        ]
        ( List.concat
            [ axes
                |> List.indexedMap
                    (\i _ ->
                        line
                            [ SA.x1 (String.fromInt (axisX i))
                            , SA.y1 (String.fromInt paddingTop)
                            , SA.x2 (String.fromInt (axisX i))
                            , SA.y2 (String.fromInt (paddingTop + h))
                            , SA.stroke "black"
                            , SA.strokeWidth "1"
                            ]
                            []
                    )
            , axes
                |> List.indexedMap
                    (\i axis ->
                        axis.tickLabels
                            |> List.indexedMap
                                (\j label ->
                                    let
                                        y =
                                            scaleY axis.min axis.max
                                                (axis.min
                                                    + (axis.max - axis.min)
                                                    * (toFloat j
                                                        / toFloat ((List.length axis.tickLabels) - 1)
                                                      )
                                                )
                                    in
                                    [ line
                                        [ SA.x1 (String.fromInt (axisX i - 5))
                                        , SA.y1 (String.fromFloat y)
                                        , SA.x2 (String.fromInt (axisX i + 5))
                                        , SA.y2 (String.fromFloat y)
                                        , SA.stroke "black"
                                        , SA.strokeWidth "1"
                                        ]
                                        []
                                    , text_
                                        [ SA.x (String.fromInt (axisX i - 10))
                                        , SA.y (String.fromFloat (y + 4))
                                        , SA.fontSize "12"
                                        , SA.textAnchor "end"
                                        ]
                                        [ Html.text label ]
                                    ]
                                )
                            |> List.concat
                    )
                |> List.concat
            , List.map personLine people
            , List.concat (List.map personPoints people)
            , axes
                |> List.indexedMap
                    (\i axis ->
                        text_
                            [ SA.x (String.fromInt (axisX i))
                            , SA.y (String.fromInt (paddingTop + h + 30))
                            , SA.fontSize "14"
                            , SA.textAnchor "middle"
                            ]
                            [ Html.text axis.name ]
                    )
            ]
        )
