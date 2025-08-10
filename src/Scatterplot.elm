module Scatterplot exposing (main)

import Browser
import Html exposing (Html, button, div, option, select, text)
import Html.Attributes as HtmlAttr
import Html.Events as HtmlEvents
import Svg exposing (..)
import Svg.Attributes as SvgAttr
import String


-- MODEL

type alias DataPoint =
    { schritte : Float
    , wasserzufuhr : Float
    , schlafdauer : Float
    , kalorienverbrauch : Float
    , gesundheitszustand : Float
    , healthScore : Float
    , stressLevel : Int
    , altersgruppe : String
    , geschlecht : String
    }


type alias Model =
    { data : List DataPoint
    , selectedX : String
    , selectedY : String
    , showPlot : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { data = sampleData
      , selectedX = "Schritte"
      , selectedY = "Kalorienverbrauch"
      , showPlot = True
      }
    , Cmd.none
    )


sampleData : List DataPoint
sampleData =
    [ { schritte = 5000, wasserzufuhr = 2, schlafdauer = 7, kalorienverbrauch = 2200, gesundheitszustand = 3, healthScore = 75, stressLevel = 2, altersgruppe = "30-40", geschlecht = "m" }
    , { schritte = 8000, wasserzufuhr = 1.5, schlafdauer = 6, kalorienverbrauch = 2500, gesundheitszustand = 4, healthScore = 80, stressLevel = 3, altersgruppe = "20-30", geschlecht = "w" }
    , { schritte = 4000, wasserzufuhr = 2.5, schlafdauer = 8, kalorienverbrauch = 2000, gesundheitszustand = 2, healthScore = 60, stressLevel = 1, altersgruppe = "40-50", geschlecht = "m" }
    , { schritte = 7000, wasserzufuhr = 2.2, schlafdauer = 7, kalorienverbrauch = 2300, gesundheitszustand = 4, healthScore = 78, stressLevel = 2, altersgruppe = "30-40", geschlecht = "w" }
    , { schritte = 6000, wasserzufuhr = 1.8, schlafdauer = 6.5, kalorienverbrauch = 2100, gesundheitszustand = 3, healthScore = 70, stressLevel = 2, altersgruppe = "40-50", geschlecht = "m" }
    , { schritte = 9000, wasserzufuhr = 1.4, schlafdauer = 5.5, kalorienverbrauch = 2600, gesundheitszustand = 5, healthScore = 85, stressLevel = 3, altersgruppe = "20-30", geschlecht = "w" }
    , { schritte = 3500, wasserzufuhr = 2.8, schlafdauer = 8, kalorienverbrauch = 1900, gesundheitszustand = 2, healthScore = 58, stressLevel = 1, altersgruppe = "50-60", geschlecht = "m" }
    , { schritte = 4500, wasserzufuhr = 2, schlafdauer = 7.5, kalorienverbrauch = 2050, gesundheitszustand = 3, healthScore = 65, stressLevel = 2, altersgruppe = "30-40", geschlecht = "w" }
    , { schritte = 8500, wasserzufuhr = 1.6, schlafdauer = 6, kalorienverbrauch = 2550, gesundheitszustand = 4, healthScore = 82, stressLevel = 3, altersgruppe = "20-30", geschlecht = "m" }
    , { schritte = 4000, wasserzufuhr = 2.4, schlafdauer = 7, kalorienverbrauch = 1950, gesundheitszustand = 2, healthScore = 60, stressLevel = 1, altersgruppe = "40-50", geschlecht = "w" }
    , { schritte = 7800, wasserzufuhr = 1.7, schlafdauer = 6.5, kalorienverbrauch = 2400, gesundheitszustand = 4, healthScore = 77, stressLevel = 2, altersgruppe = "30-40", geschlecht = "m" }
    , { schritte = 5200, wasserzufuhr = 2.1, schlafdauer = 7.2, kalorienverbrauch = 2150, gesundheitszustand = 3, healthScore = 69, stressLevel = 2, altersgruppe = "50-60", geschlecht = "w" }
    , { schritte = 9200, wasserzufuhr = 1.3, schlafdauer = 5.8, kalorienverbrauch = 2700, gesundheitszustand = 5, healthScore = 88, stressLevel = 3, altersgruppe = "20-30", geschlecht = "m" }
    , { schritte = 3000, wasserzufuhr = 2.7, schlafdauer = 8.5, kalorienverbrauch = 1800, gesundheitszustand = 2, healthScore = 55, stressLevel = 1, altersgruppe = "60-70", geschlecht = "w" }
    , { schritte = 6500, wasserzufuhr = 2, schlafdauer = 7, kalorienverbrauch = 2250, gesundheitszustand = 3, healthScore = 72, stressLevel = 2, altersgruppe = "40-50", geschlecht = "m" }
    , { schritte = 8300, wasserzufuhr = 1.5, schlafdauer = 6.2, kalorienverbrauch = 2500, gesundheitszustand = 4, healthScore = 80, stressLevel = 3, altersgruppe = "30-40", geschlecht = "w" }
    , { schritte = 4700, wasserzufuhr = 2.3, schlafdauer = 7.3, kalorienverbrauch = 2100, gesundheitszustand = 3, healthScore = 66, stressLevel = 2, altersgruppe = "50-60", geschlecht = "m" }
    , { schritte = 7800, wasserzufuhr = 1.6, schlafdauer = 6.1, kalorienverbrauch = 2450, gesundheitszustand = 4, healthScore = 79, stressLevel = 3, altersgruppe = "20-30", geschlecht = "w" }
    , { schritte = 3900, wasserzufuhr = 2.5, schlafdauer = 7.8, kalorienverbrauch = 2000, gesundheitszustand = 2, healthScore = 61, stressLevel = 1, altersgruppe = "40-50", geschlecht = "m" }
    , { schritte = 6000, wasserzufuhr = 1.9, schlafdauer = 6.7, kalorienverbrauch = 2200, gesundheitszustand = 3, healthScore = 73, stressLevel = 2, altersgruppe = "30-40", geschlecht = "w" }
    ]



-- UPDATE

type Msg
    = ChangeX String
    | ChangeY String
    | TogglePlot


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeX newX ->
            ( { model | selectedX = newX }, Cmd.none )

        ChangeY newY ->
            ( { model | selectedY = newY }, Cmd.none )

        TogglePlot ->
            ( { model | showPlot = not model.showPlot }, Cmd.none )


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
        , option [ HtmlAttr.value "Wasserzufuhr", HtmlAttr.selected (selected == "Wasserzufuhr") ] [ Html.text "Wasserzufuhr" ]
        , option [ HtmlAttr.value "Schlafdauer", HtmlAttr.selected (selected == "Schlafdauer") ] [ Html.text "Schlafdauer" ]
        ]


axisSelectY : String -> Html Msg
axisSelectY selected =
    select [ HtmlEvents.onInput ChangeY ]
        [ option [ HtmlAttr.value "Kalorienverbrauch", HtmlAttr.selected (selected == "Kalorienverbrauch") ] [ Html.text "Kalorienverbrauch" ]
        , option [ HtmlAttr.value "Gesundheitszustand", HtmlAttr.selected (selected == "Gesundheitszustand") ] [ Html.text "Gesundheitszustand" ]
        , option [ HtmlAttr.value "Health Score", HtmlAttr.selected (selected == "Health Score") ] [ Html.text "Health Score" ]
        ]


-- Hilfsfunktion zum dynamischen Zugriff auf Werte je Achse

getValueForAxis : DataPoint -> String -> Float
getValueForAxis dp axis =
    case axis of
        "Schritte" ->
            dp.schritte

        "Wasserzufuhr" ->
            dp.wasserzufuhr

        "Schlafdauer" ->
            dp.schlafdauer

        "Kalorienverbrauch" ->
            dp.kalorienverbrauch

        "Gesundheitszustand" ->
            dp.gesundheitszustand

        "Health Score" ->
            dp.healthScore

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
                        , SvgAttr.fill (colorByGender dp.geschlecht)
                        , SvgAttr.stroke (colorByStress dp.stressLevel)
                        , SvgAttr.strokeWidth "2"
                        , SvgAttr.opacity "0.8"
                        , SvgAttr.title ("Stress: " ++ String.fromInt dp.stressLevel ++ ", Alter: " ++ dp.altersgruppe)
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
test

-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }