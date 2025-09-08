module Main exposing (..)

import Browser
import Csv.Parser exposing (parse)
import Http
import Html exposing (Html, button, div, option, select, text, input, label)
import Html.Attributes as HtmlAttr
import Html.Events as HtmlEvents
import Svg exposing (..)
import Svg.Attributes as SvgAttr
import Svg.Events as SvgEvents
import String
import Maybe exposing (withDefault)
import Debug


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


type alias Model =
    { data : List Person
    , selectedX : String
    , selectedY : String
    , showPlot : Bool
    , showMale : Bool
    , showFemale : Bool
    , hoveredPoint : Maybe Person
    }


-- CSV PARSING

rowToPerson : List String -> Person
rowToPerson row =
    let
        get i = List.head (List.drop i row) |> withDefault ""
        toInt s = String.toInt s |> withDefault 0
        toFloatSafe s = String.toFloat s |> withDefault 0
    in
    { id = toInt (get 0)
    , gender = get 1
    , age = toInt (get 2)
    , occupation = get 3
    , sleepDuration = toFloatSafe (get 4)
    , sleepQuality = toInt (get 5)
    , physicalActivityLevel = toInt (get 6)
    , stressLevel = toInt (get 7)
    , bmi = get 8
    , bloodPressure = get 9
    , heartRate = toInt (get 10)
    , dailySteps = toInt (get 11)
    , sleepDisorder = get 12
    }


-- INIT

init : () -> ( Model, Cmd Msg )
init _ =
    ( { data = []
      , selectedX = "Physical Activity"
      , selectedY = "Sleep Duration"
      , showPlot = True
      , showMale = True
      , showFemale = True
      , hoveredPoint = Nothing
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
    = ChangeX String
    | ChangeY String
    | TogglePlot
    | CsvLoaded (Result Http.Error String)
    | ToggleMale Bool
    | ToggleFemale Bool
    | ClickPoint Person


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

        CsvLoaded (Ok csvString) ->
            case parse { fieldSeparator = ',' } csvString of
                Ok rows ->
                    let
                        dataRows = List.drop 1 rows
                        persons = List.map rowToPerson dataRows
                        _ = Debug.log ("Loaded persons: " ++ String.fromInt (List.length persons)) persons
                    in
                    ( { model | data = persons }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        CsvLoaded (Err _) ->
            ( model, Cmd.none )

        ClickPoint dp ->
            ( { model | hoveredPoint = Just dp }, Cmd.none )


-- VIEW

view : Model -> Html Msg
view model =
    div [ HtmlAttr.style "font-family" "Arial, sans-serif", HtmlAttr.style "margin" "20px" ]
        [ div [ HtmlAttr.style "margin-bottom" "10px" ]
            [ Html.text "X-AXIS: "
            , axisSelectX model.selectedX
            ]
        , div [ HtmlAttr.style "margin-bottom" "10px" ]
            [ Html.text "Y-AXIS: "
            , axisSelectY model.selectedY
            ]
        , div [ HtmlAttr.style "margin" "10px 0" ]
            [ button [ HtmlEvents.onClick TogglePlot ]
                [ Html.text (if model.showPlot then "HIDE PLOT" else "SHOW PLOT") ]
            ]
        , div [ HtmlAttr.style "margin-bottom" "10px", HtmlAttr.style "padding-left" "20px" ]
            [ labelCheckbox "MALE" model.showMale ToggleMale
            , labelCheckbox "FEMALE" model.showFemale ToggleFemale
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
        [ option [ HtmlAttr.value "Physical Activity", HtmlAttr.selected (selected == "Physical Activity") ] [ Html.text "Physical Activity" ]
        , option [ HtmlAttr.value "Daily Steps", HtmlAttr.selected (selected == "Daily Steps") ] [ Html.text "Daily Steps" ]
        ]


axisSelectY : String -> Html Msg
axisSelectY selected =
    select [ HtmlEvents.onInput ChangeY ]
        [ option [ HtmlAttr.value "Sleep Duration", HtmlAttr.selected (selected == "Sleep Duration") ] [ Html.text "Sleep Duration" ]
        , option [ HtmlAttr.value "Heart Rate", HtmlAttr.selected (selected == "Heart Rate") ] [ Html.text "Heart Rate" ]
        , option [ HtmlAttr.value "BMI Category", HtmlAttr.selected (selected == "BMI Category") ] [ Html.text "BMI Category" ]
        ]


-- HILFSFUNKTIONEN

getValueForAxis : Person -> String -> Float
getValueForAxis dp axis =
    case axis of
        "Sleep Duration" -> dp.sleepDuration
        "Daily Steps" -> toFloat dp.dailySteps
        "Physical Activity" -> toFloat dp.physicalActivityLevel
        "Heart Rate" -> toFloat dp.heartRate
        "BMI Category" ->
            case String.toLower dp.bmi of
                "underweight" -> 1
                "normal" -> 2
                "overweight" -> 3
                "obese" -> 4
                _ -> 0
        _ -> 0


ticksForAxis : String -> List Float -> List Float
ticksForAxis axis values =
    let
        minVal = List.minimum values |> withDefault 0
        maxVal = List.maximum values |> withDefault (minVal + 10)
        step = (maxVal - minVal) / 4
    in
    List.map (\i -> minVal + step * toFloat i) [0,1,2,3,4]


-- SCATTERPLOT

scatterPlotView : Model -> Html Msg
scatterPlotView model =
    let
        genderMatches dp =
            let g = String.toLower dp.gender in
            ((model.showMale && (g == "male" || g == "m")) ||
             (model.showFemale && (g == "female" || g == "f")))

        filteredData =
            List.filter genderMatches model.data

        xs = List.map (\dp -> getValueForAxis dp model.selectedX) filteredData
        ys = List.map (\dp -> getValueForAxis dp model.selectedY) filteredData

        minX = List.minimum xs |> withDefault 0
        maxX = List.maximum xs |> withDefault (minX + 1)
        minY = List.minimum ys |> withDefault 0
        maxY = List.maximum ys |> withDefault (minY + 1)

        plotPaddingLeft = 120
        plotPaddingRight = 40
        plotPaddingTop = 40
        plotPaddingBottom = 50
        plotWidth = 1100
        plotHeight = 500

        scaleX x =
            if maxX == minX then
                plotPaddingLeft + (plotWidth - plotPaddingLeft - plotPaddingRight)/2
            else
                plotPaddingLeft + ((x - minX) / (maxX - minX)) * (plotWidth - plotPaddingLeft - plotPaddingRight)

        scaleY y =
            if maxY == minY then
                plotHeight / 2
            else
                plotHeight - plotPaddingBottom - ((y - minY) / (maxY - minY)) * (plotHeight - plotPaddingTop - plotPaddingBottom)

        color dp =
            let g = String.toLower dp.gender in
            if g == "male" then "#2D68C4"
            else if g == "female" then "#DE5D83"
            else "gray"

        xTicks = ticksForAxis model.selectedX xs
        yTicks = ticksForAxis model.selectedY ys

        tickLabel axis v =
            let
                label =
                    case axis of
                        "BMI Category" ->
                            case round v of
                                1 -> "Underweight"
                                2 -> "Normal"
                                3 -> "Overweight"
                                4 -> "Obese"
                                _ -> ""
                        _ -> String.fromFloat ((toFloat (round (v * 10))) / 10)
            in
            Svg.text_
                [ SvgAttr.x (String.fromFloat (plotPaddingLeft - 10))
                , SvgAttr.y (String.fromFloat (scaleY v))
                , SvgAttr.textAnchor "end"
                ]
                [ Svg.text label ]

        tickLabelX v =
            Svg.text_
                [ SvgAttr.x (String.fromFloat (scaleX v))
                , SvgAttr.y (String.fromFloat (plotHeight - plotPaddingBottom + 20))
                , SvgAttr.textAnchor "middle"
                ]
                [ Svg.text (String.fromFloat ((toFloat (round (v * 10))) / 10)) ]

        xAxis =
            Svg.line
                [ SvgAttr.x1 (String.fromFloat plotPaddingLeft)
                , SvgAttr.y1 (String.fromFloat (plotHeight - plotPaddingBottom))
                , SvgAttr.x2 (String.fromFloat (plotWidth - plotPaddingRight))
                , SvgAttr.y2 (String.fromFloat (plotHeight - plotPaddingBottom))
                , SvgAttr.stroke "black"
                , SvgAttr.strokeWidth "2"
                ]
                []

        yAxis =
            Svg.line
                [ SvgAttr.x1 (String.fromFloat plotPaddingLeft)
                , SvgAttr.y1 (String.fromFloat (plotHeight - plotPaddingBottom))
                , SvgAttr.x2 (String.fromFloat plotPaddingLeft)
                , SvgAttr.y2 (String.fromFloat plotPaddingTop)
                , SvgAttr.stroke "black"
                , SvgAttr.strokeWidth "2"
                ]
                []

        xLabel =
            Svg.text_
                [ SvgAttr.x (String.fromFloat (plotWidth / 2))
                , SvgAttr.y (String.fromFloat (plotHeight))
                , SvgAttr.textAnchor "middle"
                , SvgAttr.fontSize "16"
                , SvgAttr.fontWeight "bold"
                ]
                [ Svg.text model.selectedX ]

        yLabel =
            Svg.text_
                [ SvgAttr.x "30"
                , SvgAttr.y (String.fromFloat (plotHeight / 2))
                , SvgAttr.transform ("rotate(-90 30 " ++ String.fromFloat (plotHeight / 2) ++ ")")
                , SvgAttr.textAnchor "middle"
                , SvgAttr.fontSize "16"
                , SvgAttr.fontWeight "bold"
                ]
                [ Svg.text model.selectedY ]

        points =
            List.map
                (\dp ->
                    let
                        cx = scaleX (getValueForAxis dp model.selectedX)
                        cy = scaleY (getValueForAxis dp model.selectedY)
                    in
                    Svg.circle
                        [ SvgAttr.cx (String.fromFloat cx)
                        , SvgAttr.cy (String.fromFloat cy)
                        , SvgAttr.r "5"
                        , SvgAttr.fill (color dp)
                        , SvgAttr.stroke "black"
                        , SvgAttr.strokeWidth "1"
                        , SvgEvents.onClick (ClickPoint dp)
                        ]
                        []
                )
                filteredData

        tooltipBox =
            case model.hoveredPoint of
                Just dp ->
                    div [ HtmlAttr.style "margin-left" "20px"
                        , HtmlAttr.style "padding" "10px"
                        , HtmlAttr.style "border" "1px solid black"
                        , HtmlAttr.style "background" "#f9f9f9"
                        , HtmlAttr.style "width" "220px"
                        , HtmlAttr.style "max-height" "120px"
                        , HtmlAttr.style "overflow-y" "auto"
                        ]
                        [ Html.text ("ID: " ++ String.fromInt dp.id)
                        , Html.br [] []
                        , Html.text ("Gender: " ++ dp.gender)
                        , Html.br [] []
                        , Html.text ("Age: " ++ String.fromInt dp.age)
                        , Html.br [] []
                        , Html.text (model.selectedX ++ ": " ++ (if model.selectedX == "BMI Category" then dp.bmi else String.fromFloat (getValueForAxis dp model.selectedX)))
                        , Html.br [] []
                        , Html.text (model.selectedY ++ ": " ++ (if model.selectedY == "BMI Category" then dp.bmi else String.fromFloat (getValueForAxis dp model.selectedY)))
                        ]

                Nothing ->
                    div [ HtmlAttr.style "margin-left" "20px"
                        , HtmlAttr.style "color" "gray"
                        ]
                        [ Html.text "Klicke auf einen Punkt" ]
    in
    div [ HtmlAttr.style "display" "flex" ]
        [ Svg.svg [ SvgAttr.width (String.fromFloat plotWidth), SvgAttr.height (String.fromFloat plotHeight) ]
            (points ++ [ xAxis, yAxis, xLabel, yLabel ]
                ++ List.map (\v -> Svg.line [ SvgAttr.x1 (String.fromFloat (scaleX v)), SvgAttr.y1 (String.fromFloat (plotHeight - plotPaddingBottom)), SvgAttr.x2 (String.fromFloat (scaleX v)), SvgAttr.y2 (String.fromFloat (plotHeight - plotPaddingBottom + 5)), SvgAttr.stroke "black" ] []) xTicks
                ++ List.map (\v -> Svg.line [ SvgAttr.x1 (String.fromFloat (plotPaddingLeft - 5)), SvgAttr.y1 (String.fromFloat (scaleY v)), SvgAttr.x2 (String.fromFloat plotPaddingLeft), SvgAttr.y2 (String.fromFloat (scaleY v)), SvgAttr.stroke "black" ] []) yTicks
                ++ List.map tickLabelX xTicks
                ++ List.map (tickLabel model.selectedY) yTicks
            )
        , tooltipBox
        ]


-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
