module Test exposing (..)

import Browser
import Csv.Parser exposing (parse)
import Http
import Html exposing (Html)
import Html.Attributes as HtmlAttr
import Html.Events as HtmlEvents
import Svg exposing (..)
import Svg.Attributes as SvgAttr
import Svg.Events as SvgEvents
import String
import Maybe exposing (withDefault)
import Debug
import List.Extra exposing (unique, elemIndex)


-- MODEL

type PlotType = Scatter | Parallel

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
    , currentPlot : PlotType
    }


-- INIT

init : () -> ( Model, Cmd Msg )
init _ =
    ( { data = []
      , selectedX = "Daily Steps"
      , selectedY = "Sleep Duration"
      , showPlot = True
      , showMale = True
      , showFemale = True
      , hoveredPoint = Nothing
      , currentPlot = Scatter
      }
    , loadCsv
    )


-- CSV LOADING

type Msg
    = ChangeX String
    | ChangeY String
    | TogglePlot
    | CsvLoaded (Result Http.Error String)
    | ToggleMale Bool
    | ToggleFemale Bool
    | ClickPoint Person
    | ChangePlot String


loadCsv : Cmd Msg
loadCsv =
    Http.get
        { url = "data/Sleep_health_and_lifestyle_dataset.csv"
        , expect = Http.expectString CsvLoaded
        }


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


-- UPDATE

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
                    in
                    ( { model | data = persons }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        CsvLoaded (Err _) ->
            ( model, Cmd.none )

        ClickPoint dp ->
            ( { model | hoveredPoint = Just dp }, Cmd.none )

        ChangePlot pStr ->
            let plotType =
                    case pStr of
                        "scatter" -> Scatter
                        "parallel" -> Parallel
                        _ -> Scatter
            in
            ( { model | currentPlot = plotType }, Cmd.none )


-- VIEW

view : Model -> Html Msg
view model =
    Html.div [ HtmlAttr.style "font-family" "Arial, sans-serif", HtmlAttr.style "margin" "20px" ]
        [ if model.currentPlot == Scatter then
            scatterControls model
          else
            parallelControls model
        ]


-- SCATTERPLOT CONTROLS AND VIEW

scatterControls : Model -> Html Msg
scatterControls model =
    Html.div [ HtmlAttr.style "display" "flex" ]
        [ Html.div
            [ HtmlAttr.style "width" "300px"  -- noch breiter
            , HtmlAttr.style "margin-right" "20px"
            , HtmlAttr.style "padding" "25px"
            , HtmlAttr.style "border" "1px solid #ccc"
            , HtmlAttr.style "border-radius" "8px"
            , HtmlAttr.style "background" "#f5f5f5"
            , HtmlAttr.style "box-sizing" "border-box"
            ]
            [ Html.div [ HtmlAttr.style "margin-bottom" "25px" ]
                [ Html.text "X-AXIS:"
                , Html.br [] []
                , Html.select 
                    [ HtmlEvents.onInput ChangeX
                    , HtmlAttr.style "width" "100%"
                    , HtmlAttr.style "padding" "12px"
                    , HtmlAttr.style "font-size" "16px"
                    , HtmlAttr.style "min-width" "200px"
                    , HtmlAttr.style "box-sizing" "border-box"
                    ]
                    [ Html.option [ HtmlAttr.value "Physical Activity", HtmlAttr.selected (model.selectedX == "Physical Activity") ] [ Html.text "Physical Activity" ]
                    , Html.option [ HtmlAttr.value "Daily Steps", HtmlAttr.selected (model.selectedX == "Daily Steps") ] [ Html.text "Daily Steps" ]
                    ]
                ]
            , Html.div [ HtmlAttr.style "margin-bottom" "25px" ]
                [ Html.text "Y-AXIS:"
                , Html.br [] []
                , Html.select 
                    [ HtmlEvents.onInput ChangeY
                    , HtmlAttr.style "width" "100%"
                    , HtmlAttr.style "padding" "12px"
                    , HtmlAttr.style "font-size" "16px"
                    , HtmlAttr.style "min-width" "200px"
                    , HtmlAttr.style "box-sizing" "border-box"
                    ]
                    [ Html.option [ HtmlAttr.value "Sleep Duration", HtmlAttr.selected (model.selectedY == "Sleep Duration") ] [ Html.text "Sleep Duration" ]
                    , Html.option [ HtmlAttr.value "Heart Rate", HtmlAttr.selected (model.selectedY == "Heart Rate") ] [ Html.text "Heart Rate" ]
                    ]
                ]
            , Html.div [ HtmlAttr.style "margin-bottom" "25px" ]
                [ Html.button
                    [ HtmlEvents.onClick TogglePlot
                    , HtmlAttr.style "width" "100%"
                    , HtmlAttr.style "padding" "12px"
                    , HtmlAttr.style "background-color" "#007acc"
                    , HtmlAttr.style "color" "white"
                    , HtmlAttr.style "border" "none"
                    , HtmlAttr.style "border-radius" "4px"
                    , HtmlAttr.style "cursor" "pointer"
                    , HtmlAttr.style "font-size" "16px"
                    ]
                    [ Html.text (if model.showPlot then "HIDE PLOT" else "SHOW PLOT") ]
                ]
            , Html.div []
                [ labelCheckbox "MALE" model.showMale ToggleMale
                , Html.br [] []
                , labelCheckbox "FEMALE" model.showFemale ToggleFemale
                ]
            ]
        , if model.showPlot then
            scatterPlotView model
          else
            Html.text ""
        ]




labelCheckbox : String -> Bool -> (Bool -> Msg) -> Html Msg
labelCheckbox labelText checked toMsg =
    Html.label [ HtmlAttr.style "display" "block", HtmlAttr.style "margin-bottom" "8px" ]
        [ Html.input [ HtmlAttr.type_ "checkbox", HtmlAttr.checked checked, HtmlEvents.onCheck toMsg ] []
        , Html.text (" " ++ labelText)
        ]


axisSelectX : String -> Html Msg
axisSelectX selected =
    Html.select [ HtmlEvents.onInput ChangeX, HtmlAttr.style "width" "180%", HtmlAttr.style "padding" "5px", HtmlAttr.style "margin-top" "5px" ]
        [ Html.option [ HtmlAttr.value "Physical Activity", HtmlAttr.selected (selected == "Physical Activity") ] [ Html.text "Physical Activity" ]
        , Html.option [ HtmlAttr.value "Daily Steps", HtmlAttr.selected (selected == "Daily Steps") ] [ Html.text "Daily Steps" ]
        ]


axisSelectY : String -> Html Msg
axisSelectY selected =
    Html.select [ HtmlEvents.onInput ChangeY, HtmlAttr.style "width" "180%", HtmlAttr.style "padding" "5px", HtmlAttr.style "margin-top" "5px" ]
        [ Html.option [ HtmlAttr.value "Sleep Duration", HtmlAttr.selected (selected == "Sleep Duration") ] [ Html.text "Sleep Duration" ]
        , Html.option [ HtmlAttr.value "Heart Rate", HtmlAttr.selected (selected == "Heart Rate") ] [ Html.text "Heart Rate" ]
        ]


-- VALUE FETCH

getValueForAxis : Person -> String -> Float
getValueForAxis dp axis =
    case axis of
        "Sleep Duration" -> dp.sleepDuration
        "Daily Steps" -> toFloat dp.dailySteps
        "Physical Activity" -> toFloat dp.physicalActivityLevel
        "Heart Rate" -> toFloat dp.heartRate
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

        plotPaddingLeft = 100
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
            Svg.text_
                [ SvgAttr.x (String.fromFloat (plotPaddingLeft - 10))
                , SvgAttr.y (String.fromFloat (scaleY v))
                , SvgAttr.textAnchor "end"
                ]
                [ Svg.text (String.fromFloat ((toFloat (round (v * 10))) / 10)) ]

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
                        , SvgEvents.onClick (ClickPoint dp)
                        ]
                        []
                )
                filteredData

        tooltipBox =
            case model.hoveredPoint of
                Just dp ->
                    Html.div
                        [ HtmlAttr.style "margin-left" "20px"
                        , HtmlAttr.style "padding" "10px"
                        , HtmlAttr.style "background" "#e0f0ff"
                        , HtmlAttr.style "border-radius" "8px"
                        , HtmlAttr.style "width" "220px"
                        , HtmlAttr.style "max-height" "180px"
                        , HtmlAttr.style "overflow-y" "auto"
                        , HtmlAttr.style "box-shadow" "0 2px 5px rgba(0,0,0,0.1)"
                        ]
                        [ Html.div [] [ Html.span [ HtmlAttr.style "color" "rgb(0, 122, 204)", HtmlAttr.style "font-weight" "bold" ] [ Html.text "ID: " ], Html.span [ HtmlAttr.style "color" "black" ] [ Html.text (String.fromInt dp.id) ] ]
                        , Html.div [] [ Html.span [ HtmlAttr.style "color" "rgb(0, 122, 204)", HtmlAttr.style "font-weight" "bold" ] [ Html.text "Gender: " ], Html.span [ HtmlAttr.style "color" "black" ] [ Html.text dp.gender ] ]
                        , Html.div [] [ Html.span [ HtmlAttr.style "color" "rgb(0, 122, 204)", HtmlAttr.style "font-weight" "bold" ] [ Html.text "Age: " ], Html.span [ HtmlAttr.style "color" "black" ] [ Html.text (String.fromInt dp.age) ] ]
                        , Html.div [] [ Html.span [ HtmlAttr.style "color" "rgb(0, 122, 204)", HtmlAttr.style "font-weight" "bold" ] [ Html.text (model.selectedX ++ ": ") ], Html.span [ HtmlAttr.style "color" "black" ] [ Html.text (String.fromFloat (getValueForAxis dp model.selectedX)) ] ]
                        , Html.div [] [ Html.span [ HtmlAttr.style "color" "rgb(0, 122, 204)", HtmlAttr.style "font-weight" "bold" ] [ Html.text (model.selectedY ++ ": ") ], Html.span [ HtmlAttr.style "color" "black" ] [ Html.text (String.fromFloat (getValueForAxis dp model.selectedY)) ] ]
                        ]

                Nothing ->
                    Html.div
                        [ HtmlAttr.style "margin-left" "20px"
                        , HtmlAttr.style "color" "gray"
                        ]
                        [ Html.text "Klicke auf einen Punkt" ]
    in
    Html.div [ HtmlAttr.style "display" "flex" ]
        [ Svg.svg [ SvgAttr.width (String.fromFloat plotWidth), SvgAttr.height (String.fromFloat plotHeight) ]
            (points ++ [ xAxis, yAxis, xLabel, yLabel ]
                ++ List.map (\v -> Svg.line [ SvgAttr.x1 (String.fromFloat (scaleX v)), SvgAttr.y1 (String.fromFloat (plotHeight - plotPaddingBottom)), SvgAttr.x2 (String.fromFloat (scaleX v)), SvgAttr.y2 (String.fromFloat (plotHeight - plotPaddingBottom + 5)), SvgAttr.stroke "black" ] []) xTicks
                ++ List.map (\v -> Svg.line [ SvgAttr.x1 (String.fromFloat (plotPaddingLeft - 5)), SvgAttr.y1 (String.fromFloat (scaleY v)), SvgAttr.x2 (String.fromFloat plotPaddingLeft), SvgAttr.y2 (String.fromFloat (scaleY v)), SvgAttr.stroke "black" ] []) yTicks
                ++ List.map tickLabelX xTicks
                ++ List.map (tickLabel model.selectedY) yTicks
            )
        , tooltipBox
        ]


-- PARALLEL COORDINATES

parallelControls : Model -> Html Msg
parallelControls model =
    Html.div []
        [ if List.isEmpty model.data then
            Html.text "Loading..."
          else
            parallelPlotView model.data
        ]


type alias Axis =
    { name : String
    , getValue : Person -> Float
    , min : Float
    , max : Float
    , tickLabels : List String
    }


parallelPlotView : List Person -> Html Msg
parallelPlotView people =
    let
        paddingTop = 50
        paddingLeft = 450

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
              , getValue = (\p -> toFloat p.stressLevel)
              , min = 0
              , max = 10
              , tickLabels = List.map String.fromInt (List.range 0 10)
              }
            ]

        baseSpacing = 120
        extraBetween01 = 70
        extraBetween12 = 70
        extraBetween23 = 40

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
            Svg.polyline
                [ SvgAttr.points
                    (points
                        |> List.map (\(x, y) -> String.fromFloat x ++ "," ++ String.fromFloat y)
                        |> String.join " "
                    )
                , SvgAttr.fill "none"
                , SvgAttr.stroke (color p.occupation)
                , SvgAttr.strokeWidth "1.5"
                , SvgAttr.strokeOpacity "0.6"
                ]
                []

        personPoints p =
            axes
                |> List.indexedMap
                    (\i axis ->
                        Svg.circle
                            [ SvgAttr.cx (String.fromInt (axisX i))
                            , SvgAttr.cy (String.fromFloat (scaleY axis.min axis.max (axis.getValue p)))
                            , SvgAttr.r "4"
                            , SvgAttr.fill (color p.occupation)
                            , SvgAttr.stroke "black"
                            , SvgAttr.strokeWidth "0.5"
                            ]
                            []
                    )
    in
    Html.node "div" []
        [ Svg.svg
            [ SvgAttr.width (String.fromInt w)
            , SvgAttr.height (String.fromInt (paddingTop + h + 50))
            ]
            ( List.concat
                [ axes
                    |> List.indexedMap
                        (\i _ ->
                            Svg.line
                                [ SvgAttr.x1 (String.fromInt (axisX i))
                                , SvgAttr.y1 (String.fromInt paddingTop)
                                , SvgAttr.x2 (String.fromInt (axisX i))
                                , SvgAttr.y2 (String.fromInt (paddingTop + h))
                                , SvgAttr.stroke "black"
                                , SvgAttr.strokeWidth "1"
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
                                        [ Svg.line
                                            [ SvgAttr.x1 (String.fromInt (axisX i - 5))
                                            , SvgAttr.y1 (String.fromFloat y)
                                            , SvgAttr.x2 (String.fromInt (axisX i + 5))
                                            , SvgAttr.y2 (String.fromFloat y)
                                            , SvgAttr.stroke "black"
                                            , SvgAttr.strokeWidth "1"
                                            ]
                                            []
                                        , Svg.text_
                                            [ SvgAttr.x (String.fromInt (axisX i - 10))
                                            , SvgAttr.y (String.fromFloat (y + 4))
                                            , SvgAttr.fontSize "12"
                                            , SvgAttr.textAnchor "end"
                                            ]
                                            [ Svg.text label ]
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
                            Svg.text_
                                [ SvgAttr.x (String.fromInt (axisX i))
                                , SvgAttr.y (String.fromInt (paddingTop + h + 30))
                                , SvgAttr.fontSize "14"
                                , SvgAttr.textAnchor "middle"
                                ]
                                [ Svg.text axis.name ]
                        )
                ]
            )
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
