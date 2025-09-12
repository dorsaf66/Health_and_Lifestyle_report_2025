port module Baum exposing (..)

import Browser
import Csv.Parser exposing (parse)
import Http
import Html exposing (Html, button, div, option, select, text, input, label)
import Html.Attributes as HtmlAttr
import Html.Events as HtmlEvents
import Svg exposing (..)
import Svg.Attributes as SvgAttr
import String


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


type alias Model =
    { data : List Person
    , correlationMatrix : List (List Float)
    , selectedX : String
    , selectedY : String
    , showPlot : Bool
    , showMale : Bool
    , showFemale : Bool
    }


type alias Node = { id: Int, label : String }
type alias Edge = { from: Int, to: Int, weight: Float }      


-- CSV PARSING

rowToPerson : List String -> Maybe Person
rowToPerson row =
    case row of
        idStr :: genderStr :: ageStr :: occupationStr :: sleepDurationStr :: sleepQualityStr :: physicalActivityLevelStr :: stressLevelStr :: bmiStr :: bloodPressureStr :: heartRateStr  :: dailyStepsStr :: sleepDisorderStr :: [] ->
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
        , selectedX = "Stresslevel"
        , selectedY = "Schlafdauer"
        , showPlot = True
        , showMale = True
        , showFemale = True
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
    | MatrixReceived (List (List Float ))


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

        MatrixReceived matrix ->
            ( { model | correlationMatrix = matrix }, Cmd.none )

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


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveMatrix MatrixReceived


-- VIEW

view : Model -> Html Msg
view model =
    div [ HtmlAttr.style "font-family" "Arial, sans-serif", HtmlAttr.style "margin" "20px" ]
        [ div [ HtmlAttr.style "margin-bottom" "10px" ]
            [ Html.text "X-ACHSE: "
            , axisSelectX model.selectedX
            ]
        , div [ HtmlAttr.style "margin-bottom" "10px" ]
            [ Html.text "Y-ACHSE: "
            , axisSelectY model.selectedY
            ]
        , div [ HtmlAttr.style "margin" "10px 0" ]
            [ button [ HtmlEvents.onClick TogglePlot ]
                [ Html.text (if model.showPlot then "PLOT VERBERGEN" else "PLOT ANZEIGEN") ]
            ]
        , div [ HtmlAttr.style "margin-bottom" "10px", HtmlAttr.style "padding-left" "20px" ]
            [ labelCheckbox "MÄNNER" model.showMale ToggleMale
            , labelCheckbox "FRAUEN" model.showFemale ToggleFemale
            ]
        , if model.showPlot then
            scatterPlotView model
          else
            Html.text ""
        , graphView model
        , Html.text (Debug.toString model.correlationMatrix)
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
        [ option [ HtmlAttr.value "Schlafdauer", HtmlAttr.selected (selected == "Schlafdauer") ] [ Html.text "Schlafdauer" ]
        , option [ HtmlAttr.value "Schritte", HtmlAttr.selected (selected == "Schritte") ] [ Html.text "Schritte" ]
        , option [ HtmlAttr.value "Beruf", HtmlAttr.selected (selected == "Beruf") ] [ Html.text "Beruf" ]
        -- , option [ HtmlAttr.value "Kalorienaufnahme", HtmlAttr.selected (selected == "Kalorienaufnahme") ] [ Html.text "Kalorienaufnahme" ]
        -- , option [ HtmlAttr.value "Alter", HtmlAttr.selected (selected == "Alter") ] [ Html.text "Alter" ]
        ]


axisSelectY : String -> Html Msg
axisSelectY selected =
    select [ HtmlEvents.onInput ChangeY ]
        [ option [ HtmlAttr.value "Stresslevel", HtmlAttr.selected (selected == "Stresslevel") ] [ Html.text "Stresslevel" ]
        , option [ HtmlAttr.value "Herzfrequenz", HtmlAttr.selected (selected == "Herzfrequenz") ] [ Html.text "Herzfrequenz" ]
        -- , option [ HtmlAttr.value "Herzfrequenz", HtmlAttr.selected (selected == "Herzfrequenz") ] [ Html.text "Herzfrequenz" ]
        ]


-- HILFSFUNKTION

getValueForAxis : Person -> String -> Float
getValueForAxis dp axis =
    case axis of
        "Schlafdauer" -> dp.sleepDuration
        "Stresslevel" -> toFloat dp.stressLevel
        "Schritte" -> toFloat dp.dailySteps
        -- "BMI" -> dp.bmi
        "Herzfrequenz" -> toFloat dp.heartRate
        -- "Alter" -> toFloat dp.age
        _ -> 0


-- SCATTERPLOT

width : Float
width = 800

height : Float
height = 600

padding : Float
padding = 65


ticksForAxis : String -> List Float
ticksForAxis axis =
    case axis of
        "Stresslevel" -> [ 0, 2, 4, 6, 8, 10 ]
        "Schritte" -> [ 0, 5000, 10000, 15000, 20000 ]
        "Trainingsstunden (pro Woche)" -> [ 0, 2, 4, 6, 8, 10 ]
        "Beruf" -> [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
        -- [ "Software Engineer", "Doctor", "Sales Representative", "Teacher", "Nurse", "Engineer", "Accountant", "Scientist", "Lawyer", "Salesperson", "Manager"]
        "Schlafdauer" -> [ 0, 2, 4, 6, 8, 10 ]
        "BMI" -> [ 0, 15, 20, 25, 30 ]
        "Herzfrequenz" -> [ 0, 40, 80, 100, 140 ]
        "Alter" -> [ 0, 20, 40, 60, 80, 100 ]
        _ -> []


scatterPlotView : Model -> Html Msg
scatterPlotView model =
    let
        filteredData =
            List.filter
                (\dp ->
                    ((model.showMale && dp.gender == "Male") || (model.showFemale && dp.gender == "Female"))
                    --    && (dp.occupation == model.showOccupation)
                )
                model.data

        xs = List.map (\dp -> getValueForAxis dp model.selectedX) filteredData
        ys = List.map (\dp -> getValueForAxis dp model.selectedY) filteredData

        minX = 0
        maxX = List.maximum xs |> Maybe.withDefault 1
        minY = 0
        maxY = List.maximum ys |> Maybe.withDefault 1

        -- Umbenannte Variablen, um Shadowing zu vermeiden
        plotPaddingLeft = 80
        plotPadding = 40
        plotWidth = 800
        plotHeight = 500

        scaleX x = plotPaddingLeft + ((x - minX) / (maxX - minX)) * (plotWidth - plotPaddingLeft - plotPadding)
        scaleY y = plotHeight - plotPadding - ((y - minY) / (maxY - minY)) * (plotHeight - 2 * plotPadding)

        color dp =
            if dp.gender == "Male" then "#2D68C4"
            else if dp.gender == "Female" then "#DE5D83"
            else "gray"

        xTicks = ticksForAxis model.selectedX
        yTicks = ticksForAxis model.selectedY

        tickLineX v =
            Svg.line [ SvgAttr.x1 (String.fromFloat (scaleX v)), SvgAttr.y1 (String.fromFloat (plotHeight - plotPadding)), SvgAttr.x2 (String.fromFloat (scaleX v)), SvgAttr.y2 (String.fromFloat (plotHeight - plotPadding + 5)), SvgAttr.stroke "black" ] []

        tickLineY v =
            Svg.line [ SvgAttr.x1 (String.fromFloat (plotPaddingLeft - 5)), SvgAttr.y1 (String.fromFloat (scaleY v)), SvgAttr.x2 (String.fromFloat plotPaddingLeft), SvgAttr.y2 (String.fromFloat (scaleY v)), SvgAttr.stroke "black" ] []

        tickLabelX v =
            Svg.text_ [ SvgAttr.x (String.fromFloat (scaleX v)), SvgAttr.y (String.fromFloat (plotHeight - plotPadding + 20)), SvgAttr.textAnchor "middle" ] [ Svg.text (String.fromFloat v) ]

        tickLabelY v =
            Svg.text_ [ SvgAttr.x (String.fromFloat (plotPaddingLeft - 10)), SvgAttr.y (String.fromFloat (scaleY v + 5)), SvgAttr.textAnchor "end" ] [ Svg.text (String.fromFloat v) ]

        xAxis =
            Svg.line
                [ SvgAttr.x1 (String.fromFloat plotPaddingLeft)
                , SvgAttr.y1 (String.fromFloat (plotHeight - plotPadding))
                , SvgAttr.x2 (String.fromFloat (plotWidth - plotPadding))
                , SvgAttr.y2 (String.fromFloat (plotHeight - plotPadding))
                , SvgAttr.stroke "black"
                , SvgAttr.strokeWidth "2"
                ]
                []

        yAxis =
            Svg.line
                [ SvgAttr.x1 (String.fromFloat plotPaddingLeft)
                , SvgAttr.y1 (String.fromFloat (plotHeight - plotPadding))
                , SvgAttr.x2 (String.fromFloat plotPaddingLeft)
                , SvgAttr.y2 (String.fromFloat plotPadding)
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
                [ SvgAttr.x "20"
                , SvgAttr.y (String.fromFloat (plotHeight / 2))
                , SvgAttr.transform ("rotate(-90 20 " ++ String.fromFloat (plotHeight / 2) ++ ")")
                , SvgAttr.textAnchor "middle"
                , SvgAttr.fontSize "16"
                , SvgAttr.fontWeight "bold"
                ]
                [ Svg.text model.selectedY ]
    in
    Html.div []
        [ Svg.svg [ SvgAttr.width (String.fromFloat plotWidth), SvgAttr.height (String.fromFloat plotHeight) ]
            (List.map (\dp -> Svg.circle [ SvgAttr.cx (String.fromFloat (scaleX (getValueForAxis dp model.selectedX))), SvgAttr.cy (String.fromFloat (scaleY (getValueForAxis dp model.selectedY))), SvgAttr.r "5", SvgAttr.fill (color dp) ] []) filteredData
                ++ [ xAxis, yAxis, xLabel, yLabel ]
                ++ List.map tickLineX xTicks
                ++ List.map tickLineY yTicks
                ++ List.map tickLabelX xTicks
                ++ List.map tickLabelY yTicks
            )
        ]


-- GRAPH

buildGraph : List String -> List (List Float) -> (List Node, List Edge)
buildGraph keys matrix =
    let
        nodes =
            List.indexedMap (\i label -> { id = i, label = label }) keys

        edges =
            List.concatMap
                (\(row, i) ->
                    List.concatMap
                        (\(value, j) ->
                            if abs value > 0.3 && i /= j then
                                [ { from = i, to = j, weight = value } ]
                            else
                                []
                        )
                        (List.indexedMap (\j value -> (value, j)) row)
                )
                (List.indexedMap (\i row -> (row, i)) matrix)
    in
        (nodes, edges)

graphView : Model -> Html Msg
graphView model =
    let
        keys =
            [ "Gender", "Age", "Sleep Duration", "Quality of Sleep", "Physical Activity Level", "Stress Level", "BMI Category", "Heart Rate", "Daily Steps", "Sleep Disorder" ]
        (nodes, edges) =
            buildGraph keys model.correlationMatrix

        -- Kreisförmige Anordnung der Knoten
        nodeCount = List.length nodes
        centerX = 400
        centerY = 140
        radius = 120

        nodePos node =
            let
                angle = 2 * pi * (toFloat node.id) / (toFloat nodeCount)
            in
            ( centerX + radius * cos angle
            , centerY + radius * sin angle
            )


        nodeElements =
            List.map
                (\node ->
                    let 
                        (x, y) = nodePos node
                    in
                    [ Svg.circle
                        [ SvgAttr.cx (String.fromFloat x)
                        , SvgAttr.cy (String.fromFloat y)
                        , SvgAttr.r "20"
                        , SvgAttr.fill "#69b3a2"
                        ]
                        []
                    , Svg.text_
                        [ SvgAttr.x (String.fromFloat x)
                        , SvgAttr.y (String.fromFloat (y + 5))
                        , SvgAttr.textAnchor "middle"
                        , SvgAttr.fontSize "10"
                        , SvgAttr.fontWeight "bold"
                        ]
                        [ Svg.text node.label ]
                    ]
                )
                nodes

        edgeElements =
            List.map
                (\edge ->
                    let
                        fromNode = List.head <| List.filter (\n -> n.id == edge.from) nodes
                        toNode = List.head <| List.filter (\n -> n.id == edge.to) nodes
                    in
                    case (fromNode, toNode) of
                        (Just fn, Just tn) ->
                            let
                                (x1, y1) = nodePos fn
                                (x2, y2) = nodePos tn
                            in
                            Svg.line
                                [ SvgAttr.x1 (String.fromFloat x1)
                                , SvgAttr.y1 (String.fromFloat y1)
                                , SvgAttr.x2 (String.fromFloat x2)
                                , SvgAttr.y2 (String.fromFloat y2)
                                , SvgAttr.stroke "#8a4343ff"
                                , SvgAttr.strokeWidth (String.fromFloat (abs edge.weight * 8))
                                ]
                                []
                        _ ->
                            Svg.line [] []
                )
                edges
    in
    Html.div [ HtmlAttr.style "margin-top" "30px" ]
        [ Html.text "Korrelationsgraph:"
        , Svg.svg [ SvgAttr.width "800", SvgAttr.height "300" ]
            (edgeElements ++ List.concat nodeElements)
        ]

-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
